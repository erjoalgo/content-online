(defpackage #:yt-comments/client
  (:use :cl)
  (:import-from #:yt-comments/util
                #:to-camel-case
                #:->
                #:make-from-json-alist
                #:lisp-alist-to-json-map
                #:retry-times
                #:with-json-paths)
  (:export #:make-api-login
           #:default-base-url))

(in-package #:yt-comments/client)

(defvar default-api-base-url
  "https://www.googleapis.com/youtube/v3/")

;; (setf api-base-url "http://localhost:1234/")

(defstruct resp-page
  items
  page-info
  next-page-token
  etag
  kind
  error
  )

(defstruct api-login
  key
  access-token
  refresh-token
  )

(defun api-req (login resource params-alist
                &key (method :get)
                  (depaginate-p t)
                  (retry-count 5)
                  (retry-delay 1)
                  (auto-refresh-p t)
                  )
  (let* ((as :alist)
         (api-base-url default-api-base-url)
         (url (concatenate 'string api-base-url resource))
         (params (lisp-alist-to-json-map params-alist))
         additional-headers)

    (assert (= 1 (+
                  (if (api-login-access-token login) 1 0)
                  (if (api-login-key login) 1 0))))

    (if (api-login-key login)
        (push (cons :key (api-login-key login)) params)
        (push (cons :authorization
                    (format nil "Bearer ~A" (api-login-access-token login)))
              additional-headers))

    (labels ((req (&optional already-refreshed-p)
               (multiple-value-bind (body http-code)
                   (retry-times retry-count retry-delay
                     (drakma:http-request url
                                          :method method
                                          :parameters params
                                          :additional-headers additional-headers))
                 (if (and auto-refresh-p (= 403 http-code) (not already-refreshed-p))
                     (req t)
                     (-> body
                         (babel:octets-to-string :encoding :utf-8)
                         (jonathan:parse :as as))))))
      (if (not depaginate-p)
          (req)
          (loop
             with page-token-param = (cons "pageToken" nil)
             with total-pages = nil
             for page-idx from 1

             as page = (-> (req) (make-from-json-alist resp-page))
             as error = (resp-page-error page)

             do (format t "page: ~A/~A params: ~A~%" page-idx total-pages params)

             when (null error) do
               (progn
                 (setf (cdr page-token-param)
                       (resp-page-next-page-token page))

                 (when (null total-pages)
                   (with-json-paths (resp-page-page-info page)
                       ((per-page "resultsPerPage")
                        (total "totalResults"))
                   (setf total-pages (/ total per-page)
                         params (cons page-token-param params)))))


             append (resp-page-items page) into items

             while (and (cdr page-token-param) (not error))

             finally (progn
                       (format t "fetched ~A items~%" (length items))
                       (return (values items error))))))))

(defmacro def-api-endpoint (resource-as-sym &key defaults (as :alist)
                                              fun-sym)
  (declare (ignore as))
  `(defun ,(or fun-sym resource-as-sym) (login &rest params-flat)
     (let ((params-alist
           (loop for (k v) on params-flat by #'cddr
              collect (cons k v))))
       (api-req login ,(to-camel-case resource-as-sym)
                (append params-alist ,defaults)))))

(def-api-endpoint comment-threads :defaults '((:part . "snippet")
                                              (:max-results . "100")))

(def-api-endpoint subscriptions :defaults '((:part . "snippet")
                                            (:max-results . "50")))

(def-api-endpoint playlists :defaults '((:max-results . "50")))

(def-api-endpoint playlist-items :defaults '((:max-results . "50")))

(def-api-endpoint channels :defaults '((:max-results . "50")))

(def-api-endpoint videos :defaults '((:max-results . "50")))

;; due to conflict with search function...
(def-api-endpoint search :fun-sym yt-search)

(def-api-endpoint activities :defaults '((:max-results . "50")))

(defun delete-comment (api-login comment-id)
  "DELETE https://www.googleapis.com/youtube/v3/comments"
  (format t "delete token: ~A~%" (api-login-access-token api-login))
  (api-req api-login "comments"
           `(("id" . ,comment-id))
           :method :delete))
