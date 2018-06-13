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
                  (retry-count 500)
                  (retry-delay 2)
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
                     (loop
                          named annoying-NS-TRY-AGAIN-CONDITION-retry
                        for i from 0 do
                          (handler-case
                              (return-from annoying-NS-TRY-AGAIN-CONDITION-retry
                                (drakma:http-request url
                                                     :method method
                                                     :parameters params
                                                     :additional-headers additional-headers))
                            (USOCKET:NS-TRY-AGAIN-CONDITION
                                (ex)
                              (format nil "failed with ~A: ~A retrying ~D... ~%"
                                      'USOCKET:NS-TRY-AGAIN-CONDITION ex i)
                              (sleep 1)))))
                 (if (and auto-refresh-p (= 403 http-code) (not already-refreshed-p))
                     (req t)
                     (values (-> body
                                 (babel:octets-to-string :encoding :utf-8)
                                 (jonathan:parse :as as))
                             http-code)))))
      (if (not depaginate-p)
          (req)
          (loop
             with page-token-param = (cons "pageToken" nil)
             with total-pages = -1
             for page-idx from 1

             as page = nil
             as error = nil
             as err-status-code = nil
             do (multiple-value-bind (body http-code) (req)
                    (setf err-status-code (unless (eq 200 http-code) http-code)
                          ;; this may fail?
                          page (-> body (make-from-json-alist resp-page))
                          error (resp-page-error page)))
             do (format t "page: ~A/~A params: ~A~%" page-idx
                        (ceiling total-pages)
                        params)
             when (and (null error) (null err-status-code)) do
               (progn
                 (setf (cdr page-token-param)
                       (resp-page-next-page-token page))

                 (when (eq -1 total-pages)
                   (with-json-paths (resp-page-page-info page)
                       ((per-page "resultsPerPage")
                        (total "totalResults"))
                     (setf total-pages (ceiling total (if (zerop per-page) -1 1))
                           params (cons page-token-param params)))))


             append (resp-page-items page) into items

             while (and (cdr page-token-param) (not error))

             finally (progn
                       (format t "fetched ~A items~%" (length items))
                       (return (values items err-status-code error))))))))

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
           :method :delete
           :depaginate-p nil))
