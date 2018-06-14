(defpackage #:yt-comments/client
  (:use :cl)
  (:import-from #:yt-comments/util
                #:to-camel-case
                #:->
                #:make-from-json-alist
                #:lisp-alist-to-json-map
                #:retry-times
                #:with-json-paths)
  (:import-from #:yt-comments/oauth
                #:oauth-token-auth-header)
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
  token
  )

(defmacro log-values (form)
  (let ((vals-sym (gensym "vals")))
    `(let ((,vals-sym (multiple-value-call #'list
                        ,form)))
       (format t "values: ~A~%" ,vals-sym)
       (values-list ,vals-sym))))

(defun api-req (login resource params-alist
                &key (method :get)
                  (depaginate-p t)
                  (retry-count 500)
                  (retry-delay 2)
                  (auto-refresh-p t)
                  )
  "retuns values: json-as-alist http-resp-code resp-string"
  (let* ((as :alist)
         (api-base-url default-api-base-url)
         (url (concatenate 'string api-base-url resource))
         (params (lisp-alist-to-json-map params-alist))
         additional-headers)

    (assert (= 1 (+
                  (if (api-login-token login) 1 0)
                  (if (api-login-key login) 1 0))))

    (if (api-login-key login)
        (push (cons :key (api-login-key login)) params)
        (push (oauth-token-auth-header (api-login-token login))
              additional-headers))

    (format t "auth: ~A~%" (if (api-login-key login)
                                 (api-login-key login)
                                 (oauth-token-auth-header (api-login-token login))))

    (labels ((req (&optional already-refreshed-p)
               (multiple-value-bind (octets http-code)
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
                     (progn (format t "got 403. trying to refresh..." )
                            (req t))
                     (let* ((string (babel:octets-to-string octets :encoding :utf-8))
                            (json (jonathan:parse string :as as)))
                       (values json http-code string))))))
      (if (not depaginate-p)
          (req)
          (loop
             with page-token-param = (cons "pageToken" nil)
             with total-pages = -1
             for page-idx from 1

             as resp-string = nil
             as page = nil
             as error = nil
             as status-code = nil

             do (multiple-value-bind (body http-code string) (req)
                  (format nil "body is ~A ~A" http-code body)
                  (setf resp-string string
                        status-code http-code
                        ;; this may fail?
                        page (-> body (make-from-json-alist resp-page))
                        error (resp-page-error page)))
             do (format t "body is ~A ~A~%" resp-string status-code)
             do (format t "page: ~A/~A params: ~A~%" page-idx
                        (ceiling total-pages)
                        params)
             when (and (null error)
                        ;2xx
                       (eq 2 (floor status-code 100)))
             do
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
                       (return (values items status-code resp-string error))))))))

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
  (api-req api-login "comments"
           `(("id" . ,comment-id))
           :method :delete
           :depaginate-p nil))
