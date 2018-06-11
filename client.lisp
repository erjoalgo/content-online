(defpackage #:yt-comments/client
  (:use :cl)
  (:import-from #:yt-comments/util
                #:to-camel-case
                #:->
                #:make-from-json-alist
                #:lisp-alist-to-json-map
                #:retry-times)
  (:export #:make-api-login
           #:default-base-url))

(in-package #:yt-comments/client)

(defvar default-api-base-url
  "https://www.googleapis.com/youtube/v3/")

;; (setf api-base-url "http://localhost:1234/")

(defstruct resp-page
  items
  page-info
  total-results
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

(defvar db)

(defun api-req (login resource params-alist)
  (let* ((as :alist)
         (additional-headers (when (api-login-access-token login)
                               (list (cons :authorization
                                           (format nil "Bearer ~A"
                                                   (api-login-access-token login))))))
         (api-base-url default-api-base-url)
         ;; (api-base-url "http://localhost:1234/")
         (url (concatenate 'string api-base-url resource))
         (orig-params (lisp-alist-to-json-map (if (api-login-key login)
                                                (cons (cons :key (api-login-key login))
                                                      params-alist)
                                                params-alist))))
    (loop
       with page-token-param = (cons "pageToken" nil)
       with total-results = nil
       for page-idx from 1
       as params = (if (null (cdr page-token-param))
                       orig-params
                       (cons page-token-param
                             orig-params))
       as page = (retry-times 5 1
                   (-> (drakma:http-request url
                                          :parameters params
                                          :additional-headers additional-headers)
                     (babel:octets-to-string :encoding :utf-8)
                     (jonathan:parse :as as)
                     (make-from-json-alist resp-page)))
       as error = (resp-page-error page)
       when (null error) do
         (setf (cdr page-token-param)
               (resp-page-next-page-token page))
       do (format t "page: ~A/~A params: ~A~%" page-idx total-results params)
       do (setf db page)
       append (resp-page-items page) into items
       while (and (cdr page-token-param) (not error))
       finally (progn
                 (format t "fetched ~A items~%" (length items))
                 (return (values items error))))))

(defmacro def-api-endpoint (resource-as-sym &key defaults (as :alist))
  (declare (ignore as))
  `(defun ,resource-as-sym (login &rest params-flat)
     (let ((params-alist
           (loop for (k v) on params-flat by #'cddr
              collect (cons k v))))
       (api-req login ,(to-camel-case resource-as-sym)
                (append params-alist ,defaults)))))

(def-api-endpoint comment-threads :defaults '((:part . "snippet")))

(def-api-endpoint subscriptions :defaults '((:part . "snippet")))


(defparameter youtube-comments-base-url
  "https://www.googleapis.com/youtube/v3/comments"
  ;; "http://www.googleapis.com/youtube/v3/comments"
  )

(defun delete-comment (api-login comment-id)
  "DELETE https://www.googleapis.com/youtube/v3/comments"
  (format t "delete token: ~A~%" (api-login-access-token api-login))
    (-> (drakma:http-request
         youtube-comments-base-url
         ;; "http://localhost:1234/"
         ;; "http://localhost/"
         :method :delete
         :parameters (list (cons "id" comment-id))
         ;; TODO factor out authenticated drakma request
         ;; TODO refresh token
         :additional-headers (list
                              (cons "authorization"
                                    (format nil "Bearer ~A"
                                            (api-login-access-token api-login)))))
        (babel:octets-to-string :encoding :utf-8)
        (jonathan:parse :as :alist)))
