(defpackage #:yt-comments/client
  (:use :cl)
  (:import-from #:yt-comments/util
                #:to-camel-case
                #:->
                #:make-from-json-alist
                #:lisp-alist-to-json-map)
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
  )

(defstruct api-login
  key
  token)

(defvar db)

(defun api-req (login resource params-alist)
  (let* ((as :alist)
         (api-key (api-login-key login))
         (api-base-url default-api-base-url)
         (url (concatenate 'string api-base-url resource))
         (orig-params (lisp-alist-to-json-map (cons (cons :key api-key)
                                                    params-alist))))
    (loop
       with page-token = nil
       with total-results = nil
       for page-idx from 1
       as params = (if (null page-token)
                       orig-params
                       (list (cons "pageToken" page-token)))
       as page = (-> (drakma:http-request url :parameters params)
                     (babel:octets-to-string :encoding :utf-8)
                     (jonathan:parse :as as)
                     (make-from-json-alist resp-page))
       when (null page-token) do
            (setf page-token (resp-page-next-page-token page)
                  total-results (resp-page-total-results page))
       do (format t "params: ~A~%" params)
       do (setf db page)
       append (resp-page-items page) into items
       while page-token
       finally (progn
                 (format t "fetched ~A items~%" (length items))
                 (return items)))))

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

;; (def-api-endpoint comments "comments" :defaults (:part "id"))
