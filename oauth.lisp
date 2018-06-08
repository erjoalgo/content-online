(defpackage #:yt-comments/oauth
  (:use :cl)
  (:import-from #:yt-comments/util
                #:flat-to-alist-macro
                #:read-file
                #:make-from-json-alist
                #:->)
  (:export #:make-oauth-client-from-file))

(in-package #:yt-comments/oauth)

(defstruct oauth-client
  client-id
  client-secret
  token-uri
  scopes
  auth-uri
  )

(defmacro assoq (alist item)
  `(cdr (assoc ,item ,alist :test 'equal)))

(defun make-oauth-client-from-file (filename)
  (->
   (read-file filename)
   (jonathan:parse :as :alist)
   (assoq "installed")
   (make-from-json-alist oauth-client)))

(defun fetch-token (oauth-client redirect-uri)
  (with-slots (client-id client-secret token-uri) oauth-client
      (-> (drakma:http-request token-uri :parameters
                           (flat-to-alist-macro
                            "client_id" client-id
                            "client_secret" client-secret
                            "redirect_uri" redirect-uri
                            "grant_type" "authorization_code"
                            ))
          (babel:octets-to-string :encoding :utf-8)
          (jonathan:parse :as :alist))
    ))

(defvar resp)

'(progn
  (let ((client (make-oauth-client-from-file "py/client_secrets.json")))
    (setf resp (fetch-token client "localhost:4242"))))

"POST /oauth2/v4/token HTTP/1.1
Host: www.googleapis.com
Content-Type: application/x-www-form-urlencoded

code=4/P7q7W91a-oMsCeLvIaQm6bTrgtp7&
client_id=your_client_id&
client_secret=your_client_secret&
redirect_uri=https://oauth2.example.com/code&
grant_type=authorization_code
"


(defvar auth-server-redirect-base-url
  "https://accounts.google.com/o/oauth2/v2/auth")

(defun auth-server-redirect-url (oauth-client redirect-uri)
  "example
https://accounts.google.com/o/oauth2/v2/auth?
 scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive.metadata.readonly&
 access_type=offline&
 include_granted_scopes=true&
 state=state_parameter_passthrough_value&
 redirect_uri=http%3A%2F%2Foauth2.example.com%2Fcallback&
 response_type=code&
 client_id=client_id
"
  (with-slots (scopes client-id auth-uri) oauth-client
    (format nil "~A?~A"
            auth-uri
            (-> (flat-to-alist-macro
                 "scope" scopes
                 "access_type" "online"
                 "include_granted_scopes" "true"
                 "redirect_uri" redirect-uri
                 "response_type" "code"
                 "client_id" client-id)
                (drakma::alist-to-url-encoded-string :utf-8 'drakma:url-encode)))))

(defun oauth-exchange-code-for-token (code)
  (declare (ignore code)))
