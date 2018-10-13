(in-package #:yt-comments/oauth)

(defstruct oauth-client
  client-id
  client-secret
  token-uri
  scopes
  auth-uri
  redirect-uris
  )

(defun make-oauth-client-from-file (filename)
  (let ((client (->
                 (read-file filename)
                 (jonathan:parse :as :alist)
                 ;; (assoq "installed")
                 cdar
                 (make-from-json-alist oauth-client))))
    (assert (with-slots (client-id client-secret) client
              (and client-id client-secret)))
    client))

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

(defparameter youtube-scopes
'("https://www.googleapis.com/auth/youtube.force-ssl"))

(defun auth-server-redirect-url (oauth-client redirect-uri)
  "example
https://accounts.google.com/o/oauth2/v2/auth?
 scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive.metadata.readonly&
 access-type=offline&
 include-granted-scopes=true&
 state=state-parameter-passthrough-value&
 redirect-uri=http%3A%2F%2Foauth2.example.com%2Fcallback&
 response-type=code&
 client-id=client-id
"
  (with-slots (scopes client-id auth-uri) oauth-client
    (format nil "~A?~A"
            auth-uri
            ;; "http://localhost:1234/"
            (-> (flat-to-alist-macro
                 "scope" (format nil "~{~A~^ ~}"
                                 youtube-scopes)
                 "access_type" "online"
                 "include_granted_scopes" "false"
                 "redirect_uri" redirect-uri
                 "response_type" "code"
                 "client_id" client-id)
                (drakma::alist-to-url-encoded-string :utf-8 'drakma:url-encode)))))

(defstruct resp-token
  access-token
  refresh-token
  expires-in
  token-type
  error
  error-description
  )

(defun oauth-token-auth-header (resp-token)
  (cons :authorization
        (format nil "~A ~A"
                (resp-token-token-type resp-token)
                (resp-token-access-token resp-token))))

(defun exchange-code-for-token (code oauth-client)
  "code	The authorization code returned from the initial request.
client_id	The client ID obtained from the API Console.
client_secret	The client secret obtained from the API Console.
redirect_uri	One of the redirect URIs listed for your project in the API Console.
grant_type	As defined in the OAuth 2.0 specification, this field must contain a value of authorization_code.
"
  '"
The following snippet shows a sample request:

POST /oauth2/v4/token HTTP/1.1
Host: www.googleapis.com
Content-Type: application/x-www-form-urlencoded

code=4/P7q7W91a-oMsCeLvIaQm6bTrgtp7&
client_id=your_client_id&
client_secret=your_client_secret&
redirect_uri=https://oauth2.example.com/code&
grant_type=authorization_code"


  '"
(response)
{
\"access_token\":\"1/fFAGRNJru1FTz70BzhT3Zg\",
\"expires_in\":3920,
\"token_type\":\"Bearer\",
\"refresh_token\":\"1/xEoDL4iW3cxlI7yDbSRFYNG01kVKM2C-259HOF2aQbI\"
}

"

(with-slots (scopes client-id client-secret token-uri redirect-uris) oauth-client
  (->
   (drakma:http-request
    token-uri
    ;; "http://localhost:1234"
    :method :post
    :parameters (flat-to-alist-macro
                 "code" code
                 "grant_type" "authorization_code"
                 "client_secret" client-secret
                 "redirect_uri" (car redirect-uris)
                 "client_id" client-id))
   (babel:octets-to-string :encoding :utf-8)
   (jonathan:parse :as :alist)
   (make-from-json-alist resp-token))))
