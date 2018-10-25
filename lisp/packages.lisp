(defpackage #:youtube-comments
  (:use :cl :cl-markup)

  (:import-from
   #:erjoalgo-webutil
   #:with-json-paths
   #:->
   #:-json-get-nested
   #:assoq
   #:defroutes
   #:first-file-with-extension
   #:check-nonnil
   #:json-resp
   #:json-req)

  (:import-from
   #:erjoalgo-webutil/google
   #:defapi
   #:api-req
   #:make-api-login
   #:make-oauth-client-from-file
   #:auth-server-redirect-url
   #:exchange-code-for-token

   #:resp-token-access-token
   #:resp-token-refresh-token)

  (:import-from
   #:hunchentoot
   #:session-value
   #:redirect)

  (:export #:start))
