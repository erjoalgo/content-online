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
   #:oauth-make-client-from-file)

  (:import-from
   #:hunchentoot
   #:session-value
   #:redirect)

  (:export #:start))
