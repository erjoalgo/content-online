(declaim (optimize (debug 3) (speed 0)))
(setf hunchentoot:*catch-errors-p* nil)

(progn (when *service*
         (stop *service*))
       (start (make-config :port 4244
                           :oauth-client-secret-json-path
                           "client_secret_REMOVED.apps.googleusercontent.com.json")))
