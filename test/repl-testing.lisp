(declaim (optimize (debug 3) (speed 0)))
(setf hunchentoot:*catch-errors-p* nil)

'(progn (when *service*
         (stop *service*))
       (let ((secrets-file-path (loop for path in (uiop:directory-files ".")
                                   thereis (and (equal "json" (pathname-type path)) path))))
         (start (make-config :port 4244
                             :oauth-client-secret-json-path
                             secrets-file-path))))
