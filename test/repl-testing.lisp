(declaim (optimize (debug 3) (speed 0)))
(setf hunchentoot:*catch-errors-p* nil)

(defun unintern-shadowing-symbols (package)
  (let ((syms (package-shadowing-symbols package)))
    (dolist (sym syms) (unintern sym package))
    (format t "uninterned ~D shadowing symbols ~%"
            (length syms))))

(swank:set-package "YT-COMMENTS/SERVER")
(swank-repl::in-package "YT-COMMENTS/SERVER")
(progn (when *service*
         (stop *service*))
       (let ((secrets-file-path (loop for path in (uiop:directory-files ".")
                                   thereis (and (equal "json" (pathname-type path)) path))))
         (start (make-config :port 4244
                             :oauth-client-secret-json-path
                             secrets-file-path))))


'(setf (hunchentoot::session-db (service-acceptor *service*)) nil)
