(declaim (optimize (debug 3) (speed 0)))
(setf hunchentoot:*catch-errors-p* nil)


'(setf (hunchentoot::session-db (service-acceptor *service*)) nil); delete all sessions

(defun unintern-shadowing-symbols (package)
  (let ((syms (package-shadowing-symbols package)))
    (dolist (sym syms) (unintern sym package))
    (format t "uninterned ~D shadowing symbols ~%"
            (length syms))))

(progn
  (load "yt-comments.asd")
  (ql:quickload "yt-comments")
  (in-package "YT-COMMENTS/SERVER")
  (funcall (find-symbol
            "START" "YT-COMMENTS/SERVER")
           (funcall (find-symbol
                     "MAKE-CONFIG" "YT-COMMENTS/SERVER")
                    :port 4242
                    :oauth-client-secret-json-path
                    (loop for path in (uiop:directory-files ".")
                       thereis (and (equal "json" (pathname-type path)) path)))))
