(declaim (optimize (debug 3) (speed 0)))
(setf hunchentoot:*catch-errors-p* nil)


'(setf (hunchentoot::session-db (service-acceptor *service*)) nil); delete all sessions

(defun unintern-shadowing-symbols (&optional package)
  (setf package (or package *package*))
  (let ((syms (package-shadowing-symbols package)))
    (dolist (sym syms) (unintern sym package))
    (format t "uninterned ~D shadowing symbols ~%"
            (length syms))))

'(progn
  (load "yt-comments.asd")
  (ql:quickload "yt-comments")
  (in-package "YT-COMMENTS/SERVER")
  (funcall (find-symbol
            "START" "YT-COMMENTS/SERVER")
           :port 4244))
