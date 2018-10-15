(declaim (optimize (debug 3) (speed 0)))
(setf hunchentoot:*catch-errors-p* nil)


'(setf (hunchentoot::session-db (service-acceptor *service*)) nil); delete all sessions

'(progn
  (load "yt-comments.asd")
  (ql:quickload "yt-comments")
  (in-package "YT-COMMENTS/SERVER")
  (funcall (find-symbol
            "START" "YT-COMMENTS/SERVER")
           :port 4244))
