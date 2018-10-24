(fiasco:define-test-package
    #:youtube-comments/client-test
  (:use #:youtube-comments))

(in-package #:youtube-comments/client-test)

(deftest test-resource-paths-non404 ()
  (loop for fn in
       '(
         ;; TODO export these
         youtube-comments::comment-threads-get
         youtube-comments::subscriptions-get
         youtube-comments::playlists-get
         youtube-comments::playlist-items-get
         youtube-comments::channels-get
         youtube-comments::videos-get
         youtube-comments::search-get
         youtube-comments::activities-get)
     with fake-login = (erjoalgo-webutil/google:make-api-login
                        ;; fake key
                        :key "BQwjGtD1o8DWt1biIwfDxkM6cLX1HU4WAdAGWPd")
     do
       (multiple-value-bind (json status raw)
           (funcall fn fake-login)
         (declare (ignore json raw))
         (is (eql 400 status)))))

(run-package-tests :interactive t)
