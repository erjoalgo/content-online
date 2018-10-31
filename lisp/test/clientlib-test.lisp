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
     do
       (let ((erjoalgo-webutil:*google-login*
              (erjoalgo-webutil:make-api-login
               :client
               (erjoalgo-webutil::make-oauth-client
                :key "fakeGtD1o8DWt1biIwfDxkM6cLX1HU4WAdAGWPd"))))
         (multiple-value-bind (body status err)
             (funcall fn)
           (vom:debug "body ~A~%" body)
           (vom:debug "err ~A~%" err)
           (is (eql 400 status))))))

(run-package-tests :interactive t)
