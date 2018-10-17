(in-package #:yt-comments/server)

(defvar youtube-api-base-url
  "https://www.googleapis.com/youtube/v3/")

(defapi youtube-api-base-url
    :get-depaginate
  ((comment-threads-get "commentThreads" :default-params '((:part . "snippet")
                                      (:max-results . "100")))
   (subscriptions-get "subscriptions" :default-params '((:part . "snippet")
                                    (:max-results . "50")))
   (playlists-get "playlists" :default-params '((:max-results . "50")))
   (playlist-items-get "items" :default-params '((:max-results . "50")))
   (channels-get "channels" :default-params '((:max-results . "50")))
   (videos-get "videos" :default-params '((:max-results . "50")))
   (search-get "search")
   (activities-get "activities" :default-params '((:max-results . "50"))))

  :delete
  ())

(defun youtube-api-req (&rest rest)
  (let ((erjoalgo-webutil/google:*api-base-url*
         youtube-api-base-url))
    (apply 'api-req rest)))

(defun delete-comment (api-login comment-id)
  "DELETE https://www.googleapis.com/youtube/v3/comments"
  (youtube-api-req api-login "comments"
                   `(("id" . ,comment-id))
                   :method :delete))
