(in-package #:youtube-comments)

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

(defvar youtube-base-url "https://www.youtube.com")

(defun channel-url (channel-id)
  (format nil "~A/channel/~A" youtube-base-url channel-id))

(defun video-url (video-id)
  (format nil "~A/watch?v=~A" youtube-base-url video-id))

(defun playlist-url (playlist-id)
  (format nil "~A/playlist?list=~A" youtube-base-url playlist-id))
