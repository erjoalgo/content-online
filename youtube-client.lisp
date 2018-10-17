(in-package #:yt-comments/server)

(defapi "https://www.googleapis.com/youtube/v3/"

    :get-depaginate
  ((comment-threads :default-params '((:part . "snippet")
                                      (:max-results . "100")))
   (subscriptions :default-params '((:part . "snippet")
                                    (:max-results . "50")))
   (playlists :default-params '((:max-results . "50")))
   (playlist-items :default-params '((:max-results . "50")))
   (channels :default-params '((:max-results . "50")))
   (videos :default-params '((:max-results . "50")))
   (yt-search :resource-path "search")
   (activities :default-params '((:max-results . "50"))))

  :delete
  ())

(defun delete-comment (api-login comment-id)
  "DELETE https://www.googleapis.com/youtube/v3/comments"
  (api-req api-login "comments"
           `(("id" . ,comment-id))
           :method :delete
           :depaginate-p nil))
