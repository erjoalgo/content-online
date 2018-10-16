(def-api-endpoint comment-threads :defaults '((:part . "snippet")
                                              (:max-results . "100")))

(def-api-endpoint subscriptions :defaults '((:part . "snippet")
                                            (:max-results . "50")))

(def-api-endpoint playlists :defaults '((:max-results . "50")))

(def-api-endpoint playlist-items :defaults '((:max-results . "50")))

(def-api-endpoint channels :defaults '((:max-results . "50")))

(def-api-endpoint videos :defaults '((:max-results . "50")))

;; due to conflict with search function...
(def-api-endpoint search :fun-sym yt-search)

(def-api-endpoint activities :defaults '((:max-results . "50")))

(defun delete-comment (api-login comment-id)
  "DELETE https://www.googleapis.com/youtube/v3/comments"
  (api-req api-login "comments"
           `(("id" . ,comment-id))
           :method :delete
           :depaginate-p nil))
