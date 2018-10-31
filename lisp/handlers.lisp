(in-package #:youtube-comments)

(defroutes dispatchers-auth

(((:get) "^/subscriptions/?$")

    "list user's subscription channels"
  (channels-handler
   (loop for sub in (ensure-ok
                     (subscriptions-get (params
                                         :mine "true"
                                         :part "snippet")))

      collect (with-json-paths sub
                  ((chan-id "snippet.resourceId.channelId")
                   (title "snippet.title")
                   (description "snippet.description"))
                (make-channel
                 :id chan-id
                 :title title
                 :description description)))))

(((:get) "^/playlists/?$")

    "list user's playlists"
  (markup-with-lazy-elements
   (make-table '("#" "title"  "date published" "videos")
               (ensure-ok (playlists-get (params
                                         :mine "true"
                                         :part "snippet")))
               idx playlist
               (with-json-paths playlist
                   ((id "id")
                    (title "snippet.title")
                    (published "snippet.publishedAt"))
                 (list (write-to-string idx)
                       (markup
                        (:a :href (playlist-url id) title))
                       published
                       (markup
                        (:a :href (format nil "/playlists/~A/videos"
                                          id) "videos")))))))

(((:get) "^/playlists/([^/]+)/videos/?$" playlist-id)
    "list user's playlist videos"
  (videos-handler
   (loop for video-alist in (ensure-ok
                             (playlist-items-get (params
                                                 :playlist-id playlist-id
                                                 :mine "true"
                                                 :part "snippet")))
      as video = (make-video-from-alist video-alist)
      do (setf (video-id video)
               (-json-get-nested video-alist "snippet.resourceId.videoId"))
      collect video)))

(((:get) "^/videos/([^/]*)/comments-count$" video-id)

    "list number of matching comments for the current user on the given video"
  (assert (session-channel-title))
  (results-count-handler
   (comment-threads-get
    `(("part" . "id")
      ("searchTerms" . ,(session-channel-title))
      ("videoId" . ,video-id)
      ("maxResults" . "50"))
    :depaginator nil)))

(((:get) "^/channels/([^/]*)/comments-count$" channel-id)

    "list number of matching comments for the current user on the given video"
  (assert (session-channel-title))
 (results-count-handler
  (comment-threads-get
    `(("part" . "id")
      ("searchTerms" . ,(session-channel-title))
      ("allThreadsRelatedToChannelId" . ,channel-id)
      ("maxResults" . "50"))
    :depaginator nil)))

(((:get) "^/channels/([^/]*)/comments$" sub-channel-id)

    "list comments for the current user on the given channel"
  (assert (session-channel-title))
  (list-comment-threads-handler (channel-comment-threads sub-channel-id)))

(((:get) "^/videos/([^/]*)/comments$" video-id)

    "list comments for the current user on the given video"
  (assert (session-channel-title))
  (list-comment-threads-handler
   (comment-threads-get (params
                        :part "snippet"
                        :search-terms (session-channel-title)
                        :video-id video-id))))

(((:delete) "/comment/([^/]+)/delete" comment-id)

    "delete a given comment"
  (vom:debug "deleting comment ~A~%" comment-id)
  (multiple-value-bind (resp-alist http-code)
      (comment-delete comment-id)
    (unless (= 204 http-code)
      (format nil "non-204 delete resp: ~A~%" resp-alist))
    (markup (:font :color (if (= 204 http-code) "green" "red")
                   (:b (write-to-string http-code))))))

(((:post) "/feed-history/video-ids")
 "parse video ids from the inner html of https://www.youtube.com/feed/history/comment_history"
 (let* ((json (json-req))
        (video-ids (assoq json :video-ids))
        (aggregation (assoq json :aggregation))
        (unique-id (gen-unique-id)))
    (unless (null (session-value 'feed-req-ids))
      (setf (session-value 'feed-req-ids) nil))
    (push (cons unique-id
                (cons aggregation video-ids))
          (session-value 'feed-req-ids))
   (json-resp
    `(("location" .
                  ,(format nil "/feed-history/results/~A" unique-id))))))

(((:get) "/feed-history/results/([0-9]+)$" (#'parse-integer unique-id))

    "parse video ids from the https://www.youtube.com/feed/history/comment_history inner html"
  (let ((req (assoq (session-value 'feed-req-ids) unique-id)))
    (if (not req)
        (progn (vom:warn "req ~A~%" req)
               (format nil "request id ~A not found" unique-id))
        (destructuring-bind (aggregation . video-ids) req
          (feed-aggregation-handler aggregation video-ids)))))

(((:get) "^/rated-videos/?$")

    "list user's liked videos"
  (videos-handler
   (loop for rating in '("like" "dislike") append
        (loop for video-alist in (videos-get
                                  (params
                                  :my-rating rating
                                  :part "snippet"))
           as video = (make-video-from-alist video-alist)
           do (setf (video-rating video) rating)
           collect video))))

(((:get) "^/lazy-call$")

    "list user's liked videos"
  (let ((secs (+ 2 (random 2))))
    (sleep secs)
    (format nil "slept for ~A, verb was ~A" secs (hunchentoot:request-method*))))

(((:get) "^/lazy$")

    "list user's liked videos"
  (js-lazy-element "/lazy-call"
                   loading-gif-img-tag
                   :verb :delete))

(((:get) "^/lazy-butt$")
    "list user's liked videos"
  (js-lazy-element "/lazy-call"
                   loading-gif-img-tag
                   :as-button "click me!"
                   :verb :delete)))
