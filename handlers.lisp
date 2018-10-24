(in-package #:youtube-comments)

(defstruct video
  id
  title
  channel-id
  channel-title
  published
  description
  rating)

(defun videos-handler (videos &key (max-description-chars 100))
  "videos is a video struct list"
  (markup-with-lazy-elements
   (make-table '("#" "title" "channel" "published" "description" "rating" "commments" "count")
               videos
               idx video
               (with-slots (id title channel-id channel-title published description rating)
                   video
                 (list (write-to-string idx)
                       (markup
                        (:a :href (video-url id) title))
                       (markup
                        (:a :href (channel-url channel-id) channel-title))
                       published
                       (when description
                         (string-truncate description max-description-chars))
                       rating
                       (markup
                        (:a :href (format nil "/videos/~A/comments" id) "comments"))
                       (when id
                         (js-lazy-element (format nil "/videos/~A/comments-count" id)
                                          loading-gif-img-tag
                                          :skip-self-replace-fun t)))))))

(defun make-video-from-alist (video-alist)
  (with-json-paths video-alist
      ((id "id")
       (title "snippet.title")
       (channel-id "snippet.channelId")
       (channel-title "snippet.channelTitle")
       (published "snippet.publishedAt")
       (description "snippet.description"))
    (make-video
     :id id
     :title title
     :channel-id channel-id
     :channel-title channel-title
     :published published
     :description description)))

(defmacro results-count-handler (api-req-values)
  `(->
    ,api-req-values
    ensure-ok
    (-json-get-nested "pageInfo.totalResults")
    write-to-string))

(defun session-channel-title ()
  (or
   (session-value 'channel-title)
   (let ((title (->
                 (channels-get
                  (session-value :login)
                  :part "snippet"
                  :mine "true")
                 car
                 (-json-get-nested "snippet.title"))))
     (assert title)
     (setf
      (session-value 'channel-title) title))))

(defstruct comment
  id
  author
  video-id
  channel-id
  reply-count
  text)

(defun list-comments-handler (comments &key no-author-filter)
  (markup-with-lazy-elements
   (make-table '("#" "id" "author" "video or channel id" "reply count" "text" "delete")
               (if no-author-filter
                   comments
                   (loop for comment in comments when (equal (session-channel-title)
                                                             (comment-author comment))
                      collect comment))
               comment-idx comment
               (with-slots (id author video-id channel-id reply-count text)
                   comment
                 (list (write-to-string comment-idx)
                       id
                       author
                       (markup (:a :href (if video-id (video-url video-id) (channel-url channel-id))
                                   (or video-id channel-id)))
                       (write-to-string reply-count)
                       text
                       (js-lazy-element
                        (format nil "/comment/~A/delete" id)
                        loading-gif-img-tag
                        :as-button "delete!"
                        :verb :delete
                        :skip-self-replace-fun t))))))

(defun make-comment-from-json-alist (comment-thread-alist)
  (with-json-paths comment-thread-alist
      ((comment-author "snippet.topLevelComment.snippet.authorDisplayName")
       (comment-id "id")
       (comment-video-id "snippet.videoId")
       (comment-channel-id "snippet.channelId")
       (comment-reply-count "snippet.totalReplyCount")
       (comment-text "snippet.topLevelComment.snippet.textOriginal"))
    (make-comment
     :author comment-author
     :id comment-id
     :video-id comment-video-id
     :channel-id comment-channel-id
     :reply-count comment-reply-count
     :text comment-text)))

(defun list-comment-threads-handler (comment-threads)
  (list-comments-handler (mapcar 'make-comment-from-json-alist
                                 comment-threads)))

(defun channel-comment-threads (channel-id)
  "channel comments for the current user"
  (comment-threads-get (session-value :login)
                       :part "snippet"
                       :search-terms (session-channel-title)
                       :all-threads-related-to-channel-id channel-id))

(defmacro uniquify (list elt-sym elt-key-form &key (test ''equal))
  (let ((table-sym (gensym "table")))
    `(loop with ,table-sym = (make-hash-table :test ,test)
        for ,elt-sym in ,list
        do (setf (gethash ,elt-key-form ,table-sym) ,elt-sym)
        finally (return (loop for ,elt-sym being the hash-values of ,table-sym
                           collect ,elt-sym)))))


(defun parse-unique-video-ids (text)
  (uniquify
   (cl-ppcre:all-matches-as-strings
    "(?<=/watch[?]v=)([^\"&\]*)" text)
   string string))

(defparameter inner-html-form-id "video-ids")

(defun gen-unique-id ()
  (random (ash 1 30)))

(defun feed-aggregation-handler (aggregation video-ids)
  (cond
    ((not (and aggregation video-ids))
     (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
     (format nil "aggregation and ~A parameters are required" inner-html-form-id))
    ((equal "videos" aggregation)
     (videos-handler (loop for video-id in video-ids collect
                          (make-video
                           :id video-id))))
    ((equal "channels" aggregation)
     (channels-handler (video-ids-to-unique-channel-ids video-ids)))
    ((equal "comments" aggregation)
     (list-comment-threads-handler
      (loop for channel in (video-ids-to-unique-channel-ids video-ids)
         nconc (channel-comment-threads (channel-id channel)))))
    (t (error "unknown aggregation"))))

(defmacro loop-do-chunked (chunk-sym list n &body body)
  (let ((elt-sym (gensym "elt"))
        (i-sym (gensym "i"))
        (n-sym (gensym "n")))
    `(loop
        with ,chunk-sym = nil
        with ,n-sym = ,n
        for ,elt-sym in ,list
        for ,i-sym from 1
        do (push ,elt-sym ,chunk-sym)
        when (zerop (mod ,i-sym ,n-sym)) do
          (progn (progn ,@body)
                 (setf ,chunk-sym nil))
        finally (when ,chunk-sym
                  (progn ,@body)))))

(defun fetch-videos-by-ids (video-ids
                            &key (videos-per-request 50)
                              (part "snippet"))
  "does not preserve order"
  ;; TODO parallelize?
  (let (videos)
    (loop-do-chunked video-ids-chunk
       video-ids
       videos-per-request
         (let ((video-ids-commas (format nil "~{~A~^,~}" video-ids-chunk)))
           (multiple-value-bind (items http-code string)
               (videos-get
                (session-value :login)
                :part part
                :id video-ids-commas)

             (unless (= 200 http-code)
               (vom:warn "bad http code ~A while fetching these videos:
~A.
response: ~A~%" http-code video-ids-commas string))

             (when items
               (loop for video-alist in items do
                    (push (make-video-from-alist video-alist) videos))))))
    videos))

(defun video-ids-to-unique-channel-ids (video-ids)
  (->
   (loop for video in (fetch-videos-by-ids video-ids)
      collect (make-channel
               :id (video-channel-id video)
               :title (video-channel-title video)))
   (uniquify chan (channel-id chan))))

(defroutes dispatchers-auth

(((:get) "^/subscriptions/?$")

    "list user's subscription channels"
  (channels-handler
   (loop for sub in (ensure-ok
                     (subscriptions-get (session-value :login)
                                        ;; :channel-id channel-id
                                        :mine "true"
                                        :part "snippet"))

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
               (ensure-ok (playlists-get (session-value :login)
                                         :mine "true"
                                         :part "snippet"))
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
                             (playlist-items-get (session-value :login)
                                                 :playlist-id playlist-id
                                                 :mine "true"
                                                 :part "snippet"))
      as video = (make-video-from-alist video-alist)
      do (setf (video-id video)
               (-json-get-nested video-alist "snippet.resourceId.videoId"))
      collect video)))

(((:get) "^/videos/([^/]*)/comments-count$" video-id)

    "list number of matching comments for the current user on the given video"
  (assert (session-channel-title))
  (results-count-handler
   (youtube-api-req
    (session-value :login)
    "commentThreads"
    `(("part" . "id")
      ("searchTerms" . ,(session-channel-title))
      ("videoId" . ,video-id)
      ("maxResults" . "50")))))

(((:get) "^/channels/([^/]*)/comments-count$" channel-id)

    "list number of matching comments for the current user on the given video"
  (assert (session-channel-title))
  (results-count-handler
   (youtube-api-req
    (session-value :login)
    "commentThreads"
    `(("part" . "id")

      ("searchTerms" . ,(session-channel-title))
      ("allThreadsRelatedToChannelId" . ,channel-id)
      ("maxResults" . "50")))))

(((:get) "^/channels/([^/]*)/comments$" sub-channel-id)

    "list comments for the current user on the given channel"
  (assert (session-channel-title))
  (list-comment-threads-handler (channel-comment-threads sub-channel-id)))

(((:get) "^/videos/([^/]*)/comments$" video-id)

    "list comments for the current user on the given video"
  (assert (session-channel-title))
  (list-comment-threads-handler
   (comment-threads-get (session-value :login)
                        :part "snippet"
                        :search-terms (session-channel-title)
                        :video-id video-id)))

(((:delete) "/comment/([^/]+)/delete" comment-id)

    "delete a given comment"
  (vom:debug "deleting comment ~A~%" comment-id)
  (multiple-value-bind (resp-alist http-code)
      (delete-comment (session-value :login) comment-id)
    (unless (= 204 http-code)
      (format nil "non-204 delete resp: ~A~%" resp-alist))
    (markup (:font :color (if (= 204 http-code) "green" "red")
                   (:b (write-to-string http-code))))))

(((:post) "/feed-history/dom-html")

    "parse video ids from the https://www.youtube.com/feed/history/comment_history inner html"
  (let ((video-ids (-> (hunchentoot:post-parameters*)
                       (assoq inner-html-form-id)
                       (parse-unique-video-ids)))
        (aggregation (-> (hunchentoot:post-parameters*)
                         (assoq "aggregation")))
        (unique-id (gen-unique-id)))
    (unless (null (session-value 'feed-req-ids))
      (setf (session-value 'feed-req-ids) nil))
    (push (cons unique-id
                (cons aggregation video-ids))
          (session-value 'feed-req-ids))
    (redirect (format nil "/feed-history/results/~A" unique-id))))

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
                                  (session-value :login)
                                  :my-rating rating
                                  :part "snippet")
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
