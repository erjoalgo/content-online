(in-package #:youtube-comments)

(defparameter js-table-render-script-path
  "/www/js/renderTable.js")

(defun string-truncate (string n)
  (subseq string 0 (min (length string) n)))

(defmacro check-http-ok (api-req-values &key (ok-code 200))
  (with-gensyms (body-sym http-code-sym resp-string-sym)
    `(multiple-value-bind (,body-sym ,http-code-sym)
         ,api-req-values
       (vom:debug ",body-sym: ~A~%" ,body-sym)
       (if (not (eq ,ok-code ,http-code-sym))
           (progn
             (vom:warn "unexpected code: ~A ~A ~A~%"
                       ,body-sym ,http-code-sym ,resp-string-sym)
             (hunchentoot:abort-request-handler
              (format nil "~A ~A" ,http-code-sym ,body-sym)))
           ,body-sym))))

(defun channels-handler (channels &key (max-description-chars 100))
  (setf (hunchentoot:content-type*) "application/json")
  (->
   `(
     ("headers" . ("channel-id" "title" "description" "comments" "count"))
     ("items" .
              ,(or (loop for chan in channels
                      collect
               (with-slots (id title description) chan
                          (params
                           "channel-id" (dom-link (channel-url id) id)
                           "title" title
                           "description" (string-truncate description max-description-chars)
                           "comments" (dom-link (format nil "/channels/~A/comments.html" id)
                                                "comments!")
                           "count" (dom-lazy-elm
                                    (format nil "/channels/~A/comments-count" id)))))
                   *json-empty-list*)))
   cl-json:encode-json-alist-to-string))

(defun videos-handler (videos &key (max-description-chars 100))
  "videos is a video struct list"
  (->
   `(
     ("headers" . ("title" "channel" "published" "description" "rating" "comments" "count"))
     ("items" .
              ,(or
                (loop for video in videos
                   collect
               (with-slots (id title channel-id channel-title published description rating)
                   video
                       (params
                        "title" (dom-link (video-url id) title)
                        "channel" (dom-link (channel-url channel-id) channel-title)
                        "published" published
                        "description" (when description
                         (string-truncate description max-description-chars))
                        "rating" rating
                        "comments" (dom-link (format nil "/videos/~A/comments.html" id)
                                             "comments")
                        "count"
                       (when id
                          (dom-lazy-elm (format nil "/videos/~A/comments-count" id))))))
                *json-empty-list*)))
   cl-json:encode-json-alist-to-string))

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
  `(let ((count-resp (check-http-ok ,api-req-values)))
     (vom:debug "count-resp ~A~%" count-resp)
     (-> (or
          (-json-get-nested count-resp "pageInfo.totalResults")
          (-json-get-nested count-resp "totalResults"))
         write-to-string)))

(defun session-channel-title ()
  (or
   (session-value 'channel-title)
   (let ((title (->
                 (channels-get
                  (params
                  :part "snippet"
                  :mine "true"))
                 car
                 (-json-get-nested "snippet.title"))))
     (assert title)
     (setf
      (session-value 'channel-title) title))))

(defun list-comments-handler (comments &key no-author-filter)
  (->
   `(("headers" . ("id" "author" "video or channel id" "reply-count" "text" "delete"))
     ("items" .
              ,(or
                (loop with comments =
               (if no-author-filter
                   comments
                   (loop for comment in comments when (equal (session-channel-title)
                                                             (comment-author comment))
                      collect comment))
                    for comment in comments
                  collect
               (with-slots (id author video-id channel-id reply-count text)
                   comment
                      (params
                       "id" id
                       "author" author
                       "video or channel id"
                       (dom-link (if video-id (video-url video-id) (channel-url channel-id))
                                 (or video-id channel-id))
                       "reply-count" reply-count
                       "text" text
                       "delete" (dom-delete-button
                                 (format nil "/comment/~A/delete" id)))))
                *json-empty-list*)))
   cl-json:encode-json-alist-to-string))

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
  (comment-threads-get (params
                       :part "snippet"
                       :search-terms (session-channel-title)
                       :all-threads-related-to-channel-id channel-id)))

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
                (params :part part
                        :id video-ids-commas))

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
