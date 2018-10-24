(in-package #:youtube-comments)

(defun string-truncate (string n)
  (subseq string 0 (min (length string) n)))

(defmacro make-table (headers rows row-idx-sym row-sym row-cols-list)
  `(markup
    (:table
     :border 1
     :cellpadding 4
     (:tr :align "left"
          (loop for header in ,headers collect
               (markup (:td (:b header)))))
     (loop
        for ,row-sym in ,rows
        for ,row-idx-sym from 1
        collect
          (markup
           (:tr :align "left"
                (loop
                   for cell in ,row-cols-list
                   collect (markup (:td (raw cell))))))))))

(defmacro ensure-ok (api-req-values &key (ok-code 200))
  (let ((body-sym (gensym "body"))
        (http-code-sym (gensym "http-code"))
        (resp-string-sym (gensym "resp-string")))
    `(multiple-value-bind (,body-sym ,http-code-sym ,resp-string-sym)
         ,api-req-values
       (if (not (eq ,ok-code ,http-code-sym))
           (progn
             (vom:warn "unexpected code: ~A ~A ~A~%"
                       ,body-sym ,http-code-sym ,resp-string-sym)
             (hunchentoot:abort-request-handler
              (format nil "~A ~A" ,http-code-sym ,resp-string-sym)))
           ,body-sym))))

(defun channels-handler (channels &key (max-description-chars 100))
  (markup-with-lazy-elements
   (make-table '("#" "channel id" "title" "description" "commments" "count")
               channels
               chan-idx chan
               (with-slots (id title description) chan
                 (let* ((url (channel-url id))
                        (comments-link (format nil "/channels/~A/comments" id)))
                   (list (write-to-string chan-idx)
                         (markup (:a :href url id))
                         title
                         (string-truncate description max-description-chars)
                         (markup (:a :href comments-link "comments!"))
                         (js-lazy-element (format nil "/channels/~A/comments-count" id)
                                          loading-gif-img-tag
                                          :skip-self-replace-fun t)))))))

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
