(in-package #:yt-comments/server)

;; (setf hunchentoot:*dispatch-table* nil)
(defvar *service* nil "the current service")

(defun first-file-with-extension (directory ext)
  ;; with directory-wild = (merge-pathnames (make-pathname :name :WILD) directory)
  (loop for path in (uiop:directory-files directory)
     thereis (and (equal ext (pathname-type path)) path)))

(defparameter secrets-directory
  #P"./secrets/")

(defun ensure-non-nil (form)
  (let ((val form))
    (assert val)
    val))

(defstruct config
  (port 4244)
  (oauth-client-secret-json-path
   (ensure-non-nil
    (first-file-with-extension secrets-directory "json")))

  ;; a cons cell: (SSL-CERTIFICATE-FILE . SSL-PRIVATEKEY-FILE)
  (ssl-cert (cons
             (ensure-non-nil (first-file-with-extension secrets-directory "cert"))
             (ensure-non-nil (first-file-with-extension secrets-directory "key")))))

(defstruct service
  acceptor
  config
  oauth-client
  protocol)

(defun start (&rest make-config-args)
  (start-with-config (apply 'make-config make-config-args)))

(defun start-with-config (config)
  (when *service* (stop *service*))
  (with-slots (port oauth-client-secret-json-path ssl-cert) config
    (let ((acceptor-args (list :port port
                               :document-root (truename "./www")))
          acceptor-class
          protocol)

      (if ssl-cert
          (setf acceptor-class 'hunchentoot:easy-ssl-acceptor
                protocol "https"
                acceptor-args (append acceptor-args
                                      (destructuring-bind (cert . key) ssl-cert
                                        (list :SSL-CERTIFICATE-FILE cert
                                              :SSL-PRIVATEKEY-FILE key))))
          (setf acceptor-class 'hunchentoot:easy-acceptor
                protocol "http"))

      (format t "making ~A service  ~A on port ~A~%" protocol acceptor-class port)

      (setf *service*
            (make-service
             :acceptor (apply 'make-instance acceptor-class acceptor-args)
             :protocol protocol
             :config config
             :oauth-client (make-oauth-client-from-file
                            (config-oauth-client-secret-json-path config)))))

    (hunchentoot:start (service-acceptor *service*))
    *service*))

(defun stop (&optional service)
  (setf service (or service *service*))
  (when service
    (format t "stopping...")
    (let* ((acceptor (slot-value service 'acceptor)))
      (when (and acceptor (hunchentoot:started-p acceptor))
        (hunchentoot:stop acceptor)))))

(defvar oauth-authorize-uri-path "/oauth/authorize")

(defun oauth-authorize-uri ()
  (format nil "~A://~A~A"
          ;; TODO get request protocol
          ;; https://stackoverflow.com/questions/40693291/
          ;; (hunchentoot:server-protocol*)
          (service-protocol *service*)
          (hunchentoot:host) oauth-authorize-uri-path))

(defun oauth-redirect-maybe ()
  "do an oauth redirect if session's api-login is nil"
  (unless hunchentoot:*session*
    (hunchentoot:start-session))
  (unless (session-value 'api-login)
    (setf (session-value 'original-url) (hunchentoot:request-uri*))
    (let* ((local-auth-url (oauth-authorize-uri))
           (oauth-client (service-oauth-client *service*))
           (remote-auth-url (auth-server-redirect-url oauth-client local-auth-url)))
      (redirect remote-auth-url))))

(defmacro define-regexp-route (name (url-regexp &rest capture-names) docstring &body body)
  "a macro to define a handler `name' matching requests for `url-regexp'.
An optional list `capture-names' can be provided to capture path variables.
The capturing behavior is based on wrapping `ppcre:register-groups-bind'
"
  `(progn
     (defun ,name ()
       ,docstring
       (format t "protocol is ~A~%" (hunchentoot:server-protocol*))
       (ppcre:register-groups-bind ,capture-names
           (,url-regexp (hunchentoot:script-name*))
         (oauth-redirect-maybe)
         (assert (session-value 'api-login))
         (progn ,@body)))
     (push (hunchentoot:create-regex-dispatcher ,url-regexp ',name)
           hunchentoot:*dispatch-table*)))

;; don't use define-regexp-route because:
;;  - need to exempt from define-regexp-route's authorize redirect to prevent 301 loop
;;  - need to capture query param, which is easier in hunchentoot:define-easy-handler
(hunchentoot:define-easy-handler (oauth-authorize-handler :uri oauth-authorize-uri-path)
    (code)
  ;; (assert (session-value 'original-url))
  (let ((original-url
         (if (not hunchentoot:*session*)
             (progn (hunchentoot:start-session)
                    "/")
             (progn
               ;; (assert (session-value 'original-url))
               (or (session-value 'original-url) "/"))))
        (resp-token (exchange-code-for-token code (service-oauth-client *service*))))
    (if (resp-token-access-token resp-token)
        (progn
          (setf (session-value 'api-login)
                (make-api-login
                 :key nil
                 :token resp-token))
          (redirect original-url))
        (progn (setf (hunchentoot:return-code*)
                     hunchentoot:+http-authorization-required+)
               (format nil "token request rejected: ~A~%" resp-token)))))

(defvar db);;debugging

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

(defparameter home-urls
  '("/subscriptions"
    "/playlists"
    "/feed-history/form.html"
    "/rated-videos"
    ))

(define-regexp-route root-handler ("^/$")
    "root handler"
  (markup (:ul (loop for url in home-urls
                  collect (markup (:li (:a :href url url)))))))


(defstruct channel
  id
  title
  description)

(defun string-truncate (string n)
  (subseq string 0 (min (length string) n)))

(defvar loading-gif-img-tag
  (markup (:img :src (format nil "/loading-small.gif"))))

(defvar js-lazy-load-self-replace-fmt-def-element
  (markup (:script :type "text/javascript"
                   (raw js-lazy-load-self-replace-fmt-def))))

(defmacro markup-with-lazy-elements (form)
  `(markup (:div
            (raw js-lazy-load-self-replace-fmt-def-element)
            (raw ,form))))

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

(defmacro ensure-ok (api-req-values &key (ok-code 200))
  (let ((body-sym (gensym "body"))
        (http-code-sym (gensym "http-code"))
        (resp-string-sym (gensym "resp-string")))
    `(multiple-value-bind (,body-sym ,http-code-sym ,resp-string-sym)
         ,api-req-values
       (if (not (eq ,ok-code ,http-code-sym))
           (progn
             (format t "unexpected error code: ~A ~A ~A~%"
                     ,body-sym ,http-code-sym ,resp-string-sym)
             (hunchentoot:abort-request-handler
              (format nil "~A ~A" ,http-code-sym ,resp-string-sym)))
           ,body-sym))))

(define-regexp-route subscriptions-handler ("^/subscriptions/?$")
    "list user's subscription channels"
  (channels-handler
   (loop for sub in (ensure-ok
                     (subscriptions-get (session-value 'api-login)
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

(define-regexp-route playlists-handler ("^/playlists/?$")
    "list user's playlists"
  (markup-with-lazy-elements
   (make-table '("#" "title"  "date published" "videos")
               (ensure-ok (playlists-get (session-value 'api-login)
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


(defstruct video
  id
  title
  channel-id
  channel-title
  published
  description
  rating
  )

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
     :description description
     )))

(define-regexp-route playlist-videos-handler ("^/playlists/([^/]+)/videos/?$" playlist-id)
    "list user's playlist videos"
  (videos-handler
   (loop for video-alist in (ensure-ok
                             (playlist-items-get (session-value 'api-login)
                                             :playlist-id playlist-id
                                             :mine "true"
                                             :part "snippet"))
      as video = (make-video-from-alist video-alist)
      do (setf (video-id video)
               (-json-get-nested video-alist "snippet.resourceId.videoId"))
      collect video)))

(defmacro results-count-handler (api-req-values)
  `(->
    ,api-req-values
    ensure-ok
    (-json-get-nested "pageInfo.totalResults")
    write-to-string))

(define-regexp-route list-video-comment-counts-handler
    ("^/videos/([^/]*)/comments-count$" video-id)
    "list number of matching comments for the current user on the given video"
  (assert (session-channel-title))
  (results-count-handler
   (youtube-api-req
    (session-value 'api-login)
    "commentThreads"
    `(("part" . "id")
      ("searchTerms" . ,(session-channel-title))
      ("videoId" . ,video-id)
      ("maxResults" . "50")))))

(define-regexp-route list-channel-comment-counts-handler
    ("^/channels/([^/]*)/comments-count$" channel-id)
    "list number of matching comments for the current user on the given video"
  (assert (session-channel-title))
  (results-count-handler
   (youtube-api-req
    (session-value 'api-login)
    "commentThreads"
    `(("part" . "id")
      ("searchTerms" . ,(session-channel-title))
      ("allThreadsRelatedToChannelId" . ,channel-id)
      ("maxResults" . "50")))))

(defun session-channel-title ()
  (or
   (session-value 'channel-title)
   (let ((title (->
                 (channels-get
                  (session-value 'api-login)
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
  (comment-threads-get (session-value 'api-login)
                   :part "snippet"
                   :search-terms (session-channel-title)
                   :all-threads-related-to-channel-id channel-id))

(define-regexp-route list-channel-comment-threads-handler
    ("^/channels/([^/]*)/comments$" sub-channel-id)
    "list comments for the current user on the given channel"
  (assert (session-channel-title))
  (list-comment-threads-handler (channel-comment-threads sub-channel-id)))

(define-regexp-route list-video-comment-threads-handler
    ("^/videos/([^/]*)/comments$" video-id)
    "list comments for the current user on the given video"
  (assert (session-channel-title))
  (list-comment-threads-handler
   (comment-threads-get (session-value 'api-login)
                    :part "snippet"
                    :search-terms (session-channel-title)
                    :video-id video-id)))

(define-regexp-route delete-comments-handler
    ("/comment/([^/]+)/delete" comment-id)
    "delete a given comment"
  (format t "deleting comment ~A~%" comment-id)
  (multiple-value-bind (resp-alist http-code)
      (delete-comment (session-value 'api-login) comment-id)
    (unless (= 204 http-code)
      (format nil "non-204 delete resp: ~A~%" resp-alist))
    (markup (:font :color (if (= 204 http-code) "green" "red")
                   (:b (write-to-string http-code))))))

(defmacro uniquify (list elt-sym elt-key-form &key (test ''equal))
  (let ((table-sym (gensym "table")))
    `(loop with ,table-sym = (make-hash-table :test ,test)
        for ,elt-sym in ,list
        do (setf (gethash ,elt-key-form ,table-sym) ,elt-sym)
        finally (return (loop for ,elt-sym being the hash-values of ,table-sym
                           collect ,elt-sym)))))

(fiasco:deftest test-uniquify nil
  (fiasco:is (eq 3 (length (uniquify '((1 . 1) (1 . 2) (2 . 3)) elt (car elt)))))
  (fiasco:is (eq 3 (length (uniquify '((1 . 1) (1 . 2) (2 . 3)) elt (cdr elt))))))


(defun parse-unique-video-ids (text)
  (uniquify
   (cl-ppcre:all-matches-as-strings
    "(?<=/watch[?]v=)([^\"&\]*)" text)
   string string))

(defparameter inner-html-form-id "video-ids")

(defun gen-unique-id ()
  (random (ash 1 30)))

(define-regexp-route feed-history-input-video-ids-handler
    ("/feed-history/dom-html")
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
(define-regexp-route feed-history-get-results-handler
    ("/feed-history/results/([0-9]+)$" (#'parse-integer unique-id))
    "parse video ids from the https://www.youtube.com/feed/history/comment_history inner html"
  (let ((req (assoq (session-value 'feed-req-ids) unique-id)))
    (if (not req)
        (progn (format t "req ~A~%" req)
               (format nil "request id ~A not found" unique-id))
        (destructuring-bind (aggregation . video-ids) req
          (feed-aggregation-handler aggregation video-ids)))))

(define-regexp-route liked-videos-handler ("^/rated-videos/?$")
    "list user's liked videos"
  (videos-handler
   (loop for rating in '("like" "dislike") append
        (loop for video-alist in (videos-get
                                  (session-value 'api-login)
                                  :my-rating rating
                                  :part "snippet")
           as video = (make-video-from-alist video-alist)
           do (setf (video-rating video) rating)
           collect video))))

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
                (session-value 'api-login)
                :part part
                :id video-ids-commas)

             (unless (= 200 http-code)
               (format t "bad http code ~A while fetching these videos:
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

(define-regexp-route lazy-test-handler ("^/lazy-call$")
    "list user's liked videos"
  (let ((secs (+ 2 (random 2))))
    (sleep secs)
    (format nil "slept for ~A, verb was ~A" secs (hunchentoot:request-method*))))

(define-regexp-route lazy-test ("^/lazy$")
    "list user's liked videos"
  (js-lazy-element "/lazy-call"
                   loading-gif-img-tag
                   :verb :delete))

(define-regexp-route lazy-button-test ("^/lazy-butt$")
    "list user's liked videos"
  (js-lazy-element "/lazy-call"
                   loading-gif-img-tag
                   :as-button "click me!"
                   :verb :delete))
