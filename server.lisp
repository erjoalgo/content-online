(defpackage #:yt-comments/server
  (:use :cl :cl-markup)
  (:import-from #:yt-comments/util
                #:with-json-paths
                #:->
                #:get-nested-macro
                )
  (:import-from #:yt-comments/client
                #:make-api-login
                #:subscriptions
                #:comment-threads
                #:channel-url
                #:video-url
                #:delete-comment
                #:playlists
                #:playlist-items
                )
  (:import-from #:yt-comments/oauth
                #:make-oauth-client-from-file
                #:auth-server-redirect-url
                #:exchange-code-for-token
                #:resp-token-access-token
                #:resp-token-refresh-token
                )
  (:import-from #:hunchentoot
                #:session-value
                #:redirect
                ))


(in-package #:yt-comments/server)

(defvar *service* nil "the current service")

(defstruct config
  port
  oauth-client-secret-json-path
  api-login
  )

(defstruct service
  acceptor
  config
  oauth-client
  )

(defun start (config)
  (setf *service*
        (make-service
         :acceptor (make-instance 'hunchentoot:easy-acceptor
                                  :port (config-port config)
                                  :document-root (truename "./www"))
         :config config
         :oauth-client (make-oauth-client-from-file
                        (config-oauth-client-secret-json-path config))
         ))
  (hunchentoot:start (service-acceptor *service*))
  *service*)

(defun stop (service)
  (setf service (or service *service*))
  (when service
    (let* ((acceptor (slot-value service 'acceptor)))
      (when (and acceptor (hunchentoot:started-p acceptor))
        (hunchentoot:stop acceptor)))))

(defvar oauth-authorize-uri-path "/oauth/authorize")

(defun oauth-authorize-uri ()
  (format nil "http://~A~A"
          ;; TODO get request protocol
          ;; https://stackoverflow.com/questions/40693291/
          ;; (hunchentoot:server-protocol*)
          (hunchentoot:host) oauth-authorize-uri-path))

(defun oauth-redirect (original-url)
  (setf (session-value 'original-url) original-url)
  (let* ((local-auth-url (oauth-authorize-uri))
         (oauth-client (service-oauth-client *service*))
         (remote-auth-url (auth-server-redirect-url oauth-client local-auth-url)))
    (redirect remote-auth-url)))

(defmacro define-regexp-route (name (url-regexp &rest capture-names) docstring &body body)
  "a macro to define a handler `name' matching requests for `url-regexp'.
An optional list `capture-names' can be provided to capture path variables.
The capturing behavior is based on wrapping `ppcre:register-groups-bind'
"
  `(progn
     (defun ,name ()
       ,docstring
       (ppcre:register-groups-bind ,capture-names
           (,url-regexp (hunchentoot:script-name*))
         (if (not (session-value 'api-login))
             (oauth-redirect (hunchentoot:request-uri*))
             (progn ,@body))))
     (push (hunchentoot:create-regex-dispatcher ,url-regexp ',name)
           hunchentoot:*dispatch-table*)))

(hunchentoot:define-easy-handler (oauth-authorize-handler :uri oauth-authorize-uri-path)
    (code)
  ;; (assert (session-value 'original-url))
  (let ((original-url
         (if (not hunchentoot:*session*)
             (progn (hunchentoot:start-session)
                    "/")
             (progn
               (assert (session-value 'original-url))
                    (session-value 'original-url))))
        (resp-token (exchange-code-for-token code (service-oauth-client *service*))))
    (if (resp-token-access-token resp-token)
        (progn
          (setf (session-value 'api-login)
                (make-api-login
                 :key nil
                 :access-token (resp-token-access-token resp-token)
                 :refresh-token (resp-token-refresh-token resp-token)))
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

(define-regexp-route root-handler ("^/$")
    "initiate session and fetch token"
  (unless hunchentoot:*session*
    (format t "starting session...~%" )
    (hunchentoot:start-session))
  ;; TODO ask for username?
  (let* ((token (session-value 'api-login))
         (remote-redirect-url (format nil "~A~A"
                                      (hunchentoot:host)
                                      oauth-authorize-uri-path))
         (oauth-client (service-oauth-client *service*)))
    (if token
        (home-handler)
        (hunchentoot:redirect
         (auth-server-redirect-url oauth-client remote-redirect-url)))))

(defparameter home-urls
  '("/subscriptions"
    "/playlists"))

(defun home-handler ()
  (markup (:ul (loop for url in home-urls
                  collect (markup (:li (:a :href url url)))))))


(defstruct channel
  id
  title
  description
  )

(define-regexp-route subscriptions-handler ("^/subscriptions/?$")
    "list user's subscription channels"

  ;; (format t "have ~A subs~%" (length subs))
  ;; (setf db subs)
  ;; defmacro (headers rows-form row-idx-sym row-sym row-cols-list-form)
  (let ((channs (make-hash-table :test 'equal)))
    (loop for sub in (subscriptions (session-value 'api-login)
                                    ;; :channel-id channel-id
                                    :mine "true"
                                    :part "snippet")
       do (with-json-paths sub
              ((chan-id "snippet.resourceId.channelId")
               (title "snippet.title")
               (description "snippet.description"))
            (unless (gethash chan-id channs)
              (setf (gethash chan-id channs)
                    (make-channel
                     :id chan-id
                     :title title
                     :description description)))))

    (make-table '("#" "channel id" "description" "url" "commments")
                ;; db
                (loop for chan being the hash-values of channs
                   collect chan)
                chan-idx chan
                (let* ((chan-id (channel-id chan))
                       (chan-url (channel-url chan-id))
                       (chan-comments-link (format nil "/channels/~A/comments"
                                                   chan-id)))
                  (list (format nil "~D" chan-idx)
                        chan-id
                        (channel-title chan)
                        (markup (:a :href chan-url chan-url))
                        (markup (:a :href chan-comments-link "comments!")))))))

(define-regexp-route playlists-handler ("^/playlists/?$")
    "list user's playlists"
  (make-table '("#" "id" "title" "published" "commments")
              (playlists (session-value 'api-login)
                         :mine "true"
                         :part "snippet")
              idx playlist
              (with-json-paths playlist
                  ((id "id")
                   (title "snippet.title")
                   (published "snippet.publishedAt"))
                (list (format nil "~D" idx)
                      id
                      title
                      published
                      (markup
                       (:a :href (format nil "/playlists/~A/videos"
                                         id) "videos"))))))

(define-regexp-route playlist-videos-handler ("^/playlists/([^/]+)/videos/?$" playlist-id)
    "list user's playlist videos"
  (make-table '("#" "id" "title" "channel" "published" "description" "commments")
              (loop for item in (playlist-items (session-value 'api-login)
                                                :playlist-id playlist-id
                                                :mine "true"
                                                :part "snippet")
                   do (format t "item is ~A~%" item)
                 ;; when (equal "youtube#video" (get-nested-macro item "kind"))
                 collect item)
              idx video
              (with-json-paths video
                  ((id "snippet.resourceId.videoId")
                   (title "snippet.title")
                   (channel-id "snippet.channelId")
                   (published "snippet.publishedAt")
                   (description "snippet.description"))
                (list (format nil "~D" idx)
                      (markup
                       (:a :href (video-url id) id))
                      title
                      channel-id
                      published
                      (subseq description 0 (min (length description) 100))
                      (markup
                       (:a :href (format nil "/videos/~A/comments" id) "comments"))))))

(defun session-channel-title ()
  (or
   (session-value 'channel-title)
   (setf
    (session-value 'channel-title)
    (->
     (yt-comments/client::channels
      (session-value 'api-login)
      :part "snippet"
      :mine "true")
     car
     (get-nested-macro "snippet.title")))))

(defstruct comment
  id
  author
  video-id
  channel-id
  reply-count
  text)

(defun list-comments-handler (comments)
  (make-table '("#" "id" "author" "video or channel id" "reply count" "text")
              comments
              comment-idx comment
              (with-slots (id author video-id channel-id reply-count text)
                  comment
                (list (format nil "~D" comment-idx)
                      id
                      author
                      (markup (:a :href (if video-id (video-url video-id) (channel-url channel-id))
                                  (or video-id channel-id)))
                      (format nil "~D" reply-count)
                      text
                      (markup (:a :href (format nil "/comment/~A/delete" id) "delete!"))))))

(define-regexp-route list-channel-comment-threads-handler
    ("^/channels/([^/]*)/comments$" sub-channel-id)
    "list comments for the current user on the given channel"
  (assert (session-channel-title))
  (list-comments-handler
   (loop for comment-thread in (comment-threads (session-value 'api-login)
                                                :part "snippet"
                                                :search-terms (session-channel-title)
                                                :all-threads-related-to-channel-id sub-channel-id
                                                )
      collect
        (with-json-paths comment-thread
            ((comment-author "snippet.topLevelComment.snippet.authorDisplayName")
             (comment-id "id")
             (comment-video-id "snippet.videoId")
             (comment-channel-id "snippet.channelId")
             (comment-reply-count "snippet.totalReplyCount")
             (comment-text "snippet.topLevelComment.snippet.textOriginal")
             )
          (make-comment
           :author comment-author
           :id comment-id
           :video-id comment-video-id
           :channel-id comment-channel-id
           :reply-count comment-reply-count
           :text comment-text)))))

(define-regexp-route delete-comments-handler
    ("/comment/([^/]+)/delete" comment-id)
    "delete a given comment"
  (format t "deleting comment ~A~%" comment-id)
  (let ((resp (delete-comment (session-value 'api-login) comment-id)))
    (format t "response: ~A~%" resp)
    (format nil "~A" resp)))
