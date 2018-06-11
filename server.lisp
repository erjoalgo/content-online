(defpackage #:yt-comments/server
  (:use :cl :cl-markup)
  (:import-from #:yt-comments/util
                #:with-json-paths
                )
  (:import-from #:yt-comments/client
                #:subscriptions
                #:comment-threads
                #:channel-url
                #:video-url
                )
  (:import-from #:yt-comments/oauth
                #:make-oauth-client-from-file
                #:auth-server-redirect-url
                #:oauth-exchange-code-for-token)
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
  (format nil "~A~A" (hunchentoot:host) oauth-authorize-uri-path))

(defun oauth-redirect (original-url)
  (setf (session-value original-url) original-url)
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
         (if (not (session-value 'token))
             (oauth-redirect (hunchentoot:request-uri*))
             (progn ,@body))))
     (push (hunchentoot:create-regex-dispatcher ,url-regexp ',name)
           hunchentoot:*dispatch-table*)))

(hunchentoot:define-easy-handler (oauth-authorize-handler :uri oauth-authorize-uri-path)
    (code)
  (assert (session-value 'original-url))
  (let ((token (oauth-exchange-code-for-token code))
        (original-url (session-value 'original-url)))
    (setf (session-value token) token)
    (redirect original-url)))

'(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

(defvar db)

(defmacro make-table (headers rows row-idx-sym row-sym row-cols-list)
  `(markup
    (:table
     :border 0
     :cellpadding 4
     (:tr :align "right"
          (loop for header in ,headers collect
               (markup (:td (:b header)))))
     (loop
        for ,row-sym in ,rows
        for ,row-idx-sym from 1
        collect
          (markup
           (:tr :align "right"
                (loop
                   for cell in ,row-cols-list
                   collect (markup (:td (raw cell))))))))))

(define-regexp-route root-handler ("^/$")
    "initiate session and fetch token"
  ;; TODO ask for username?
  (let* ((redirect-url (format nil "~A~A"
                              (hunchentoot:host)
                              oauth-authorize-uri-path))
         (oauth-client (service-oauth-client *service*))
        (url (auth-server-redirect-url oauth-client redirect-url)))
    (hunchentoot:redirect url)))

(define-regexp-route user-name-handler ("^user/([^/]*)$" username)
    "initiate session and fetch token"
  (hunchentoot:start-session)
  (setf (session-value username) username))

(define-regexp-route subscriptions-handler ("^/subscription/([^/]*)$" channel-id)
    "list subscriptions for the given channel id"
  ;; (format t "have ~A subs~%" (length subs))
  ;; (setf db subs)
  ;; defmacro (headers rows-form row-idx-sym row-sym row-cols-list-form)
  (make-table '("#" "channel id" "description" "url" "commments")
              ;; db
              (subscriptions (config-api-login (service-config *service*))
                                    :channel-id channel-id
                                    :part "snippet")
              sub-idx sub
              (with-json-paths sub
                  (sub-chan-id "snippet.resourceId.channelId" sub-title "snippet.title")
                (let* ((sub-url (channel-url sub-chan-id))
                      (user-id (session-value 'username))
                      (sub-comments-link (format nil "/user/~A/subscription/~A/comments"
                                                 user-id sub-chan-id)))
                      (list (format nil "~D" sub-idx) sub-chan-id sub-title
                            (markup (:a :href sub-url sub-url))
                            (markup (:a :href sub-comments-link "comments!")))))))

(define-regexp-route comments
    ("^/user/([^/]*)/subscription/([^/]*)$" user-name sub-channel-id)
    "list comments for the given user on the given subscription"
  (make-table '("#" "id" "author" "video or channel id" "reply count" "text")
              (comment-threads (config-api-login (service-config *service*))
                               :part "snippet"
                               :search-terms user-name
                               :all-threads-related-to-channel-id sub-channel-id
                               )
              comment-idx comment
              (with-json-paths comment
                  (comment-author "snippet.topLevelComment.snippet.authorDisplayName"
                                  comment-id "id"
                                  comment-video-id "snippet.videoId"
                                  comment-channel-id "snippet.channelId"
                                  comment-reply-count "snippet.totalReplyCount"
                                  comment-text "snippet.topLevelComment.snippet.textOriginal"
                                  )
                (let ((comment-page-url (if comment-video-id
                                            (video-url comment-video-id)
                                            (channel-url comment-channel-id)))
                      (delete-comment-link (format nil "/comment/~A/delete" comment-id)))
                  (list (format nil "~D" comment-idx)
                        comment-id
                        comment-author
                        comment-page-url
                        comment-reply-count
                        comment-text
                        (markup (:a :href delete-comment-link "delete!")))))))
