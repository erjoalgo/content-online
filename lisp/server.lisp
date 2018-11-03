(in-package #:youtube-comments)

(defvar *sites-alist* nil)


(defstruct config
  (port 4244)
  ;; a cons cell: (SSL-CERTIFICATE-FILE . SSL-PRIVATEKEY-FILE)
  (ssl-cert (cons
             #P"./secrets/cert/*.cert"
             #P"./secrets/cert/*.key")))

;; (defclass service (hunchentoot:ssl-acceptor)
(defclass service (hunchentoot:easy-ssl-acceptor)
  ((config :accessor service-config :initarg :config)
   (protocol :accessor service-protocol :initform "ssl")))

(defvar *service* nil)
(defun service-make-start (&rest make-config-args)
  (when *service* (service-stop *service*))
  (let* ((config (apply 'make-config make-config-args)))
    (with-slots (port ssl-cert) config
      (destructuring-bind (cert-rel . key-rel) ssl-cert
        (setf *service*
              (make-instance
               'service
               :CONFIG config
               :SSL-CERTIFICATE-FILE (first-file-matching cert-rel)
               :SSL-PRIVATEKEY-FILE (first-file-matching key-rel)
               :PORT port
               :DOCUMENT-ROOT #P"./www/")))
      (service-start *service*))))

(defmethod service-start ((service service))
  (setf hunchentoot:*dispatch-table*
        (list
         (youtube-dispatcher)
         (hunchentoot::create-folder-dispatcher-and-handler
          "/www/" #P"./www/")))
  (hunchentoot:start service))

(defmethod service-stop ((service service))
  (when (hunchentoot:started-p service)
    (hunchentoot:stop service)))

'(defstruct site
  site-name
  dispatcher-prefix
  oauth-client-pathname
  routes
  top-level-links-alist)

'(defparameter
    youtube-site
  (make-site
   :site-name "youtube"
   :dispatcher-prefix "/youtube"
   :oauth-client-pathname '("" . ("web"))
   :routes-fn 'youtube-app
   :top-level-links-alist
   (params
    "/subscriptions.html" "subscriptions"
    "/playlists.html" "playlists"
    "/html/feed-history.html" "feed history upload"
    "/rated-videos.html" "rated videos")))


(defun youtube-dispatcher ()
  (let* ((youtube-scopes '("https://www.googleapis.com/auth/youtube.force-ssl"))
         (oauth-client (oauth-make-client-from-file
                        (first-file-matching
                         #P"./secrets/oauth-clients/google/*.json")
                        :json-path-to-client "web"))
         (youtube-handler
          (lambda ()
            ;; TODO akward handler-to-dispatcher translation
            (loop
               with request = hunchentoot:*request*
               for dispatcher in *youtube-dispatchers*
               as handler = (funcall dispatcher request)
               when handler do
                 (return (funcall handler))))))
    (hunchentoot:create-prefix-dispatcher
     "/youtube"
     (hunchentoot-create-oauth-redirect-handler
      oauth-client youtube-scopes
      youtube-handler
      ;; TODO decouple this from authenticator
      :oauth-authorize-uri-path "/youtube/oauth/authorize"
      :login-session-key google-login-key))))

(defmethod hunchentoot:maybe-invoke-debugger
    ((condition SB-INT:CLOSED-STREAM-ERROR))
  (vom:debug "ignoring SB-INT:CLOSED-STREAM-ERROR~%"))
