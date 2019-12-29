(in-package #:youtube-comments)



(defstruct config
  (port 4244))

;; subclass hunchentoot acceptor to add some custom accessors
(defclass service (hunchentoot:easy-acceptor)
  ((config :accessor service-config :initarg :config)))

(defvar *service* nil)
(defun start (&rest make-config-args)
  (when *service* (service-stop *service*))
  (let* ((config (apply 'make-config make-config-args)))
    (with-slots (port) config
      (setf *service*
            (make-instance
             'service
             :CONFIG config
             :PORT port
             :DOCUMENT-ROOT #P"./www/"))
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
