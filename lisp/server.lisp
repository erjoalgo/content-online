(in-package #:youtube-comments)

(defvar *service* nil "the current service")

(defparameter secrets-directory
  #P"./secrets/")

(defstruct config
  (port 4244)
  (oauth-client-secret-json-path
   (check-nonnil
    (first-file-with-extension secrets-directory "json")))

  ;; a cons cell: (SSL-CERTIFICATE-FILE . SSL-PRIVATEKEY-FILE)
  (ssl-cert (cons
             (check-nonnil (first-file-with-extension secrets-directory "cert"))
             (check-nonnil (first-file-with-extension secrets-directory "key")))))

(defstruct service
  acceptor
  config
  oauth-client
  protocol)

(defun start (&rest make-config-args)
  (start-with-config (apply 'make-config make-config-args)))

(defparameter youtube-scopes
  '("https://www.googleapis.com/auth/youtube.force-ssl"))

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

      (vom:info "making ~A service  ~A on port ~A~%" protocol acceptor-class port)

      (setf *service*
            (make-service
             :acceptor (apply 'make-instance acceptor-class acceptor-args)
             :protocol protocol
             :config config
             :oauth-client (oauth-make-client-from-file
                            (config-oauth-client-secret-json-path config)))))

    (let* ((oauth-dispatcher
            (erjoalgo-webutil:create-hunchentoot-oauth-redirect-dispatcher
             (service-oauth-client *service*)
             youtube-scopes))
           (www-dispatcher
            (hunchentoot::create-folder-dispatcher-and-handler
             "/www/" #P"./www/"))
           (app (append
                 dispatchers-noauth
                 (list
                  www-dispatcher
                  oauth-dispatcher)
                 ;; anything below is authenticated
                 dispatchers-auth)))
      (setf hunchentoot:*dispatch-table* app))

    (hunchentoot:start (service-acceptor *service*))
    *service*))

(defun stop (&optional service)
  (setf service (or service *service*))
  (when service
    (vom:info "stopping...")
    (let* ((acceptor (slot-value service 'acceptor)))
      (when (and acceptor (hunchentoot:started-p acceptor))
        (hunchentoot:stop acceptor)))))

(defmethod hunchentoot:maybe-invoke-debugger
    ((condition SB-INT:CLOSED-STREAM-ERROR))
  (vom:debug "ignoring SB-INT:CLOSED-STREAM-ERROR~%"))
