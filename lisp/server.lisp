(in-package #:youtube-comments)

;; (setf hunchentoot:*dispatch-table* nil)
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
             :oauth-client (make-oauth-client-from-file
                            (config-oauth-client-secret-json-path config)))))

    (let* ((oauth-dispatcher
            (erjoalgo-webutil/google:create-hunchentoot-oauth-redirect-dispatcher
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
             (vom:warn "unexpected code: ~A ~A ~A~%"
                       ,body-sym ,http-code-sym ,resp-string-sym)
             (hunchentoot:abort-request-handler
              (format nil "~A ~A" ,http-code-sym ,resp-string-sym)))
           ,body-sym))))

(defmethod hunchentoot:maybe-invoke-debugger
    ((condition SB-INT:CLOSED-STREAM-ERROR))
  (format t "ignoring SB-INT:CLOSED-STREAM-ERROR~%"))
