(in-package #:youtube-comments)

(defroutes dispatchers-noauth
    (((:get) "^/health/?$")
     (json-resp '((:status . "OK")))))

(defparameter www-dispatcher
  (hunchentoot::create-folder-dispatcher-and-handler
 "/www/" #P"./www/"))

;; (defroutes dispatchers-auth
;;     (((:get) "^/sick/?$")
;;      (json-resp '((:status . "sick")))))
