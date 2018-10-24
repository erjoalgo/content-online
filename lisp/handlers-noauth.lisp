(in-package #:youtube-comments)

(defroutes dispatchers-noauth
    (((:get) "^/health/?$")
     (json-resp '((:status . "OK")))))

(defroutes dispatchers-auth
    (((:get) "^/sick/?$")
     (json-resp '((:status . "sick")))))
