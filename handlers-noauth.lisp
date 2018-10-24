(in-package #:youtube-comments)

(push (hunchentoot::create-folder-dispatcher-and-handler
       "www/" #P"./www/")
      hunchentoot::*dispatch-table*)
(defroutes dispatchers-noauth
    (((:get) "^/health/?$")
     (json-resp '((:status . "OK")))))
