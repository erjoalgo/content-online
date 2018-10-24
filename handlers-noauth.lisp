(define-regexp-route health-check ("^/health/?$")
    "health check"
  (json-resp '((:status . "OK"))))
(in-package #:youtube-comments)

(push (hunchentoot::create-folder-dispatcher-and-handler
       "www/" #P"./www/")
      hunchentoot::*dispatch-table*)
