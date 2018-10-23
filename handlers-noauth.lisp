(define-regexp-route health-check ("^/health/?$")
    "health check"
  (json-resp '((:status . "OK"))))

(push (hunchentoot::create-folder-dispatcher-and-handler
       "www/" #P"./www/")
      hunchentoot::*dispatch-table*)
