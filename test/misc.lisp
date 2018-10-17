(erjoalgo-webutil:->
 youtube-comments/server::*service*
 youtube-comments/server::service-acceptor
 hunchentoot::session-db
 car
 cdr
 hunchentoot::session-data
 (erjoalgo-webutil:assoq 'YOUTUBE-COMMENTS/SERVER::API-LOGIN))


(defmacro log-values (form)
  (let ((vals-sym (gensym "vals")))
    `(let ((,vals-sym (multiple-value-call #'list
                        ,form)))
       (format t "values: ~A~%" ,vals-sym)
       (values-list ,vals-sym))))
