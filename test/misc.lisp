(erjoalgo-webutil:->
 youtube-comments::*service*
 youtube-comments::service-acceptor
 hunchentoot::session-db
 car
 cdr
 hunchentoot::session-data
 (erjoalgo-webutil:assoq 'YOUTUBE-COMMENTS::API-LOGIN))


(defmacro log-values (form)
  (let ((vals-sym (gensym "vals")))
    `(let ((,vals-sym (multiple-value-call #'list
                        ,form)))
       (format t "values: ~A~%" ,vals-sym)
       (values-list ,vals-sym))))

;; (defclass vhost (hunchentoot:acceptor)
(defclass vhost ()
  ;; slots
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  ;; options
  (:default-initargs                    ; default-initargs must be used
   :address "127.0.0.1"))               ; because ACCEPTOR uses it


(defvar dbg);;debugging

;;; Specialise ACCEPTOR-DISPATCH-REQUEST for VHOSTs
'(defmethod hunchentoot:acceptor-dispatch-request ((vhost vhost) request)
  ;; try REQUEST on each dispatcher in turn
  (format t "hello world!")
  (setf dbg "hola")
  (redirect "https://google.com")
  ;; (/ 1 0)
  '(mapc (lambda (dispatcher)
	  (let ((handler (funcall dispatcher request)))
	    (when handler               ; Handler found. FUNCALL it and return result
	      (return-from hunchentoot:acceptor-dispatch-request (funcall handler)))))
	(dispatch-table vhost))
  '(hunchentoot::call-next-method))
