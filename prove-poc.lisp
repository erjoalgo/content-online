(defmacro with-symbols-from ((package-name &rest symbols) &body body)
;; https://stackoverflow.com/questions/18679993/
  (let ((f-symbols (loop :for s :in symbols :collect
                        (intern (symbol-name s) package-name))))
    `(progn
       ,@(loop :for symbol :in symbols :for f-symbol :in f-symbols
            :with body = body
            :do (setf body (subst f-symbol symbol body :test #'eq))
            :finally (return body)))))


(setf prove:*enable-colors* nil)
(setf prove:*debug-on-error* t)

(with-symbols-from (prove ok is subtest isnt)
  (subtest "Showing off Prove"
    (ok (not (find 4 '(1 2 3))))
    (is 4 4)
    (is 4 5)
    (isnt 1 #\1)))
