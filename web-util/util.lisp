(defun read-file (filename)
  "Read file contents into a string."
  (with-output-to-string (out)
    (with-open-file (in filename)
      (format out "~{~A~^~%~}"
              (loop as line = (read-line in nil)
                 while line
                 collect line)))))

(defmacro assoq (alist item)
  "get ITEM in ALIST"
  `(cdr (assoc ,item ,alist :test 'equal)))

(defmacro -> (&rest forms)
  "Threading macro."
  (if (cadr forms)
      (destructuring-bind (first second . rest) forms
	(destructuring-bind (a . a-rest) (if (atom second)
					     (cons second nil)
                                             second)
	  `(-> ,(apply 'list a first a-rest) ,@rest)))
      (car forms)))