(defmacro json-to-class (class
                         identifier-name-to-lisp
                         &body body)
  (let ((instance-sym (gensym "instance"))
        (slot-name-sym (gensym "slot-name")))

    `(let ((,instance-sym (make-instance ',class))
           ,slot-name-sym
           (cl-json:*json-identifier-name-to-lisp* ,identifier-name-to-lisp))
       (json:with-custom-decoder-level
           (
            ;; :identifier-name-to-lisp ,identifier-name-to-lisp
            :object-key (lambda (key)
                          (setf ,slot-name-sym
                                (intern
                                 (funcall ,identifier-name-to-lisp
                                          key))))
            :object-value (lambda (value)
                            (setf (slot-value ,instance-sym ,slot-name-sym) value)))
         ,@body)
       ,instance-sym)))

'(deftest cl-json-key-transformation ()
  (is (equal "foo-bar" (cl-ppcre:regex-replace-all "_" "foo_bar" "-")))

  (let* (
         (cl-json:*json-symbols-package* nil)
         (cl-json:*json-identifier-name-to-lisp* 'underscore-to-dash)
         (fluid
          (json:with-decoder-simple-clos-semantics
            (cl-json:decode-json-from-string json-foo_bar))))

    (format t "slots: ~{~A~^~%~}~%" (object-bound-slots fluid))

    (is (equal (funcall cl-json:*json-identifier-name-to-lisp* "foo_bar")
               "FOO-BAR"))

    (is (slot-boundp fluid 'foo-bar))))

(defvar 2-level-json
  "{\"foo_bar\": {\"a_1\":1, \"b_1\":2}}")

(deftest cl-json-top-level-value-accumulation ()

  (let* (
         (cl-json:*json-symbols-package* nil)
         (cl-json:*json-identifier-name-to-lisp* 'underscore-to-dash)
         keys-seen
         values-seen
         (key-handler
          (lambda (key)
            (format t "oauth-test: value of key: ~A~%" key)
            (push key keys-seen)))
         (value-handler
          (lambda (value)
            (format t "oauth-test: value of cl-json::*accumulator-last*: ~A~%"
                    cl-json::*accumulator-last*)
            (format t "oauth-test: value of value: ~A~%" value)
            (push value values-seen))))

    (json:with-custom-decoder-level
        (
         :object-key key-handler
         :object-value value-handler)
      (cl-json:decode-json-from-string 2-level-json))

    (is (eq 1 (length keys-seen)))
    (is (equal "foo_bar" (car keys-seen)))
    (is (equal '((A-1 . 1) (B-1 . 2)) (car values-seen)))

    ;; json:*object-key-handler*
    ;; cl-json:*object-value-handler*
    ;; (fluid (json:with-decoder-simple-clos-semantics))

    ;; (format t "slots: ~{~A~^~%~}~%" (object-bound-slots fluid))
    ;; (is (slot-boundp fluid 'foo-bar))

    ))

(deftest test-json-to-class ()
    (defstruct my-test-struct
      foo-bar
      another-slot)

    (let* ((instance
         (json-to-class my-test-struct
             'underscore-to-dash
           (cl-json:decode-json-from-string 2-level-json))))
    (format t "oauth-test: value of instance: ~A~%" instance)
    (is (equal (slot-value instance 'foo-bar)
               '((:A-1 . 1) (:B-1 . 2))))))
