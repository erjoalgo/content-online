(fiasco:define-test-package
    #:yt-comments/util-test
  (:use #:yt-comments/util))

(in-package #:yt-comments/util-test)

(fiasco-clear-tests)

(deftest test-to-camel-case nil
  (is (equal "commentThreads" (lisp-to-json-key :comment-threads)))
  (is (equal "nextPageToken" (lisp-to-json-key :next-page-token)))

  (is (equal (json-key-to-lisp "commentThreads")
                    "COMMENT-THREADS"))

  (is (equal (json-key-to-lisp "comment_threads") "COMMENT-THREADS"))

  (is (equal (json-key-to-lisp "commentThreads") "COMMENT-THREADS")))

(deftest test-unmarshall-1 ()
  (is (eq (intern (json-key-to-lisp "keyOne") :keyword) :key-one))
  (defstruct my-unmarshal-test
    key-one
    key-two)
  (let* ((json "{ \"keyOne\" : 1,\"key_two\": 2, \"non-existent\": 3}")
         (json-alist (cl-json:decode-json-from-string json))
         (obj (make-from-json-alist json-alist my-unmarshal-test)))
    (is (equal (my-unmarshal-test-key-one obj) 1))
    (is (equal (my-unmarshal-test-key-two obj) 2))))

(deftest test-params ()
    (is (equal (params "a" 1 "b" 2)
               `(("a" . 1) ("b" . 2))))
  (is (equal (params "a" 1 "b" 2)
             `(("a" . 1) ("b" . 2)))))

(deftest test-unmarshall-2 ()
    (defstruct my-unmarshal-test-2
      access-token
      error
      error-description
      )
  (let* ((json "{
  \"error\" : \"invalid_grant\",
  \"error_description\" : \"Code was already redeemed.\"
}")
         (json-alist (cl-json:decode-json-from-string json))
         (obj (make-from-json-alist json-alist my-unmarshal-test-2)))
    (is (equal (my-unmarshal-test-2-error-description obj) "Code was already redeemed."))
    (is (equal (my-unmarshal-test-2-error obj) "invalid_grant"))))

(deftest test-retry-times ()
    (let ((i 0))
      (is (eq 42 (retry-times 3 .1
                   (if (< (1- (incf i)) 2)
                       (error "err") 42)))))
  (let ((i 0))
    (is (eq 43
            (handler-case
                (retry-times 3 .1
                  (if (< (1- (incf i)) 3)
                      (error "err") 42))
              (error nil 43))))))

(deftest test-extract-paths ()
    (let ((json
           (cl-json:decode-json-from-string
            "{\"a\": {\"b\": 1, \"c\": 2}}")))
      (is (length json) 1)
      (is (length (cdr (assoc :a json))) 2)
      (is (equal (json-path-split "a.b") '(:a :b)))
      (is (equal (json-path-split "a.b[0]") '(:a :b 0)))
      (is (equal (json-path-split "a[0].b") '(:a 0 :b)))
      (is (eq (json-get-nested '((:a . ((:b . 1)))) "a.b") 1))
      (is (equal json '((:a . ((:b . 1) (:c . 2))))))
      (is (eq (json-get-nested json "a.b") 1))
      (is (eq (json-get-nested-macro json "a.c") 2))))

(run-package-tests :interactive t)
