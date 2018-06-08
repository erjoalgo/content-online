(in-package #:yt-comments/util)

(stefil:defsuite* run-tests)

(defstruct my-unmarshal-test
  key-one
  key-two)

(stefil:deftest test-to-camel-case nil
  (stefil:is (equal "commentThreads" (to-camel-case
                                      (symbol-name :comment-threads))))
  (stefil:is (equal "nextPageToken" (to-api-param-key :next-page-token)))

  (stefil:is (equal (from-camel-case "commentThreads" :sep #\-)
                    "comment-threads"))

  (stefil:is (equal (json-key-to-lisp "comment_threads") "COMMENT-THREADS"))

  (stefil:is (equal (json-key-to-lisp "commentThreads") "COMMENT-THREADS"))


  (let* ((json "{ \"keyOne\" : 1,\"key_two\": 2 }")
         (json-alist (jonathan:parse json :as :alist))
        (obj (make-from-json-alist json-alist my-unmarshal-test)))
    (stefil:is (equal (my-unmarshal-test-key-one obj) 1))
    (stefil:is (equal (my-unmarshal-test-key-two obj) 2)))

  (stefil:is (equal (flat-to-alist "a" 1 "b" 2)
                    `(("a" . 1) ("b" . 2))))
  (stefil:is (equal (flat-to-alist-macro "a" 1 "b" 2)
                    `(("a" . 1) ("b" . 2)))))



(run-tests)
