(fiasco:define-test-package
    #:youtube-comments/test
  (:use #:youtube-comments))

(in-package #:youtube-comments/test)

(defvar results-count-json-string
  "{
 \"kind\": \"youtube#commentThreadListResponse\",
 \"etag\": \"\\\"XI7nbFXulYBIpL0ayR_gDh3eu1k/ZG21yj3BiKrd-_sPro4Uu2c55GM\\\"\",
 \"pageInfo\": {
  \"totalResults\": 0,
  \"resultsPerPage\": 50
 },
 \"items\": []
}
")

'(deftest test-results-count-handler ()
  (let* ((json (cl-json:decode-json-from-string results-count-json-string))
         (count (youtube-comments::results-count-handler json))
)
    (is (equal count "0"))))

(deftest test-uniquify ()
  (fiasco:is (eq 3 (length (youtube-comments::uniquify '((1 . 1) (1 . 2) (2 . 3)) elt (car elt)))))
  (fiasco:is (eq 3 (length (youtube-comments::uniquify '((1 . 1) (1 . 2) (2 . 3))
                                                       elt (cdr elt))))))

(defparameter *base-url*
  (format nil "~A://localhost:~D"
          (youtube-comments::service-protocol youtube-comments::*service*)
          (youtube-comments::config-port
           (youtube-comments::service-config youtube-comments::*service*))))

(defun endpoint (path)
  (concatenate 'string *base-url* path))

(deftest test-noauth ()
  (loop for path in '("/health"
                      "/www/html/privacy.html")
     do
       (multiple-value-bind (content status)
           (drakma:http-request
            ;; (endpoint )
            (endpoint path)
            :redirect nil)
         (declare (ignore content))
         (is (equal (cons path 200)
                    (cons path status))))))

(youtube-comments::stop)
(youtube-comments::start)

;; (run-package-tests :interactive t)
(run-package-tests)
