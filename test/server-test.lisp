(fiasco:define-test-package
    #:youtube-comments/test
  (:use #:youtube-comments/server))

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

(deftest test-results-count-handler ()
  (let* ((json (cl-json:decode-json-from-string results-count-json-string))
         (count (youtube-comments/server::results-count-handler json))
)
    (is (equal count "0"))))

(run-package-tests :interactive t)
