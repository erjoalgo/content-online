

(fiasco:define-test-package
    #:yt-comments/oauth-test
  (:use #:yt-comments/oauth
        #:yt-comments/util))

(in-package #:yt-comments/oauth-test)

;; delete previously defined, renamed tests

(fiasco-clear-tests)

(defvar json-foo_bar
  "{\"foo_bar\": 1}")

(defun object-bound-slots (obj)
  (remove-if-not (lambda (slot) (slot-boundp obj slot))
                 (mapcar (lambda (slot)
                           (slot-value slot 'sb-pcl::name))
                         (sb-mop:class-slots (class-of obj)))))

(defparameter client-json
  "{
  \"web\": {
    \"client_id\": \"REMOVED.apps.googleusercontent.com\",
    \"project_id\": \"the-id\",
    \"auth_uri\": \"https://accounts.google.com/o/oauth2/auth\",
    \"token_uri\": \"https://accounts.google.com/o/oauth2/token\",
    \"auth_provider_x509_cert_url\": \"https://www.googleapis.com/oauth2/v1/certs\",
    \"client_secret\": \"the-secret\",
    \"redirect_uris\": [
      \"***REMOVED***\",
      \"http://localhost\"
    ]
  }
}")


(deftest test-json-parsing ()
  (let ((json client-json)
        (tmp-filename #P"/tmp/oauth-test.json"))
    (with-open-file (fh tmp-filename
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
      (format fh "~A" json))

    (let ((oauth-client (make-oauth-client-from-file
                         tmp-filename)))
      (format t "recovered client ~A~%" oauth-client)
      (with-slots (yt-comments/oauth::client-secret
                   yt-comments/oauth::client-id
                   yt-comments/oauth::auth-uri)
          oauth-client
        (is (equal "the-secret" yt-comments/oauth::client-secret))
        (is (equal "REMOVED.apps.googleusercontent.com" yt-comments/oauth::client-id))))))

(run-package-tests :interactive t)
