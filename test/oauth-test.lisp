

(stefil:deftest test-json-parsing ()
  (let ((json
         "{
  \"installed\": {
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
        (tmp-filename #P"/tmp/oauth-test.json"))
    (with-open-file (fh tmp-filename
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
      (format fh "~A" json))
    (let ((oauth-client (yt-comments/oauth:make-oauth-client-from-file tmp-filename)))
      (format t "recovered client ~A~%" oauth-client)
      (with-slots (yt-comments/oauth::client-secret
                   yt-comments/oauth::client-id
                   yt-comments/oauth::auth-uri)
          oauth-client
          (stefil:is (equal "the-secret" yt-comments/oauth::client-secret))
        (stefil:is (equal "REMOVED.apps.googleusercontent.com" yt-comments/oauth::client-id))))))

(run-tests)
