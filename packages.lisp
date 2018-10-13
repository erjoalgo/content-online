(defpackage #:yt-comments/server-util
  (:use :cl :cl-markup)
  (:export #:js-lazy-element))

(defpackage #:yt-comments/util
  (:use :cl)
  (:export #:flat-to-alist
           #:flat-to-alist-macro
           #:read-file
           #:retry-times
           #:assoq
           #:make-from-json-alist
           #:flat-to-alist-macro
           #:read-file
           #:make-from-json-alist
           #:->
           #:assoq
           #:TO-CAMEL-CASE
           #:with-json-paths
           #:LISP-ALIST-TO-JSON-MAP
           #:get-nested-macro))

(defpackage #:yt-comments/oauth
  (:use :cl)
  (:import-from #:yt-comments/util
                #:flat-to-alist-macro
                #:read-file
                #:make-from-json-alist
                #:->
                #:assoq
                )
  (:export #:make-oauth-client-from-file
           #:OAUTH-TOKEN-AUTH-HEADER
           #:AUTH-SERVER-REDIRECT-URL
           #:EXCHANGE-CODE-FOR-TOKEN
           #:RESP-TOKEN-ACCESS-TOKEN
           #:RESP-TOKEN-REFRESH-TOKEN))

(defpackage #:yt-comments/client
  (:use :cl)
  (:import-from #:yt-comments/util
                #:to-camel-case
                #:->
                #:make-from-json-alist
                #:lisp-alist-to-json-map
                #:retry-times
                #:with-json-paths)
  (:import-from #:yt-comments/oauth
                #:oauth-token-auth-header)
  (:export #:make-api-login
           #:default-base-url
           #:subscriptions
           #:comment-threads
           #:subscriptions
           #:playlists
           #:playlist-items
           #:channels
           #:videos
           #:search
           #:activities
           #:channel-url
           #:video-url
           #:playlist-url
           #:delete-comment
           ))

(defpackage #:yt-comments/server
  (:use :cl :cl-markup)
  (:import-from #:yt-comments/util
                #:with-json-paths
                #:->
                #:get-nested-macro
                #:assoq
                )
  (:import-from #:yt-comments/server-util
                #:js-lazy-element)
  (:import-from #:yt-comments/client
                #:make-api-login
                #:subscriptions
                #:comment-threads
                #:channel-url
                #:video-url
                #:playlist-url
                #:delete-comment
                #:playlists
                #:playlist-items
                )
  (:import-from #:yt-comments/oauth
                #:make-oauth-client-from-file
                #:auth-server-redirect-url
                #:exchange-code-for-token
                #:resp-token-access-token
                #:resp-token-refresh-token
                )
  (:import-from #:hunchentoot
                #:session-value
                #:redirect)
  (:export #:start))

