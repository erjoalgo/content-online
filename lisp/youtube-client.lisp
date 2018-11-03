(in-package #:youtube-comments)

(SB-CLTL2:COMPILER-LET
    ((erjoalgo-webutil::authenticator 'erjoalgo-webutil::google-authenticator)
     (erjoalgo-webutil::depaginator nil)
     (erjoalgo-webutil::base-url "https://www.googleapis.com/youtube/v3")
     (erjoalgo-webutil::api-req-extra-args-compile-time `(:retry-count 10)))

  ;; get queries.
  ;; default req-update passes additional query params from caller
  (SB-CLTL2:COMPILER-LET
      ((erjoalgo-webutil::method :get)
       (erjoalgo-webutil::qparams '((:part . "snippet") (:max-results . "100")))
       (erjoalgo-webutil::req-update
        '((&optional extra-qparams) http-req
          (with-slots (qparams) http-req
            (setf qparams (append qparams extra-qparams))))))

    ;; non-depaginating here...

    (SB-CLTL2:COMPILER-LET
        ((erjoalgo-webutil::depaginator 'erjoalgo-webutil::google-depaginator))
      ;; depaginating...

      (defendpoint comment-threads-get :resource "/commentThreads")

      (defendpoint subscriptions-get :resource "/subscriptions")

      (defendpoint playlists-get :resource "/playlists")

      (defendpoint playlist-items-get :resource "/playlistItems")

      (defendpoint channels-get :resource "/channels")

      (defendpoint videos-get :resource "/videos")

      (defendpoint search-get :resource "/search")

      (defendpoint activities-get :resource "/activities")))

  (SB-CLTL2:COMPILER-LET
      ((erjoalgo-webutil::method :delete))
    (defendpoint comment-delete
        :resource "/comments"
        :req-update
        ((comment-id) http-request
         (with-slots (qparams) http-request
           (push (cons "id" comment-id) qparams))))))

(defvar youtube-base-url "https://www.youtube.com")

(defun channel-url (channel-id)
  (format nil "~A/channel/~A" youtube-base-url channel-id))

(defun video-url (video-id)
  (format nil "~A/watch?v=~A" youtube-base-url video-id))

(defun playlist-url (playlist-id)
  (format nil "~A/playlist?list=~A" youtube-base-url playlist-id))
