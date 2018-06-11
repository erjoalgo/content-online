(in-package #:yt-comments/client)

(defvar youtube-base-url "https://www.youtube.com")

(defun channel-url (channel-id)
  (format nil "~A/channel/~A" youtube-base-url channel-id))

(defun video-url (video-id)
  (format nil "~A/watch?v=~A" youtube-base-url video-id))

(defun playlist-url (playlist-id)
  (format nil "~A/playlist?list=~A" youtube-base-url playlist-id))
