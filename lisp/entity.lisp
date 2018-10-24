(in-package #:youtube-comments)

(defstruct channel
  id
  title
  description)

(defstruct video
  id
  title
  channel-id
  channel-title
  published
  description
  rating)

(defstruct comment
  id
  author
  video-id
  channel-id
  reply-count
  text)
