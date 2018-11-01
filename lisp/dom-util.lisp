(in-package #:youtube-comments)

(defun dom-link (url text)
  (params
   ;; comment-id
   "type" "link"
   "text" text
   "url" url))

(defun dom-delete-button (url &key (text "DELETE"))
  (params
   ;; comment-id
   "type" "button/delete"
   "text" text
   "url" url))

(defun dom-lazy-elm (load-url)
  (params
   "type" "lazy-elm"
   "url" load-url))
