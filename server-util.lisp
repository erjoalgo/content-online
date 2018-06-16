(defpackage #:yt-comments/server-util
  (:use :cl :cl-markup)
  (:export #:js-lazy-element))

(in-package #:yt-comments/server-util)

(defparameter js-lazy-load-self-replace-fmt-def
  "function XHR_self_replace (id, url, verb) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
	if (xmlhttp.readyState == XMLHttpRequest.DONE ) {
	    var innerHTML = xmlhttp.responseText;
            document.getElementById(id).innerHTML = innerHTML;
	}
    };
    xmlhttp.open(verb, url, true);
    xmlhttp.send();
  }")

(defparameter js-lazy-load-self-replace-fmt-funcall
  "XHR_self_replace('~A', '~A', '~A');")

(defun js-lazy-element (url tmp-content &key as-button
                                          (verb :get)
                                          skip-self-replace-fun)
  "Return a 'lazy' element which makes an XHR request to the
specified url, then overwrites itself with the reponse contents.
An optinal tmp-contents are shown while XHR is being executed

If as-button is non-nil, the XHR request does not happen until the button is clicked.
as-button should be a string to be used as the button's value
"
  (let* ((id (write-to-string (random (ash 1 31))))
         (tmp-content-id (write-to-string (random (ash 1 31))))
         (js-funcall (format nil js-lazy-load-self-replace-fmt-funcall
                             id url verb))
         (initially-hidden-p as-button)
         (js-unhide-tmp-and-funcall
          (concatenate 'string
                       (if initially-hidden-p
                           (format nil "document.getElementById('~A').style = 'display:show';"
                                   tmp-content-id)
                           "")
                       js-funcall)))

    (markup (:div :id id
                  (raw
                   (unless skip-self-replace-fun
                    (markup (:script :type "text/javascript"
                                     (raw js-lazy-load-self-replace-fmt-def)))))
                  (raw (markup* `(:div ,@(if (not initially-hidden-p) nil
                                       '(:style "display:none" :visibility "hidden"))
                                 :id ,tmp-content-id (raw ,tmp-content))))
                  (raw
                   (if (null as-button)
                       (markup* `(:script :type "text/javascript"
                                          (raw ,js-unhide-tmp-and-funcall)))
                       (markup (:input :type "button"
                                       :onclick
                                       (format nil "~A; ~A"
                                               ;; hide this button
                                               "this.style = 'display:none';"
                                               js-unhide-tmp-and-funcall)
                                       :value as-button))
                       ))))))
