(defpackage #:yt-comments/server-util
  (:use :cl :cl-markup)
  (:import-from #:yt-comments/util
                #:->)
  (:export #:js-lazy-element))

(defvar js-lazy-load-self-replace-fmt-def
  "function XHR_self_replace_:ID () {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
	if (xmlhttp.readyState == XMLHttpRequest.DONE ) {
	    var innerHTML = xmlhttp.responseText;
            document.getElementById(':ID').innerHTML = innerHTML;
	}
    };
    xmlhttp.open(':VERB', ':URL', true);
    xmlhttp.send();
  }")

(defvar js-lazy-load-self-replace-fmt-funcall
  "XHR_self_replace_:ID();")

(defun replace-all (string from to)
  (cl-ppcre:regex-replace-all from string to))

(defun js-lazy-element (url tmp-content &key as-button (verb :get))
  "Return a 'lazy' element which makes an XHR request to the
specified url, then overwrites itself with the reponse contents.
An optinal tmp-contents are shown while XHR is being executed

If as-button is non-nil, the XHR request does not happen until the button is clicked.
as-button should be a string to be used as the button's value
"
  (let* ((id (write-to-string (random (ash 1 31))))
         (js-defun (-> js-lazy-load-self-replace-fmt-def
                       (replace-all ":ID" id)
                       (replace-all ":URL" url)
                       (replace-all ":VERB" (string-upcase (symbol-name verb)))))
         (js-funcall (-> js-lazy-load-self-replace-fmt-funcall
                         (replace-all ":ID" id)))
         (tmp-content-id (write-to-string (random (ash 1 31))))
         (js-unhide-tmp-and-funcall
          (format nil
                  "document.getElementById('~A').style = 'display:show';
~A;"
                  tmp-content-id
                  js-funcall)))
    (markup (:div :id id
                  (:script :type "text/javascript" (raw js-defun))
                  (:div :style "display:none" :visibility "hidden"
                        :id tmp-content-id (raw tmp-content))
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
