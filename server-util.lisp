(defpackage #:yt-comments/server-util
  (:use :cl :cl-markup)
  (:import-from #:yt-comments/util
                #:->)
  (:export #:js-lazy-element))

(defvar js-lazy-load-self-replace-fmt

 "function XHR_self_replace_:ID () {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
	if (xmlhttp.readyState == XMLHttpRequest.DONE ) {
	    var innerHTML = xmlhttp.responseText;
            document.getElementById(':ID').innerHTML = innerHTML;
	}
    };
    xmlhttp.open('GET', ':URL', true);
    xmlhttp.send();
  }
  XHR_self_replace_:ID();"
  )

(defun replace-all (string from to)
  (cl-ppcre:regex-replace-all from string to))

(defun js-lazy-element (url &optional initial-content)
  "Return a 'lazy' element which makes an XHR request to the
specified url, then overwrites itself with the reponse contents.
An optinal tmp-contents are shown while XHR is executed"
  (let* ((id (format nil "~D" (random (ash 1 31))))
         (js-script (-> js-lazy-load-self-replace-fmt
                        (replace-all ":ID" id)
                        (replace-all ":URL" url))))
    (markup (:div :id id
                  (raw initial-content)
                  (:script :type "text/javascript" (raw js-script))))))
