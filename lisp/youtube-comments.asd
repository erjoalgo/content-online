(asdf:defsystem :youtube-comments
                :serial t
                :description "tool to retrieve and delete youtube comments"
                :license "GPLv3"
                :author "Ernesto Alfonso <erjoalgo@gmail.com>"
                :depends-on (:drakma
                             :hunchentoot
                             :vom
                             :command-line-arguments
                             :cl-ppcre
                             :cl-json
                             :cl-markup
                             :fiasco
                             :erjoalgo-webutil)
                :components ((:file "packages")
                             (:file "youtube-client")
                             (:file "lazy-handler")
                             (:file "server")
                             (:file "entity")
                             (:file "handler-util")
                             (:file "handlers")
                             (:file "handlers-noauth")))
