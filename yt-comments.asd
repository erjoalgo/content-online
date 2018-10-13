(asdf:defsystem :yt-comments
  :serial t
  :description "tool to retrieve and delete youtube comments"
  :license "GPLv3"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :depends-on (:drakma
               :jonathan
               :hunchentoot
               :vom
               :command-line-arguments
               :cl-ppcre
               :cl-markup
               :stefil
               )
  :components ((:file "packages")
               (:file "util")
               (:file "oauth")
               (:file "client")
               (:file "repr")
               (:file "server-util")
               (:file "server")))
