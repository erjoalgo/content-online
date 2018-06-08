(asdf:defsystem :yt-comments
  :serial t
  :description "tool to retrieve and delete youtube comments"
  :license "GPLv3"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :depends-on (:drakma
               :jonathan
               :vom
               :command-line-arguments
               :cl-ppcre
               :cl-markup
               :stefil
               )
  :components ((:file "util")
               (:file "client")
               (:file "repr")
               (:file "oauth")
               (:file "server")))
