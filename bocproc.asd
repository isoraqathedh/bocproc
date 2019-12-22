;;;; bocproc.asd

(asdf:defsystem #:bocproc
  :description "Processing incoming scans"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :version "6.3.1"
  :license "MIT"
  :depends-on (#:local-time #:thread-expr #:alexandria #:north-drakma #:humbler #:apply-argv)
  :serial t
  :components ((:file "package")
               (:file "helper")
               (:file "objects")
               (:file "exiftool-integration")
               (:file "tumblr-integration")
               (:file "bookdef")
               (:file "bookgen")
               (:file "script-reading")))
