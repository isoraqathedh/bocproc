;;;; bocproc.asd

(asdf:defsystem #:bocproc
  :description "Processing incoming scans"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license "MIT"
  :depends-on (#:local-time #:thread-expr #:alexandria #:humbler)
  :serial t
  :components ((:file "package")
               (:file "helper")
               (:file "exiftool-integration")
               (:file "bookdef")
               (:file "bookgen")
               (:file "script-reading")))
