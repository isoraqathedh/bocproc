;;;; bocproc.asd

(asdf:defsystem #:bocproc
  :description "Processing incoming scans"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license "MIT"
  :depends-on (#:local-time #:thread-expr #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "helper")
               (:file "books-and-pages")
               (:file "wandering-pages")
               (:file "script-reading")
               (:file "bookdef-exp")))
