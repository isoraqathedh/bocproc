;;;; bocproc.asd

(asdf:defsystem #:bocproc
  :description "Processing incoming scans"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license "MIT"
  :depends-on (#:local-time #:cl-yaclyaml #:thread-expr)
  :serial t
  :components ((:file "package")
               (:file "books-and-pages")))
