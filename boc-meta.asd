;;;; bocproc.asd

(asdf:defsystem #:bocproc
  :description "Describe boc-meta here"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license "MIT"
  :depends-on (#:zpb-exif #:local-time #:cl-json #:thread-expr)
  :serial t
  :components ((:file "package")
               (:file "books-and-pages")))
