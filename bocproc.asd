;;;; bocproc.asd

(asdf:defsystem #:bocproc
  :description "Processing incoming scans"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :version "6.3.1"
  :license "MIT"
  :depends-on (#:local-time #:thread-expr #:alexandria #:north-drakma #:humbler #:apply-argv
                            #:uuid #:gzip-stream)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "entities")
               (:file "config-reading")
               (:file "series-processing")
               (:file "docs")))
