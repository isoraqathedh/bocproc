;;;; package.lisp

(defpackage #:info.isoraqathedh.bocproc
  (:use #:cl #:thread-expr #:alexandria)
  (:nicknames #:bocproc)
  (:export #:main #:load-script))

(defpackage #:info.isoraqathedh.bocproc.bpc-parser
  (:nicknames #:bpc)
  (:export #:version #:process-file #:loud))
