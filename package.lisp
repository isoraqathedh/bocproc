;;;; package.lisp

(defpackage #:info.isoraqathedh.bocproc
  (:use #:cl #:thread-expr #:alexandria)
  (:nicknames #:bocproc)
  (:export #:main #:load-script))

(defpackage #:info.isoraqathedh.bocproc.experimental
  (:use #:cl #:thread-expr #:alexandria #:local-time)
  (:nicknames #:bocproc-exp))

(defpackage #:info.isoraqathedh.bocproc.bpc-parser
  (:nicknames #:bpc)
  (:export #:version #:process-file #:loud)
  (:import-from #:cl #:nil #:t))
