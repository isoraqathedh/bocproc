;;;; package.lisp

(defpackage #:info.isoraqathedh.bocproc
  (:use #:cl #:thread-expr #:alexandria #:local-time)
  (:nicknames #:bocproc)
  (:export #:main #:load-script #:load-config-file))

(import 'humbler::aget '#:bocproc)

(defpackage #:info.isoraqathedh.bocproc.bpc-parser
  (:nicknames #:bpc)
  (:export #:version #:process-file #:next)
  (:import-from #:cl #:nil #:t)
  (:documentation "Special package for .bpc files.

Symbols in this package is available for use in bocproc files."))
