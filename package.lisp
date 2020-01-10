;;;; package.lisp

(defpackage #:info.isoraqathedh.bocproc
  (:use #:cl)
  (:documentation "General package for bocproc."))

(defpackage #:info.isoraqathedh.bocproc.core
  (:use #:cl #:info.isoraqathedh.bocproc #:local-time)
  (:documentation "Core concepts for bocproc."))

(defpackage #:info.isoraqathedh.bocproc.entities
  (:nicknames #:bpc-entities)
  (:documentation "Symbols used by entities."))

(defpackage #:info.isoraqathedh.bocproc.data-store
  (:use #:cl #:info.isoraqathedh.bocproc #:local-time)
  (:documentation "Component for managing Book of Conworlds data."))

(defpackage #:info.isoraqathedh.bocproc.script
  (:use #:cl #:info.isoraqathedh.bocproc)
  (:documentation "Component for reading bpc files."))

(import 'humbler::aget '#:info.isoraqathedh.bocproc.core)

(defpackage #:info.isoraqathedh.bocproc.bpc-parser
  (:nicknames #:bpc)
  (:export #:version #:process-file #:next #:multi-post)
  (:import-from #:cl #:nil #:t)
  (:use #:bpc-entities)
  (:documentation "Special package for .bpc files.

Symbols in this package is available for use in bocproc files."))
