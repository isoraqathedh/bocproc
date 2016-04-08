;;;; Helper script to compile the executable
;;;; Not meant to be part of the system, but to be invoked like so:
;;;; $ sbcl --load compile.lisp

(ql:quickload :bocproc)
(in-package :bocproc)
(sb-ext:save-lisp-and-die
 #+os-windows "bocproc.exe"
 #+os-unix "bocproc"
 :toplevel #'main
 :compression #+sb-core-compression t
              #-sb-core-compression nil
 :executable t)
