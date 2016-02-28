;;;; Reading scripts

#| This file reads in a script file from some directory
and interprets it as commands. |#

(in-package :bocproc)

(defclass processing-session ()
  ((in-file :initarg :in-file
            :reader in-file
            :documentation
            "The location of the file that is currently being processed.")
   (current-file :initform nil
                 :documentation
                 "The page object of the most recent file being processed.")
   (exiftool-file :documentation
                  "The location of the file that is passed to exiftool."))
  (:documentation "Represents a single file of BPC that has to be processed
and translated."))

(defmethod print-object ((object processing-session) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a.~a"
            (pathname-name (in-file object))
            (pathname-type (in-file object)))))
