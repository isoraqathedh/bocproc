(in-package :bocproc)

(defconstant +alphabet+ "abcdefghijklmnopqrstuvwxyz"
  "The alphabet as required in the subpage specifications.")

(defvar *config-file* (asdf:system-relative-pathname
                       :bocproc "config" :type "lisp"))

(defparameter *books-location*
  (merge-pathnames
   (make-pathname :directory '(:relative "Documents" "My Scans"))
   (uiop/common-lisp:user-homedir-pathname)))

(defparameter *config* ()
  "Configuration tree.")

(defparameter *ignore-list* ()
  "Hash-table for the ignore list.")

(defun letter->number (letter)
  (let ((maybe-position (position letter +alphabet+)))
    (if maybe-position
        (1+ maybe-position)
        (error "Not a letter: ~s" letter))))

(defun number->letter (number)
  (when number
    (if (<= 1 number 26)
        (char +alphabet+ (1- number)))))

(defun load-config-file ()
  "Reads the config file into *config*."
  (with-open-file (s *config-file* :external-format :utf-8)
    (setf *config* (read s))))

(defun books-location-subdir (&rest folder-names)
  (merge-pathnames
   (make-pathname :directory (cons :relative folder-names))
   *books-location*))
