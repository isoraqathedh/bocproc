(in-package :bocproc)

(defvar +alphabet+ "abcdefghijklmnopqrstuvwxyz"
  ;; It's actually a constant
  ;; but using defvar makes redefinition warnings makes the errors go away.
  "The alphabet as required in the subpage specifications.")

(defun make-subdirectory-pathname (root directories &key name type)
  "Creates pathname representing a file name or subdirectory in ROOT.

DIRECTORIES is a list of strings that represent folder names,
and NAME and TYPE is as in `make-pathname'."
  (merge-pathnames
   (make-pathname :directory (cons :relative directories)
                  :name name
                  :type type)
   root))

(defvar *config-file*
  (make-subdirectory-pathname
   (uiop:xdg-config-home) '("bocproc")
   :name "config"
   :type "lisp")
  "Location of the file that holds the config file.")

(defparameter *books-location*
  (make-subdirectory-pathname
   (uiop/common-lisp:user-homedir-pathname) '("Documents" "My Scans"))
  "Root directory of the scan destinations.")

(defparameter *config* ()
  "Configuration tree.")

(defparameter *exists-list* ()
  "List for finding which files are there and not.")

(defun letter->number (letter)
  (let ((maybe-position (position letter +alphabet+)))
    (if maybe-position
        (1+ maybe-position)
        (error "Not a letter: ~s" letter))))

(defun number->letter (number)
  (when (and number (<= 1 number 26))
    (char +alphabet+ (1- number))))

(defun load-config-file ()
  "Reads the config file into *config*."
  (with-open-file (s *config-file* :external-format :utf-8)
    (setf *config* (read s))))

(defun scan-for-files ()
  "Detects and stores all files in *BOOKS-LOCATION*.
Returns number of files detected, as this can be very large."
  (with-expression-threading ()
    *books-location*
    (make-subdirectory-pathname :|| '(:wild-inferiors)
                                :name :wild
                                :type :wild)
    #'directory
    (setf *exists-list* :||)
    #'length))

(defun books-location-subdir (&rest folder-names)
  (make-subdirectory-pathname
   *books-location* folder-names))
