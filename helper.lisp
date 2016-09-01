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
    (let ((*package* (find-package '#:bpc)))
      (setf *config* (read s))
      (loop for i in (append (cdr (assoc :tags *config*))
                             (cdr (assoc :books *config*)))
            do (export (car i))))
    (loop for (name specs . format) in (cdr (assoc :books *config*))
          do (define-book% name format specs))))

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

(defun ensure-keyword (symbol)
  "Coerces SYMBOL into a keyword."
  (or (find-symbol (symbol-name symbol) "KEYWORD")
      (intern (symbol-name symbol) "KEYWORD")))

(defun symbol-name-assoc-cdr (keyform alist)
  "Like ASSOC, but compares only symbol names (case-insensitively)."
  (cdr (assoc keyform alist :test #'string-equal :key #'symbol-name)))

(defun setup ()
  "Runs the functions that read configuration files."
  (load-config-file)
  (reread-timezone-repository)
  (scan-for-files)
  t)

(defun clear ()
  "Removes all setup information."
  (setf *exists-list* ()
        *series-list* ()
        *config* ()))

(defun get-timezone ()
  "Retrieves the timezone as set by the configuration variable."
  ;; Ensure that the timezone repository is read.
  (when (zerop (hash-table-count local-time::*location-name->timezone*))
    (reread-timezone-repository))
  ;; Now get the timezone.
  (with-expression-threading ()
    *config*
    (assoc :timezone :||)
    #'cdr
    #'find-timezone-by-location-name))
