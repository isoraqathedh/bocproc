(in-package #:info.isoraqathedh.bocproc.core)

(defvar +alphabet+ "abcdefghijklmnopqrstuvwxyz")

(defvar +bocproc-uuid+
  (uuid:make-uuid-from-string "87832309-5EE5-48D0-B2C7-41E88531B360"))

(defvar *entities-package-symbol* '#:info.isoraqathedh.bocproc.entities)

(defun letter->number (letter)
  (let ((maybe-position (position letter +alphabet+)))
    (if maybe-position
        (1+ maybe-position)
        (error "Not a letter: ~s" letter))))

(defun number->letter (number)
  (when (and number (<= 1 number 26))
    (char +alphabet+ (1- number))))

(defun get-timezone ()
  "Retrieves the timezone as set by the configuration variable."
  ;; Ensure that the timezone repository is read.
  (when (zerop (hash-table-count local-time::*location-name->timezone*))
    (reread-timezone-repository))
  ;; Now get the timezone.
  (-> :timezone config find-timezone-by-location-name))

(defun slugify (string)
  (let* ((slug-symbol
           (thread-expr:with-expression-threading (x)
             (string-upcase string)
             (substitute #\- #\Space x)
             (substitute-if-not #\_ #'simple-character-p x)
             (string-trim "-" x)
             (or (find-symbol x *entities-package-symbol*)
                 (intern x *entities-package-symbol*)))))
    (export slug-symbol *entities-package-symbol*)
    slug-symbol))

(defun make-subdirectory-pathname (root directories &key name type)
  "Creates pathname representing a file name or subdirectory in ROOT.

DIRECTORIES is a list of strings that represent folder names,
and NAME and TYPE is as in `make-pathname'."
  (merge-pathnames
   (make-pathname :directory (cons :relative directories)
                  :name name
                  :type type)
   root))
