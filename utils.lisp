(in-package #:info.isoraqathedh.bocproc.core)

(defvar +alphabet+ "abcdefghijklmnopqrstuvwxyz"
  ;; It's actually a constant
  ;; but using defvar makes redefinition warnings makes the errors go away.
  "The alphabet as required in the subpage specifications.")

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
  (let* ((slug-package '#:info.isoraqathedh.bocproc.entities)
         (slug-symbol
           (thread-expr:with-expression-threading (x)
             (string-upcase string)
             (substitute #\- #\Space x)
             (substitute-if-not #\_ #'simple-character-p x)
             (string-trim "-" x)
             (or (find-symbol x slug-package)
                 (intern x slug-package)))))
    (export slug-symbol slug-package)
    slug-symbol))
