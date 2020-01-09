(in-package #:info.isoraqathedh.bocproc.core)

(defvar *config-file*
  (uiop:xdg-config-home "bocproc" "config.lisp")
  "Location of the file that holds the config file.")

(defvar *entities-file*
  (uiop:xdg-data-home "bocproc" "entities.lisp"))

(defparameter *books-location*
  (make-subdirectory-pathname
   (uiop/common-lisp:user-homedir-pathname) '("Documents" "My Scans"))
  "Root directory of the scan destinations.")

(defvar +alphabet+ "abcdefghijklmnopqrstuvwxyz"
  ;; It's actually a constant
  ;; but using defvar makes redefinition warnings makes the errors go away.
  "The alphabet as required in the subpage specifications.")

(defparameter *config* ()
  "Configuration tree.")

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

;;; Config file parsing
(defun config (key)
  "Retrieve KEY from the config."
  (aget key *config*))

(defun token (token-key)
  "Retrieve the token named TOKEN-KEY."
  (getf (config :tokens) token-key))

(defun load-config-file ()
  "Read the config file into *config*, and set up external variables."
  (with-open-file (s *config-file* :external-format :utf-8)
    (let ((*package* (find-package '#:bpc)))
      (setf *config* (read s))
      (loop for i in (append (config :tags)
                             (config :books))
            do (export (car i))))))

;;; Entity parsing
(defun dump-entities ()
  (uiop:ensure-all-directories-exist (list *entities-file*))
  (with-open-file (entity-file *entities-file* :direction :output
                                               :external-format :utf-8
                                               :if-exists :supersede
                                               :if-does-not-exist :create)
    (let ((*package* (find-package 'bpc-entities)))
      (prin1 *stable-entities* entity-file))))

(defun read-entities (&optional preservep)
  (with-open-file (entity-file *entities-file* :direction :input
                                               :external-format :utf-8
                                               :if-does-not-exist :error)
    (unless preservep
      (setf *stable-entities* (list)))
    (dolist (entity (let ((*package* (find-package 'bpc-entities)))
                      (read entity-file)))
      (store-stable-entity entity))))
