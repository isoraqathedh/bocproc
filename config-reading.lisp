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

(defparameter *config* ()
  "Configuration tree.")

;;; Config file parsing
(defun config (key)
  "Retrieve KEY from the config."
  (humbler::aget key *config*))

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
    (let ((*package* (find-package 'bpc-entities))
          (*print-readably* t)
          (*print-case* :downcase)
          (sorted-entities (sort (copy-list *stable-entities*) #'entity<)))
      (dolist (entity sorted-entities)
        (prin1 entity entity-file)
        (fresh-line entity-file)))))

(defun load-entities (&optional preservep)
  (with-open-file (entity-file *entities-file* :direction :input
                                               :external-format :utf-8
                                               :if-does-not-exist :error)
    (unless preservep
      (setf *stable-entities* (list)))
    (loop for entity = (read entity-file nil :end)
          until (eql entity :end)
          do (progn
               (export (second entity) *entities-package-symbol*)
               (store-stable-entity entity)))))
