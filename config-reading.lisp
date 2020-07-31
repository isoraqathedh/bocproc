(in-package #:info.isoraqathedh.bocproc.core)

(defvar *config-file*
  (uiop:xdg-config-home "bocproc" "config.lisp")
  "Location of the file that holds the config file.")

(defmacro with-config-file ((stream-var) &body body)
  `(with-open-file (,stream-var *config-file*
                                :external-format :utf-8
                                :direction :input
                                :if-does-not-exist :error)
     ,@body))

(defvar *entities-file*
  (uiop:xdg-data-home "bocproc" "entities.lisp"))

(defmacro with-entities-file ((stream-var &optional (direction :input))
                              &body body)
  `(with-open-file (,stream-var *entities-file*
                                :external-format :utf-8
                                ,@(ecase direction
                                    (:input '(:direction :input
                                              :if-does-not-exist :error))
                                    (:output '(:direction :output
                                               :if-does-not-exist :create
                                               :if-exists :supersede))))
     ,@body))

(defvar *pages-file*
  (uiop:xdg-data-home "bocproc" "pages.lisp"))

(defmacro with-pages-file ((stream-var &optional (direction :input))
                           &body body)
  `(with-open-file (,stream-var *pages-file*
                                :external-format :utf-8
                                ,@(ecase direction
                                    (:input '(:direction :input
                                              :if-does-not-exist :error))
                                    (:output '(:direction :output
                                               :if-does-not-exist :create
                                               :if-exists :supersede))))
     ,@body))

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
  (with-config-file (s)
    (let ((*package* (find-package '#:bpc)))
      (setf *config* (read s))
      (loop for i in (append (config :tags)
                             (config :books))
            do (export (car i))))))

;;; Entity parsing
(defun dump-entities ()
  (uiop:ensure-all-directories-exist (list *entities-file*))
  (with-entities-file (entity-file :output)
    (let ((*package* (find-package 'bpc-entities))
          (*print-readably* t)
          (*print-case* :downcase)
          (sorted-entities (sort (copy-list *stable-entities*) #'entity<)))
      (dolist (entity sorted-entities)
        (prin1 entity entity-file)
        (fresh-line entity-file)))))

(defun load-entities (&optional preservep)
  (with-entities-file (entity-file :input)
    (unless preservep
      (setf *stable-entities* (list)))
    (loop for entity = (let ((*package* (find-package *entities-package-symbol*)))
                         (read entity-file nil :end))
          until (eql entity :end)
          do (progn
               (export (second entity) *entities-package-symbol*)
               (store-stable-entity entity)))
    (setf *stable-entities* (sort *stable-entities* #'entity<))))

;;; Setup
(defun setup ()
  (load-entities)
  (load-config-file))
