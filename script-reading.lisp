;;;; Reading scripts

#| This file reads in a script file from some directory
and interprets it as commands. |#

(in-package :bocproc)

;;; Functions for processing BPC files.
;;; = = = = = = = = = = = = = = = = = =

;;; Processor states.
(defclass bocproc-state ()
  ((version :initarg :version
            :initform nil
            :reader state-version)
   (files-to-process :initform ()
                     :accessor files-to-process)
   (current-page :accessor current-page))
  (:documentation "An object that represents the state of the processor."))

(defmethod print-object ((object bocproc-state) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "v.~{~a~^.~}; ~a file~:p"
            (state-version object)
            (length (files-to-process object)))))

(defparameter *state* nil
  "An object holds the state of the processor.")

(define-condition state-already-there (error) ()
  (:documentation "Error indicating that there already is a processor."))

(define-condition state-not-there (error) ()
  (:documentation
   "Error indicating that there is no processor currently active."))

(defun make-restart-lambda (restart-name)
  "Creates a lambda function that that takes in one argument, ignores it,
and invokes the restart RESTART-NAME."
  (lambda (condition)
    (declare (ignore condition))
    (invoke-restart restart-name)))

;;; Utility functions for processing the script, not exported
(defun set-state (version-numbers)
  "Sets *state* to be a fresh new processor."
  (setf *state*
        (make-instance 'bocproc-state :version version-numbers)))

(defmethod marry-page ((wandering-page wandering-page)
                       (state bocproc-state))
  (make-instance 'married-page
                 :metadata wandering-page
                 :page-slot (construct-book-object
                             (paging-series wandering-page)
                             (paging-behaviour wandering-page)
                             (current-page state))))

(defun ensure-keyword (symbol)
  "Coerces SYMBOL into a keyword."
  (or (find-symbol (symbol-name symbol) "KEYWORD")
      (intern (symbol-name symbol) "KEYWORD")))

(defun symbol-name-assoc-cdr (keyform alist)
  "Like ASSOC, but compares only symbol names (case-insensitively)."
  (cdr (assoc keyform alist :test #'string-equal :key #'symbol-name)))

(defun %process-file (file &rest options
                      &key series paging-behaviour &allow-other-keys)
  "Constructs and forms a "
  (let ((instance (make-instance 'wandering-page :file file)))
    (setf (paging-series instance) series
          (paging-behaviour instance) paging-behaviour)
    (loop for parameter in *processing-parameters*
          for parameter-args = (getf options parameter)
          when parameter-args
          do (setf (get-parameter instance parameter)
                   (getf options parameter)))
    (if *state*
        (push instance (files-to-process *state*))
        (error 'state-not-there))))

;;; The functions that the processor understands.
(defun bpc:version (&rest version-numbers)
  "Sets up the parameters for processing the following script page."
  (if (and *state* (state-version *state*))
      (restart-case (error 'state-already-there)
        (continue ()
          :report "Ignore the form and do nothing else.")
        (set-value ()
          :report "Make a new state and overwrite the old one."
          (set-state version-numbers)))
      (set-state version-numbers)))

(defmacro process-file (file &body options)
  "Thin wrapper around `%process-file',
 adding in quotes and breaking out lists."
  (destructuring-bind (series &rest paging-behaviour)
      (symbol-name-assoc-cdr :paging options)
    `(%process-file
      ,file
      :series ,series
      :paging-behaviour ',paging-behaviour
      ,@(loop for parameter in *processing-parameters*
              for parameter-args = (symbol-name-assoc-cdr parameter options)
              when parameter-args
              append (list parameter
                           (if (get parameter :compound-value)
                               `',parameter-args
                               `',(car parameter-args)))))))

;;; Finally, load files
(defun load-script (bpc-location)
  "Loads the script from the file."
  (let ((*package* (find-package :bpc)))
    (load boc-location)))
