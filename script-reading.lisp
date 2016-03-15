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
   (current-page :initform (make-hash-table :test #'equal)
                 :accessor current-page)
   (exiftool-file :accessor exiftool-file
                  :documentation "Exiftool ARGFILE."))
  (:documentation "An object that represents the state of the processor."))

(defmethod initialize-instance :after ((instance bocproc-state)
                                       &key &allow-other-keys)
  (setf (exiftool-file instance)
        (asdf:system-relative-pathname
         :bocproc
         (local-time:format-timestring
          nil (local-time:now) :format '((:month 2) (:day 2) (:hour 2)))
         :type "txt")))

(defgeneric get-current-page (state series)
  (:documentation "Gets the latest accessed page number in the series.")
  (:method ((state bocproc-state) (series string))
    (gethash series (current-page state))))

(defgeneric (setf get-current-page) (value state series)
  (:documentation "SETF function for get-current-page.")
  (:method (value (state bocproc-state) (series string))
    (setf (gethash series (current-page state)) value)))

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

(defun ensure-keyword (symbol)
  "Coerces SYMBOL into a keyword."
  (or (find-symbol (symbol-name symbol) "KEYWORD")
      (intern (symbol-name symbol) "KEYWORD")))

(defun symbol-name-assoc-cdr (keyform alist)
  "Like ASSOC, but compares only symbol names (case-insensitively)."
  (cdr (assoc keyform alist :test #'string-equal :key #'symbol-name)))

(defun %process-file (file &rest options
                      &key series paging-behaviour &allow-other-keys)
  "Constructs and forms a wandering-page from the arguments."
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
        (restart-case (error 'state-not-there)
          (make-state (version-numbers)
            :report "Set up a state."
            :interactive (lambda ()
                           (format *query-io* "A list of version numbers: ")
                           (let ((test-version (read)))
                             (assert (every #'numberp test-version)
                                     (test-version)
                                     "Must be a list of numbers.")
                             test-version))
            (set-state version-numbers))))))

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

(defmacro bpc:process-file (file &body options)
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

;;; Processing facilities
(defgeneric move-pages (pages-to-move)
  (:documentation "Performs a page move to an automatically determined path.")
  (:method ((pages-to-move bocproc-state))
    (dolist (page (files-to-process pages-to-move))
      (let ((corresponding-page
              (construct-book-object (paging-series page)
                                     (paging-behaviour page)
                                     (get-current-page pages-to-move
                                                       (paging-series page)))))
        (setf (get-current-page pages-to-move (paging-series page))
              corresponding-page)
        (rename-file (file page)
                     (get-path-with-metadata corresponding-page page))))))

(defgeneric run-exiftool (pages-to-move)
  (:documentation "Dumps all arguments to a file and run exiftool with it.")
  (:method ((pages-to-move bocproc-state))
    (let ((associated-file (exiftool-file pages-to-move)))
      (loop for page in (files-to-process pages-to-move)
            for newp = t then nil
            do (dump-exiftool-args associated-file page newp))
      (uiop:run-program `("exiftool" "-@" ,(namestring associated-file)))
      (delete-file associated-file))))

;;; Finally, load files
(defun load-script (bpc-location &optional (new-state-p t))
  "Loads the script from the file."
  (let ((*package* (find-package :bpc))
        (*state* (bpc:version 6))
        (*read-eval* nil))
    (handler-bind ((state-already-there (if new-state-p
                                            (make-restart-lambda 'continue)
                                            (constantly nil))))
      (load bpc-location))
    (setf (files-to-process *state*) (reverse (files-to-process *state*)))
    ;; handle stuff here
    (run-exiftool *state*)
    (move-pages *state*)))
