;;;; Reading scripts

#| This file reads in a script file from some directory
and interprets it as commands. |#

(in-package :bocproc)

;;; Functions for processing BPC files.
;;; = = = = = = = = = = = = = = = = = =

;;; Processor states.
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

;;; Utility functions for processing the script, not exported
(defun set-state (version-numbers)
  "Sets *state* to be a fresh new processor."
  (setf *state* (make-instance 'bocproc-state :version version-numbers)))

(defun ensure-generator (name state)
  "Return a generator for that state, making one if necessary.

The generator will always be set to be at the latest page."
  (or (find name (generators state) :key #'series)
      (let ((new-generator (make-generator name t)))
        (push new-generator (generators state))
        new-generator)))

(defun ensure-state ()
  "Ensures that a state is in *state*. Errors otherwise."
  (unless *state*
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
        (set-state version-numbers)))))

(defun %process-file (file &rest options
                      &key series paging-behaviour &allow-other-keys)
  "Constructs and forms a wandering-page from the arguments."
  (ensure-state)
  (let* (;; Find a generator
         (generator (ensure-generator series *state*))
         ;; Make the instance
         (instance
           (cond ((eql (car paging-behaviour) :next)
                  (next generator (second paging-behaviour))
                  (this generator))
                 (t
                  (make-page
                   series
                   (loop for (spec nil nil) in (specificities generator)
                         for page-number = (getf paging-behaviour spec)
                         if (numberp page-number)
                         collect page-number
                         else do (error "Spec ~s is not a number: ~s"
                                        spec page-number)))))))
    ;; Assign the file
    (setf (get-page-property instance :file) (pathname file)
          (get-page-property instance :date-of-creation) (now))
    (unless (probe-file (pathname file))
      (cerror "File not found: ~s" file))
    ;; Grab properties
    (loop for parameter in *processing-parameters*
          for parameter-args = (or (getf options parameter)
                                   (get parameter 'default))
          when parameter-args
          do (setf (get-page-property instance parameter)
                   parameter-args))
    (set-genre instance)
    (push instance (files-to-process *state*))))

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

(defun bpc:next (series spec &optional (amount 1))
  "Directly increment the specified generator by AMOUNT times."
  (loop repeat amount
        do (next (ensure-generator series *state*) spec)))

(defmacro bpc:process-file (file &body options)
  "Thin wrapper around `%process-file',
 adding in quotes and breaking out lists."
  (destructuring-bind (series &rest paging-behaviour)
      (symbol-name-assoc-cdr :paging options)
    `(%process-file
      ,file
      :series (find-symbol (symbol-name ',series) "BPC")
      :paging-behaviour ',paging-behaviour
      ,@(loop for parameter in *processing-parameters*
              for parameter-args = (symbol-name-assoc-cdr parameter options)
              when parameter-args
              append (list parameter
                           (if (get parameter 'compound-value)
                               `',parameter-args
                               `',(car parameter-args)))))))

(defmacro bpc:multi-post ((&optional caption tags) &body names)
  "Declare that the following names are going to be posted into one large post."
  `(push (files-to-process *state*)
         (construct-multi-post
          (mapcar (lambda (name)
                    (find name (files-to-process *state*)
                          :key (lambda (page)
                                 (and (typep page 'book-page)
                                      (get-page-property page :name)))))
                  ,names)
          ,caption
          ,tags)))

;;; Processing facilities
(defmacro define-action-all (name (&optional (arg (gensym)))
                             &body body &aux doc)
  "Specifies what to do with the completely parsed bocproc form."
  ;; Docstring handling
  (when (stringp (first body))
    (setf doc (pop body)))
  ;; Template
  `(defgeneric ,name (,arg)
     ,@(when doc `((:documentation ,doc)))
     (:method ((,arg bocproc-state))
       ,@body)))

(defmacro define-action (name (page &optional (arg (gensym)))
                         &body body &aux doc)
  "Specify an action to do for every page in a completely parsed bocproc form."
  ;; Docstring handling
  (when (stringp (first body))
    (setf doc (pop body)))
  `(define-action-all ,name (,arg)
     ,doc
     (dolist (,page (files-to-process ,arg))
       ,@body)))

(define-action rotate-image (page pages-to-rotate)
  "Rotates an image based on its rotation parameter."
  (when (get-page-property page :rotate)
    (uiop:run-program
     (list "mogrify" "-rotate"
           (format nil "~d" (get-page-property page :rotate))
           (namestring (truename (get-page-property page :file)))))
    (when (verbosep pages-to-rotate)
      (format t "Rotated image ~s~%" (get-page-property page :file)))))

(define-action move-pages (page pages-to-move)
  "Performs a page move to an automatically determined path."
  (let* ((old-name (truename (namestring (get-page-property page :file))))
         (new-name (format-page page :unknown-values :error)))
    (when (verbosep pages-to-move)
      (format t "Moving ~s to ~s~%" old-name new-name))
    (ensure-directories-exist new-name :verbose (verbosep pages-to-move))
    (rename-file old-name new-name)
    (push new-name *exists-list*)))

(define-action-all run-exiftool (pages-to-move)
  "Dumps all arguments to a string and run exiftool with it."
  (with-input-from-string
      (send-stream
       (with-output-to-string (receive-stream)
         (dolist (page (files-to-process pages-to-move))
           (dump-exiftool-args receive-stream page))))
    (uiop:run-program '("exiftool" "-@" "-") :input send-stream)))

(define-action post-to-tumblr (page pages-to-move)
  "Post all the marked images to Tumblr."
  (let ((resulting-post (%post-to-tumblr page)))
    (when (and resulting-post
               (verbosep pages-to-move))
      (format t "Posted ~s to Tumblr with URL ~s~%"
              (get-page-property page :file)
              (get-page-property page :image-url)))))

(define-action-all dump-URLs (pages-to-move)
  "Dump the URLs that are posted on Tumblr to some file."
  (delete-file (config :dump-file))
  (with-open-file (dump-file (config :dump-file)
                             :direction :output
                             :external-format :utf-8
                             :if-does-not-exist :create
                             :if-exists :overwrite)
    (format dump-file "Today: ~{~a~^ | ~}"
            (remove nil
                    (mapcar (lambda (page)
                       (get-page-property page :image-url))
                     (files-to-process pages-to-move))))))

;;; Finally, load files
(defun read-script (bpc-location &optional (new-state-p t))
  "Read the script from the file, but do not do any actions just yet."
  (handler-bind ((state-already-there (if new-state-p
                                          (continue)
                                          (constantly nil))))
    (let ((*package* (find-package :bpc))
          (*state* nil)
          (*read-eval* nil))
      (load (or bpc-location *standard-input*))
      (setf (files-to-process *state*) (reverse (files-to-process *state*)))
      *state*)))

(defun load-script (bpc-location &optional (new-state-p t))
  "Loads the script from the file."
  (let ((bocproc-state (read-script bpc-location new-state-p)))
    ;; handle stuff here
    (dolist (action (list #'rotate-image
                          #'run-exiftool
                          #'post-to-tumblr
                          #'dump-URLs
                          #'move-pages))
      (funcall action bocproc-state))))

(defun main (args)
  "Entry point to bocproc."
  ;; This should only be run non-interactively
  ;; (i.e. as a shell script entry point.)
  ;; If you run this interactively, you might break Lisp
  ;; Because it automatically kills the image if *any* errors show up.
  (handler-bind ((error (lambda (condition)
                          (uiop:die 1 "Error: ~a" condition))))
    (setup)
    (load-script
     (or (find "bpc" args :key #'pathname-type :test #'string-equal)
         *standard-input*))))
