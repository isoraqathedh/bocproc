(in-package :bocproc)

(defclass page-generator (book-page)
  ((locally-ignored
    :initform nil
    :initarg :locally-ignored
    :accessor locally-ignored
    :documentation "A list of page numbers that are locally ignored."))
  (:documentation "A generator of page numbers."))

(defmethod print-object ((object page-generator) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a ~{~a~^ ~}" (series object) (page-numbers object))))

(defgeneric point-specificity (gen spec)
  (:documentation "Find the specificity of the point.")
  (:method ((gen page-generator) (spec symbol))
    (nth (position spec (specificities (find-book (series gen)))
                   :key #'first)
         (page-numbers gen))))

(defmethod specificities ((gen page-generator))
  (specificities (find-book (series gen))))

;;; Modifying

;; Out-of-bounds correction
(define-condition spec-out-of-bounds (error)
  ((series
    :reader series
    :initarg :series)
   (page-numbers
    :reader page-numbers
    :initarg :page-numbers))
  (:documentation "Error raised when point is out of bounds.")
  (:report
   (lambda (condition stream)
     (format stream "Point ~s out of bounds given by series ~s,~%which is ~s."
             (page-numbers condition)
             (series condition)
             (mapcar #'cdr (specificities (find-book (series condition))))))))

(defun point-in-bounds-p% (test bounds)
  "Check if TEST is in BOUNDS."
  (loop for point in test
        for (min max) in bounds
        always (or (null point)
                   (if max
                       (<= min point max)
                       (<= min point)))))

(defgeneric point-in-bounds-p (gen)
  (:documentation "Check if a generator is in bounds.")
  (:method ((gen page-generator))
    (point-in-bounds-p% (page-numbers gen) (mapcar #'cdr (specificities gen)))))

(defgeneric point-specificity (gen spec)
  (:documentation "Return the SPEC part of GEN's point.")
  (:method ((gen page-generator) (spec symbol))
    (nth (position spec (specificities gen) :key #'first)
         (page-numbers gen))))

(define-condition uncarriable-list (error)
  ((original :reader original
             :initarg :original)
   (limits :reader limits
           :initarg :limits))
  (:documentation "Error signalled when the list cannot be carried.")
  (:report (lambda (c s)
             (format s "List ~s cannot be coerced into limits ~s."
                     (original c)
                     (limits c)))))

(defun carry-or-borrow (original limits)
  "Clamp the ORIGINAL list of numbers so each number appears between the LIMITS.

Furthermore, the list of numbers are considered to be a number
with a variable base,
which is one more than the difference between the first and second elements
in each sublist of LIMITS.
If clamping occurs, the next value is carried to or borrowed from as needed,
e.g. (3 5 1) ((1 nil) (1 2) (1 3)) yields (5 1 1).

However, some lists are not carriable.
In this case, the function signals an error."
  ;; We need to have alternate versions of subtract, modulo and integer division
  ;; that treat nil as infinity.
  (flet ((-* (subtractand subtractor)
           (when subtractand
             (- subtractand subtractor -1))) ; add 1 back for inclusiveness
         (mod* (number divisor)
           (if divisor
               (mod number divisor)
               number))
         (/* (dividend divisor)
           (if divisor
               (floor (/ dividend divisor))
               0)))
    (let* ((mins       (mapcar #'first limits))
           (maxs       (mapcar #'second limits))
           (ranges     (mapcar #'-* maxs mins))
           (normalised (mapcar #'- original mins)))
      ;; Carrying mechanism
      (loop repeat (length original)    ; Carrying cannot happen
                                        ; more than once per digit
            for remainders = (mapcar #'mod* normalised ranges)
            for carries    = (append
                              (cdr (mapcar #'/* normalised ranges))
                              (list 0))
            for i = (mapcar #'+ remainders carries)
            do (if (equal i normalised)
                   (loop-finish)
                   (setf normalised i))
            finally (setf normalised (mapcar #'+ normalised mins)))
      ;; Sanity checking
      (if (every (lambda (tmin test tmax)
                   (if tmax
                       (<= tmin test tmax)
                       (<= tmin test)))
                 mins normalised maxs)
          normalised
          (restart-case (error 'uncarriable-list
                               :original original
                               :limits limits)
            (do-nothing ()
              :report "Do nothing to the original value."
              original)
            (set-minimum ()
              :report "Set to the smallest available value."
              mins)
            (use-value (value)
              :report "Write in a new value."
              :interactive (lambda ()
                             (format t "Enter a value to use: ")
                             (read))
              value))))))


;; Modifying things.
(defgeneric (setf point-specificity) (value gen spec)
  (:documentation "Set the SPEC part of GEN's point to VALUE.

After setting, perform bounds checking.
Attempt to keep the value in bounds; if it cannot then do not perform changes.
Keeping the value in bounds can mean performing carrying and borrowing
or clamping the values in between the maximum and minimum allowed values.")
  (:method (value (gen page-generator) (spec symbol))
    (let ((old-value (copy-list (page-numbers gen))))
      ;; Actual setting
      (setf (nth (position spec (specificities gen) :key #'first)
                 (page-numbers gen))
            value)
      ;; Bounds checking
      (unless (point-in-bounds-p gen)
        (restart-case (error 'spec-out-of-bounds
                             :page-numbers (page-numbers gen)
                             :series (series gen))
          (revert ()
            :report "Cancel the change."
            (setf (page-numbers gen) old-value))
          (clamp ()
            :report "Constrain value to the limits of the book."
            (setf (page-numbers gen)
                  (mapcar (lambda (vals lims)
                            (destructuring-bind (name low &optional high) lims
                              (declare (ignore name))
                              (if high
                                  (max low (min high vals))
                                  (max low vals))))
                          (page-numbers gen)
                          (specificities gen))))
          (carry ()
            :report "Perform carry/borrow calculations."
            (setf (page-numbers gen)
                  (handler-bind ((uncarriable-list
                                   (lambda (c)
                                     (declare (ignore c))
                                     (use-value old-value))))
                    (carry-or-borrow
                     (page-numbers gen)
                     (mapcar #'cdr (specificities gen))))))))
      ;; Return something useful
      (page-numbers gen))))

;;; Generating

(defgeneric this (gen)
  (:documentation "Return the page that GEN is pointing to.")
  (:method ((gen page-generator))
    (make-page (series gen) (page-numbers gen))))

(defun find-pattern-in-list (wild-pathname)
  "Find a pathname in *exists-list*, or query the disk if it is empty."
  (if *exists-list*
      (find-if (lambda (file)
                 (pathname-match-p file wild-pathname))
               *exists-list*)
      (first (directory wild-pathname))))

(defgeneric find-page (gen &optional spec)
  (:documentation "Find the file represented by point in the generator.

If there is no file, then return nil.
If SPEC is defined, then if there is any one object
that matches GEN up to SPEC then return that.")
  (:method ((page book-page) &optional spec)
    (find-pattern-in-list (format-page page :limit spec)))
  (:method ((gen page-generator)  &optional spec)
    (find-pattern-in-list (format-page (this gen) :limit spec))))

(defgeneric point-status (gen &optional spec)
  (:documentation "Return the status of the current point.

The output can be one of these three:

- :AVAILABLE, which means that this page number is unused
  and ready for filling with a page;
- :IGNORED and an ignore list, which means that this page number is ignored
  at the SPEC level.
- :OCCUPIED and a pathname, which means that this page number already taken,
  specifically by this particular file.

If SPEC is a specificity, then for the purposes of find-page
if there is any one page there then all pages are.")
  (:method ((gen page-generator) &optional spec)
    (let ((found-file (find-page gen spec))
          (ignored-spec
            (find-if
             (lambda (entry)
               (and (string= (symbol-name (series gen))
                             (symbol-name (car entry)))
                    (every #'= (page-numbers gen) (cdr entry))))
             (cdr (assoc :ignore-list-6.1 *config*)))))
      (cond (found-file (values :occupied found-file))
            (ignored-spec (values :ignored ignored-spec))
            (t :available)))))

(defgeneric latest-spec (gen spec)
  (:documentation "Finds the largest occupied value in spec.")
  (:method ((gen page-generator) spec &aux (failsafe 10000))
    (destructuring-bind (min max) (cdr (assoc spec (specificities gen)))
      (if max
          (loop with latest-occupied = min
                for i from min to max
                do (setf (point-specificity gen spec) i)
                   (if (eql (point-status gen spec) :occupied)
                       (setf latest-occupied i))
                finally (setf (point-specificity gen spec) latest-occupied)
                        (return gen))
          (loop with latest-occupied = min
                for i from min to failsafe
                do (setf (point-specificity gen spec) i)
                   (case (point-status gen spec)
                     (:occupied
                      (setf latest-occupied i))
                     (:available
                      (setf (point-specificity gen spec) latest-occupied)
                      (return gen))))))))

(defgeneric latest (gen)
  (:documentation "Find the largest occupied value.")
  (:method ((gen page-generator))
    (loop for i in (specificities gen)
          do (latest-spec gen (car i))
          finally (return gen))))

(defgeneric next (gen spec)
  (:documentation "Generate a new page using the generator.

Modifies the generator, returns the new page.")
  (:method ((gen page-generator) (spec symbol))
    (loop do (handler-bind ((spec-out-of-bounds
                              (lambda (condition)
                                (declare (ignore condition))
                                (invoke-restart 'carry))))
               (incf (point-specificity gen spec)))
          until (eql (point-status gen) :available)
          finally (return gen))))

(defgeneric prev (gen spec)
  (:documentation "Goes back one page on the generator.

Defined so that (next (prev gen)) or (prev (next gen))
should be GENERATOR= to gen.")
  (:method ((gen page-generator) (spec symbol))
    (loop do (handler-bind ((spec-out-of-bounds
                              (lambda (condition)
                                (declare (ignore condition))
                                (invoke-restart 'carry))))
               (decf (point-specificity gen spec)))
          until (eql (point-status gen) :available)
          finally (return gen))))

(defgeneric reset (gen)
  (:documentation "Reset the generator.

This means that the point is moved to the closest page
that the ignore list will allow.")
  (:method ((gen page-generator))
    (setf (locally-ignored gen) ()
          (page-numbers gen) (mapcar #'second (specificities gen)))))

(defun make-generator (name &optional jump-to-latest)
  "Make a generator.

If JUMP-TO-LATEST is true,
then set the point of the generator to the latest page."
  (let* ((corresponding-book (find-book name))
         (gen
           (make-instance
            'page-generator
            :series name
            :page-numbers (mapcar #'second (specificities corresponding-book))
            :format (book-format corresponding-book))))
    (when jump-to-latest
      (latest gen))
    gen))
