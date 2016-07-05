(in-package :bocproc)

(defclass page-generator ()
  ((series
    :initform nil
    :initarg :series
    :reader series
    :documentation "The series from which the page numbers are drawn from.")
   (locally-ignored
    :initform nil
    :initarg :locally-ignored
    :accessor locally-ignored
    :documentation "A list of page numbers that are locally ignored.")
   (point
    :initform nil
    :initarg :point
    :accessor point
    :documentation "The current page of the generator."))
  (:documentation "A generator of page numbers."))

(defmethod print-object ((object page-generator) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a ~{~a~^ ~}" (series object) (point object))))

(defgeneric generator= (gen1 gen2)
  (:documentation "Determines if two generators are at the same state.

Two generators are equal if they are generating from the same series,
have the same locally-ignored list and are at the same point.")
  (:method ((gen1 page-generator) (gen2 page-generator))
    (and (eql (series gen1) (series gen2))
         )))

(defgeneric point-specificity (gen spec)
  (:documentation "Find the specificity of the point.")
  (:method ((gen page-generator) (spec symbol))
    (nth (position spec (specificities (find-book (series gen)))
                   :key #'first)
         (point gen))))

(defmethod specificities ((gen page-generator))
  (specificities (find-book (series gen))))

;;; Modifying

(define-condition spec-out-of-bounds (error)
  ((series
    :reader series
    :initarg :series)
   (point
    :reader point
    :initarg :point))
  (:documentation "Error raised when point is out of bounds.")
  (:report
   (lambda (condition stream)
     (format stream "Point ~s out of bounds given by series ~s,~%which is ~s."
             (point condition)
             (series condition)
             (mapcar #'cdr (specificities (find-book (series condition))))))))

(defgeneric point-in-bounds-p (gen)
  (:documentation "Check if a generator is in bounds.")
  (:method ((gen page-generator))
    (loop for point in (point gen)
          for (nil min max) in (specificities (find-book (series gen)))
          always (if max
                     (<= min point max)
                     (<= min point)))))

(defgeneric point-specificity (gen spec)
  (:documentation "Return the SPEC part of GEN's point.")
  (:method ((gen page-generator) (spec symbol))
    (nth (position spec (specificities gen) :key #'first)
         (point gen))))

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
    (let* ((mins   (mapcar #'first limits))
           (maxs   (mapcar #'second limits))
           (ranges (mapcar #'-* maxs mins))
           (working-copy (copy-list original)))
      (dotimes (i (length original))
        (setf working-copy
              (let* ((norm-origs (mapcar #'- working-copy mins))
                     (remainders (mapcar #'mod* norm-origs ranges))
                     (carries    (append
                                  (cdr (mapcar #'/* norm-origs ranges))
                                  (list 0))))
                (mapcar #'+ remainders carries mins))))
      (if (every (lambda (tmin test tmax)
                   (if tmax
                       (<= tmin test tmax)
                       (<= tmin test)))
                 mins working-copy maxs)
          working-copy
          (error "List cannot be carried.")))))

(defgeneric (setf point-specificity) (value gen spec)
  (:documentation "Set the SPEC part of GEN's point to VALUE.")
  (:method (value (gen page-generator) (spec symbol))
    (let ((old-value (point gen))
          (new-value (setf (nth (position spec (specificities gen) :key #'first)
                                (point gen))
                           value)))
      (unless (point-in-bounds-p gen)
        (restart-case (error 'spec-out-of-bounds :point (point gen)
                                                 :series (series gen))
          (revert ()
            :report "Cancel the change."
            (setf (point gen) old-value))
          (carry ()
            :report "Perform carry/borrow calculations."
            (setf (point gen)
                  (carry-or-borrow new-value
                                   (mapcar #'cdr (specificities gen))))))))))

;;; Generating

(defgeneric this (gen)
  (:documentation "Return the page that GEN is pointing to.")
  (:method ((gen page-generator))
    (make-page (series gen) (point gen))))

(defgeneric point-status (gen)
  (:documentation "Return the status of the current point.

The output can be one of these three:

- :AVAILABLE, which means that this page number is unused
  and ready for filling with a page;
- :IGNORED and an ignore list, which means that this page number is ignored
  at the SPEC level.
- :OCCUPIED and a pathname, which means that this page number already taken,
  specifically by this particular file.")
  (:method ((gen page-generator))
    (let ((found-file
            (directory
             (format-page
              (make-page (series gen) (point gen)) :unknown-values :glob)))
          (ignored-spec
            (find-if
             (lambda (entry)
               (and (string= (symbol-name (series gen))
                             (symbol-name (car entry)))
                    (every #'= (point gen) (cdr entry))))
             (cdr (assoc :ignore-list-6.1 *config*)))))
      (cond (found-file (values :occupied (first found-file)))
            (ignored-spec (values :ignored ignored-spec))
            (t :available)))))

(defgeneric next (gen spec)
  (:documentation "Generate a new page using the generator.

Modifies the generator, returns the new page.")
  (:method ((gen page-generator) (spec symbol))
    ()))

(defgeneric prev (gen spec)
  (:documentation "Goes back one page on the generator.

Defined so that (next (prev gen)) or (prev (next gen))
should be GENERATOR= to gen."))

(defgeneric reset (gen)
  (:documentation "Reset the generator.

This means that the point is moved to the closest page
that the ignore list will allow."))

(defun make-generator (name)
  "Make a generator."
  (make-instance 'page-generator
                 :series name
                 :point (mapcar #'second (specificities (find-book name)))))
