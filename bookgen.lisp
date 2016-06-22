(in-package :bocproc-exp)

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
    (format stream "~{~a~^ ~}" (point object))))

(defgeneric generator= (gen1 gen2)
  (:documentation "Determines if two generators are at the same state.

Two generators are equal if they are generating from the same series,
have the same locally-ignored list and are at the same point.")
  (:method ((gen1 page-generator) (gen2 page-generator))
    (and (eql (series gen1) (series gen2))
         )))

(defgeneric next (gen)
  (:documentation "Generate a new page using the generator.

Modifies the generator, returns the new page.")
  (:method ((gen page-generator))
    ()))

(defgeneric prev (gen)
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
