(in-package #:info.isoraqathedh.bocproc.core)

;;; Page number creation and manipulation
(defun make-page-number-with-series (series)
  (let ((number (make-instance 'page-number :base (slug-symbol series))))
    (setf (numbers number)
          (loop for (specification min . nil) in (page-specification series)
                collect (cons specification min)))
    number))

(define-condition page-out-of-bounds-error (error)
  ((old-page-number :accessor old-page-number
                    :initarg :old
                    :initform nil)
   (attempted-page-number :accessor attempted-page-number
                          :initarg :attempted))
  (:report (lambda (c s)
             (format s "Got a page number of ~a ~
which is out of bounds~:[~; ~:*(was ~a)~]"
                     (attempted-page-number c)
                     (old-page-number c)))))

(defun ensure-valid-page-number (page-number)
  (if (loop with series = (get-stable-entity (base page-number) 'series)
            for (nil . number) in (numbers page-number)
            for (nil min max) in (page-specification series)
            always (<= min number (or max most-positive-fixnum)))
      page-number
      (error 'page-out-of-bounds-error :attempted page-number)))

(defun parse-spec (page-number spec)
  (cons (etypecase spec
          ((or null number) (caar (last (numbers page-number))))
          (cons (car spec))
          (symbol spec))
        (etypecase spec
          ((or null symbol) 1)
          (cons (cdr spec))
          (number spec))))

(defun next (page-number &optional spec)
  (destructuring-bind (spec-symbol . amount) (parse-spec page-number spec)
    (incf (cdr (assoc spec-symbol (numbers page-number))) amount)
    page-number))

(defun previous (page-number &optional spec)
  (destructuring-bind (spec-symbol . amount) (parse-spec page-number spec)
    (decf (cdr (assoc spec-symbol (numbers page-number))) amount)
    page-number))

(defgeneric page-number-list (page-number)
  (:method ((page-number page-number))
    (mapcar #'cdr (numbers page-number))))

(defgeneric (setf page-number-list) (value page-number)
  (:method ((value list) (page-number page-number))
    (loop for num in value
          for old-number in (numbers page-number)
          do (setf (cdr old-number) num))
    value))

;;; Converting page numbers to pages
(defun grab-data (page-number))
(defun available (page))
