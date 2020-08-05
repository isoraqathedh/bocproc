(in-package #:info.isoraqathedh.bocproc.core)

;;; Page number
(defclass page-number ()
  ((base :accessor base
         :initarg :base)
   (number :initform 0
           :accessor page-number)))

(defun make-page-number-with-series (series)
  (make-instance 'page-number :base series))

(defmethod page-specification ((number page-number))
  (page-specification (base number)))

(defun page-place-values (page-spec)
  (reverse
   (loop with acc = 1
         for (nil min . max) in (reverse page-spec)
         collect acc
         if max do (setf acc (* acc (- (first max) min -1))))))

(defun encode-page-number (number-list series)
  (loop with specs = (page-specification series)
        for place-value in (page-place-values specs)
        for (nil min . nil) in specs
        for num in number-list
        sum (* place-value (- num min))))

(defun decode-page-number (single-number series)
  (loop with specs = (page-specification series)
        for place-value in (page-place-values specs)
        for (nil min . nil) in specs
        for (quotient remainder)
        = (multiple-value-list (floor single-number place-value))
        then (multiple-value-list (floor remainder place-value))
        collect (+ quotient min)))

(defgeneric numbers (object)
  (:method ((object page-number))
    (decode-page-number (page-number object)
                        (base object))))

(defmethod print-object ((object page-number) stream)
  (if *print-readably*
      (prin1 (listify object) stream)
      (print-unreadable-object (object stream :type t)
        (format stream "~s ~{~s~^ ~}" (slug-symbol (base object))
                (numbers object)))))

(defmethod listify append ((object page-number))
  (cons (slug-symbol (base object))
        (mapcar (lambda (spec num)
                  (cons (car spec) num))
                (page-specification object)
                (decode-page-number
                 (page-number object)
                 (base object)))))

;;; URL creation
(defgeneric url (page-number)
  (:method ((page-number page-number))
    (format nil "/~(~a~)/~{~a~^/~}"
            (symbol-name (slug-symbol (base page-number)))
            (decode-page-number (page-number page-number)
                                (base page-number)))))

(defun page-number-from-url (url)
  (destructuring-bind (empty series &rest page-numbers)
      (split-sequence:split-sequence #\/ url)
    (unless (string-equal empty "")
      (error "Path not absolute"))
    (let ((instance
            (make-instance 'page-number
                           :base (get-stable-entity
                                  (or
                                   (find-symbol
                                    (string-upcase series)
                                    '#:bpc-entities)
                                   (error "Name ~a does not indicate a series."
                                          series))))))
      (setf (page-number instance)
            (mapcar #'parse-integer page-numbers))
      instance)))

;;; Navigation
(defmethod (setf page-number) ((value list) (page-number page-number))
  (setf (page-number page-number)
        (encode-page-number value (base page-number))))

(defgeneric next-page (page spec &optional alt-spec)
  (:method ((page page-number) (increment number) &optional spec-level)
    (next-page page (cons increment spec-level)))
  (:method ((page page-number) (spec-level symbol) &optional (number 1))
    (next-page page (cons number spec-level)))
  (:method ((page page-number) (spec cons) &optional _unused)
    (declare (ignore _unused))
    (let ((page-spec (page-specification page)))
     (incf (page-number page)
           (* (car spec)
              (nth (position (cdr spec) page-spec :key #'car)
                   (page-place-values page-spec))))
      page)))
