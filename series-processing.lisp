(in-package #:info.isoraqathedh.bocproc.core)

;;; Page number creation and manipulation
(defun make-page-number-with-series (series)
  (let ((number (make-instance 'page-number :base (slug-symbol series))))
    (setf (numbers number)
          (loop for (specification min . nil) in (page-specification series)
                collect (cons specification min)))
    number))

(defun next (page-number &optional spec))
(defun previous (page-number &optional spec))

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

;;; Converting page numbers to pages
(defun fill (page-number))
(defun available (page))
