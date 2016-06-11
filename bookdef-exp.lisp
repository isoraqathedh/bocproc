(in-package :bocproc-exp)

(defparameter *series-list* ()
  "List of series that are currently on the system.")

(defclass book-series ()
  ((series
    :type symbol
    :initarg :series
    :documentation "Series identifier. A keyword."
    :reader series)
   (specificities
    :initarg :specificities
    :documentation #.(concatenate 'string
                                  "An n×3 array"
                                  " with the names of the specificities"
                                  " and also their cutoffs. "
                                  " The rows are symbol, min, max.")
    :reader specificities)
   (format
    :initarg :format
    :documentation "The format of the filenames of this series."
    :reader book-format))
  (:documentation "Class for defining book series."))

(defmethod print-object ((object book-series) stream)
  (with-slots (series) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a" series))))

(defclass book-page (book-series)
  ((page-numbers :initarg :page-numbers
                 :initform (vector)
                 :type (vector number)
                 :accessor page-numbers))
  (:documentation "A manifestation of a book-series,
with a definite page number."))

(defun define-book% (name format specificities)
  (push (make-instance
         'book-series
         :series name
         :format format
         :specificities
         (make-array
          (list (length specificities) 3)
          :initial-contents
          (mapcar (lambda (list)
                    (loop repeat 3
                          for l = list then (cdr l)
                          collect (car l)))
                  specificities))) *series-list*))

(defmacro define-book (name specificities &body format)
  "Defines a book series."
  `(define-book% ,name ',format ',specificities))

(define-book :boc ((:book 1)
                   (:page 1 99)
                   (:subpage 1 26))
  "Book of Conworlds/Book " :book "/"
  (:page :pad 2) (:subpage :type :letter) "-"
  :genre "-" :title ".jpg")

(define-book :purple ((:book 1)
                      (:page 1 99)
                      (:subpage 1 26))
  "Book of Conworlds/Book p" :book "/"
  (:page :pad 2) (:subpage :type :letter) "-"
  :genre "-" :title ".jpg")

(define-book :nboc ((:page 1 9999))
  "Book of Conworlds/Non-BoC/"
  (:date :year "-" (:month 2) " (" :short-month ")") "/"
  "SCAN" (:page :pad 4) ".jpg")

(define-book :uncategorised ((:page 1 9999))
  "Unsorted By Date/" (:date :year "-" (:month 2) " (" :short-month ")") "/"
  "SCAN" (:page :pad 4) ".jpg")

(defun find-book (name)
  "Finds the book series named NAME from the list"
  (find name *series-list* :key #'series))

(defun undefine-book (name)
  "Removes the book series named NAME from the list."
  (find name *series-list* :key #'series))

(defun make-page (name &rest page-numbers)
  "Creates a page that is in the series NAME, with the specified PAGE-NUMBERS."
  (let ((found-page (find-book name)))
    (make-instance
     'book-page
     :format (book-format found-page)
     :specificities (specificities found-page)
     :series (series found-page)
     :page-numbers
     (make-array (-> found-page specificities array-dimensions first list)
                  :initial-contents page-numbers))))
