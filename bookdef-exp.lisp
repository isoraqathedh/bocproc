(in-package :bocproc-exp)

(defparameter *series-list* ()
  "List of series that are currently on the system.")

(defparameter *directory-list* ()
  "List of files currently in the base directory.")

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
                 :accessor page-numbers)
   (properties :initarg properties
               :initform (make-hash-table)
               :accessor properties))
  (:documentation "A manifestation of a book-series,
with a definite page number."))

(defmethod print-object ((object book-page) stream)
  (with-accessors ((properties properties)) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a ~a" (series object) (page-numbers object))
      (loop for k being the hash-keys of properties
            for v being the hash-values of properties
            do (format stream " ~a ~a" k v)))))

(defgeneric get-page-property (object name)
  (:documentation "Retrieves the page property from a page object.")
  (:method ((object book-page) name)
    (gethash name (properties object))))

(defgeneric (setf get-page-property) (value object name)
  (:documentation "Sets the page property from a page object to VALUE.")
  (:method (value (object book-page) name)
    (setf (gethash name (properties object)) value)))

(defun define-book% (name format specificities)
  (push (make-instance
         'book-series
         :series name
         :format format
         :specificities
         (mapcar (lambda (list)
                   (loop repeat 3
                         for l = list then (cdr l)
                         collect (car l)))
                 specificities)) *series-list*))

(defmacro define-book (name specificities &body format)
  "Defines a book series."
  `(define-book% ',name ',format ',specificities))

(define-book :boc ((:book 1)
                   (:page 1 99)
                   (:subpage 1 26))
  "Book of Conworlds/Book " :book "/"
  (:page :pad 2) (:subpage :type :letter) "-"
  :genre "-" :title ".jpg")

'(:sequence
  :start-anchor
  (:non-greedy-repetition 0 1 (namestring bocproc::*books-location*))
  "Book of Conworlds/Book "
  (:register (:non-greedy-repetition 1 nil :digit-class))
  #\/
  (:register (:non-greedy-repetition 2 nil :digit-class))
  (:register (:non-greedy-repetition 1 nil :word-char-class))
  #\-
  (:register (:non-greedy-repetition 0 nil (:inverted-char-class #\Space)))
  #\-
  (:register (:greedy-repetition 0 nil :everything))
  ".jpg"
  :end-anchor)

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
  (delete name *series-list* :key #'series))

(defun make-page (name &rest page-numbers)
  "Creates a page that is in the series NAME, with the specified PAGE-NUMBERS."
  (let ((found-page (if (symbolp name)
                        (find-book name)
                        name)))
    (make-instance
     'book-page
     :format (book-format found-page)
     :specificities (specificities found-page)
     :series (series found-page)
     :page-numbers (if (= (length page-numbers)
                          (length (specificities found-page)))
                       page-numbers
                       (error "Not enough page numbers for the book ~a"
                              (series found-page))))))

(defun normalise-book-format (page fragment &key unknown-values)
  "Normalise and compute formatting options."
  (flet ((handle-missing-value ()
           (case unknown-values
             ((nil) (return-from normalise-book-format))
             (:glob (return-from normalise-book-format "*"))
             (:error (error
                      "Cannot compose file name, missing component ~a"
                      fragment)))))
    (etypecase fragment
      (string fragment)
      (character (string fragment))
      (symbol (normalise-book-format page (list fragment)
                                     :unknown-values unknown-values))
      (list
       (cond
         ((eql (car fragment) :date)
          (local-time:format-timestring nil (local-time:now)
                                        :format (cdr fragment)
                                        :timezone (bocproc::get-timezone)))
         ((find (car fragment) (specificities page) :key #'first)
          (destructuring-bind (spec &key (pad 0) (type :number)) fragment
            (let ((page-number
                    (nth (position spec (specificities page) :key #'first)
                         (page-numbers page))))
              (unless page-number
                (handle-missing-value))
              (case type
                (:letter
                 (format nil "~c" (bocproc::number->letter page-number)))
                (:number
                 (format nil "~?"
                         (format nil "~~~d,'0d" pad)
                         (list page-number)))))))
         (t (let ((property-value (get-page-property page (car fragment))))
              (unless property-value
                (handle-missing-value))
              (format nil "~a" property-value))))))))

(defgeneric format-page (page &key unknown-values)
  (:documentation "Takes a PAGE and derives its filename from it.
If there are any parts that are not specified,
UNKNOWN-VALUES will control what happens next:

- NIL, the default, simply makes the function return nil.
- :ERROR causes an error to be raised.
- :GLOB replaces any unknown values with a globbing *.")
  (:method ((page book-page) &key unknown-values)
    (apply #'concatenate 'string
           (namestring bocproc::*books-location*)
           (loop for fragment in (book-format page)
                 if (normalise-book-format
                     page fragment :unknown-values unknown-values) collect it
                 else do (return-from format-page)))))


(defun load-directory-contents ()
  (length
   (setf *directory-list*
         (directory (merge-pathnames
                     (make-pathname
                      :directory '(:relative :wild-inferiors)
                      :name :wild
                      :type :wild)
                     bocproc::*books-location*)))))
