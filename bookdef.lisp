;;;; Book definitions

#| Defines what a book and its associated page is.

A "page" is simply an arbitrary JPEG file that
contains (or purports to contain) some conworlding information.
A "series" is a collection of such pages with a common file name convention.
This convention typically includes a serial number,
which due to the physical manifestations of the image files
have many parts and are better understood as a list of numbers
rather than just a single number.
|#

(in-package #:bocproc)

(defparameter *series-list* ()
  "List of series that are currently on the system.")

(defparameter *directory-list* ()
  "List of files currently in the base directory.")

;;; Basic objects and accessors
(defgeneric get-page-property (object name)
  (:documentation "Retrieves the page property from a page object.")
  (:method ((object book-page) name)
    (gethash name (properties object))))

(defgeneric (setf get-page-property) (value object name)
  (:documentation "Sets the page property from a page object to VALUE.")
  (:method (value (object book-page) name)
    (setf (gethash name (properties object)) value)))

(defun get-page-property-function (property)
  "Create a get-page-property function."
  (lambda (page)
    (get-page-property page property)))

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

(defun find-book (name)
  "Finds the book series named NAME from the list"
  (or (find name *series-list* :key #'series)
      (error "Book not found: ~s" name)))

(defun undefine-book (name)
  "Removes the book series named NAME from the list."
  (delete name *series-list* :key #'series))

(defun make-page (name page-numbers)
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

;;; Formatting books and their title.
(defun normalise-book-format (page fragment &key unknown-values limit)
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
          (format-timestring nil (now)
                             :format (cdr fragment)
                             :timezone (get-timezone)))
         ((find (car fragment) (specificities page) :key #'first)
          (destructuring-bind (spec &key (pad 0) (type :number)) fragment
            (let* ((spec-pos (position spec (specificities page) :key #'first))
                   (page-number
                     (nth spec-pos (page-numbers page))))
              (when (or (not page-number)
                        (and limit
                             (< (position limit (specificities page)
                                          :key #'first)
                                spec-pos)))
                    (handle-missing-value))
              (case type
                (:letter
                 (format nil "~c" (number->letter page-number)))
                (:number
                 (format nil "~?"
                         (format nil "~~~d,'0d" pad)
                         (list page-number)))))))
         (t (let ((property-value (get-page-property page (car fragment))))
              (unless property-value
                (handle-missing-value))
              (format nil "~a" property-value))))))))

(defgeneric format-page (page &key unknown-values limit)
  (:documentation "Takes a PAGE and derives its filename from it.
If there are any parts that are not specified,
UNKNOWN-VALUES will control what happens next:

- NIL simply makes the function return nil.
- :ERROR causes an error to be raised.
- :GLOB, the default, replaces any unknown values with a globbing *.")
  (:method ((page book-page) &key (unknown-values :glob) limit)
    (apply #'concatenate 'string
           (namestring *books-location*)
           (loop for fragment in (book-format page)
                 if (normalise-book-format
                     page fragment :unknown-values unknown-values
                                   :limit limit) collect it
                 else do (return-from format-page)))))

(defun load-directory-contents ()
  (length
   (setf *directory-list*
         (directory (merge-pathnames
                     (make-pathname
                      :directory '(:relative :wild-inferiors)
                      :name :wild
                      :type :wild)
                     *books-location*)))))

;;; Genre tagging
;; Technically not part of exiftool integration
;; but there's no better place to put it in.
(defgeneric set-genre (page)
  (:documentation "Computes and sets the genre property on a given page.")
  (:method ((page book-page))
    (setf (get-page-property page :genre)
          (-> page
            (get-page-property :tags)
            tag-manifestations
            (getf :filename)))))
