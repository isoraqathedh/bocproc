;;;; Books and pages

#|
Classes and methods that relate to representing and finding books and pages.
|#

(in-package :bocproc)

;;; Basic variables and helper functions
(defvar *config-file* (asdf:system-relative-pathname
                       :bocproc "config" :type "json"))

(defparameter *books-location*
  (merge-pathnames
   (make-pathname :directory '(:relative "Documents" "My Scans"))
   (uiop/common-lisp:user-homedir-pathname)))

(defparameter *config* ()
  "Configuration sexpr.")

(defun load-config-file ()
  "Reads the config file into *config*."
  (with-open-file (s *config-file* :external-format :utf-8)
    (setf *config* (cl-json:decode-json s))))

(defun books-location-subdir (&rest folder-names)
  (merge-pathnames
   (make-pathname :directory (cons :relative folder-names))
   *books-location*))

;;; Generic classes for a page.
(defclass page ()
  ((book :initarg :book
         :initform nil
         :accessor book
         :documentation "Book number of a page.")
   (page :initarg :page
         :initform nil
         :accessor page
         :documentation "Page number of a page")
   (subpage :initarg :subpage
            :initform nil
            :accessor subpage
            :documentation "Subpage number of a page")
   (root-path :initarg :root
              :reader root
              :documentation "The path
 where all the objects that this class represents are stored in."))
  (:documentation "Represents a single file
that is either a page or part of one."))

;;; Specific errors
(define-condition not-specific-enough (error)
  ((problem-book :initarg :problem-book
                 :reader problem-book)
   (target-specificity :initarg :target-specificity
                       :reader target-specificity))
  (:report (lambda (condition stream)
             (format stream "The object ~a has specificity ~a, ~
                             which is less specific than ~a."
                          (problem-book condition)
                          (specificity (problem-book condition))
                          (target-specificity condition)))))

;;; Specific types of page
(defclass book-of-conworlds-page (page)
  ((root-path :initform (books-location-subdir "Book of Conworlds")))
  (:documentation "Represents a page in the book of conworlds book."))

(defclass non-boc-conworld-page (page)
  ((root-path :initform (books-location-subdir "Book of Conworlds" "Non-BoC")))
  (:documentation "Represents a book of conworlds page
without an associated book."))

;;; Printing controls
(defgeneric format-page-code (page)
  (:documentation "Writes the printed representation of a page object.")
  (:method ((page book-of-conworlds-page))
    (with-accessors ((book book) (page page) (subpage subpage)) page
      (format nil "Book ~a/~a~c"
              (or book "?")
              (if page (format nil "~2,'0d" page) "??")
              (cond ((null subpage) #\?)
                    ((<= 1 subpage 26)
                     (char "abcdefghijklmnopqrstuvwxyz" (1- subpage))))))))

(defmethod print-object ((object book-of-conworlds-page) stream)
  (print-unreadable-object (object stream :type t)
    (princ (format-page-code object) stream)))

(defgeneric specificity (page)
  (:documentation "Determines how specific a page is.")
  (:method ((page page))
    (with-accessors ((book book) (page page) (subpage subpage)) page
      (cond ((and book page subpage) :subpage)
            ((and book page) :page)
            (book :book)
            (t nil)))))

(defun specificity< (specificity-spec &rest more-specificities)
  "Determines if the specificities are in increasing order."
  (apply #'<=
         (mapcar (lambda (specificity)
                   (or (position specificity '(nil :book :page :subpage))
                       (error "Invalid specificity: ~a" specificity)))
                 (cons specificity-spec more-specificities))))

;;; Operations
(defgeneric ensure-book-specific (book specificity)
  (:documentation "Ensures that the book is at least as specific as specificity.
Raises an error and gives common resolutions.")
  (:method ((book page) specificity)
    (if (specificity< (specificity book) specificity)
        (error 'not-specific-enough
               :problem-book book
               :target-specificity specificity))))

(defgeneric book-path (book)
  (:documentation "Makes the pathname for the book.
Raises an error if the book is not specific enough.")
  (:method ((book page))
    (ensure-book-specific book :book)
    ))

(defgeneric book-exists (book)
  (:documentation "Checks if a book exists.
 Raises errors if the book is not specific enough.")
  (:method ((book number))
    ())
  #+(or) (:method ((book book))
           (unless (specificity<= :book (specificity book))
             (error 'not-specific-enough :thing ))))

(defgeneric book-ignored-p (book)
  (:documentation "Checks if a book is ignored.")
  (:method ((book number))
    (->> *config*
      (assoc :ignored) cdr
      (assoc :boc) cdr
      (nth (1- book))
      (eql t)))
  (:method ((book page))
    (book-ignored-p (book book))))

(defgeneric page-ignored-p (book page)
  (:documentation "Checks if a book is ignored.")
  (:method ((book number) (page number))
    (with-expression-threading (*config*)
      (assoc :ignored :||) cdr
      (assoc :boc :||) cdr
      (nth (1- book) :||)
      (and (listp :||)
           (member page :||)))))
