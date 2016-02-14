;;;; Books and pages

#|
Classes and methods that relate to representing and finding books and pages.
|#

(in-package :bocproc)
;;; Generic classes for a page.
(defclass page ()
  ((page-number-list :initarg :page-numbers
                     :initform (list)
                     :accessor page-numbers
                     :documentation "A list of numbers
that represent the book/page/subpage structure of the page.")
   (specificities :initform ()
                  :reader specificities
                  :allocation :class
                  :documentation "A list of specificity-specs
that indicate what the corresponding number in page-number-list means.")
   (root-path :reader root
              :documentation "The path
 where all the objects that this class represents are stored in."))
  (:documentation "Represents a single file
that is either a page or part of one.
Should not be instantiated directly;
if a page that isn't covered in the other classes is required
use generic-page."))

(defclass generic-page (page)
  ((root-path :initarg :root
              :initform (uiop/common-lisp:user-homedir-pathname))
   (specifcities :initarg :specificities))
  (:documentation "Represents a single file
that is either a page or a part of one.
This generic page is meant to be instantiated if none of the other classes
that are provided fit."))

;;; Specific errors
(define-condition not-specific-enough (error)
  ((problem-book :initarg :current-specificity
                 :reader current-specificity)
   (target-specificity :initarg :target-specificity
                       :reader target-specificity))
  (:report (lambda (condition stream)
             (format stream "~a returned NIL, ~
which is not specific enough for ~a."
                     (current-specificity condition)
                     (target-specificity condition)))))

(define-condition no-such-specificity (error)
  ((datum :initarg :datum
          :reader datum)
   (target :initarg :target
           :reader target))
  (:report (lambda (condition stream)
             (format stream "No such specificity ~s in ~s."
                     (datum condition)
                     (target condition)))))

(defclass dated-page ()
  ((date-of-creation :accessor date-of-creation))
  (:documentation "Mixin of pages
whose file name depends on the date of creation."))

(defclass page-using-wildcards () ()
  (:documentation "Mixin for pages
whose file names depend on wildcards."))

;;; Specific types of page
(defclass book-of-conworlds-page (page-using-wildcards page)
  ((specificities :initform '(:book :page :subpage))
   (root-path :initform (books-location-subdir "Book of Conworlds")))
  (:documentation "Represents a page in the book of conworlds book."))

(defclass non-boc-conworld-page (dated-page page-using-wildcards page)
  ((specificities :initform '(:page :subpage))
   (root-path :initform (books-location-subdir "Book of Conworlds" "Non-BoC")))
  (:documentation "Represents a book of conworlds page
without an associated book."))

(defclass non-boc-page (dated-page page)
  ((specificities :initform '(:page))
   (root-path :initform (books-location-subdir "Unsorted by Date")))
  (:documentation "Represents another scan."))

;;; Printing controls
(defun format-time (timestamp)
  (local-time:format-timestring
   nil timestamp :format '(:year "-" (:month 2) " (" :short-month ")")))

(defmethod initialize-instance :after
    ((instance dated-page) &key &allow-other-keys)
  (setf (date-of-creation instance) (local-time:now)))

(defgeneric format-page-code (page)
  (:documentation "Writes the printed representation of a page object.")
  (:method ((page book-of-conworlds-page))
    (destructuring-bind (&optional book page subpage) (page-numbers page)
      (format nil "~:[?~;~:*~d~] / ~:[??~;~:*~2,'0d~]~:[?~;~:*~c~]"
              book
              page
              (number->letter subpage))))
  (:method ((page-object non-boc-conworld-page))
    (destructuring-bind (&optional page subpage) (page-numbers page-object)
      (format nil "~a / ~:[??~;~:*~2,'0d~]~:[?~;~:*~c~]"
              (format-time (date-of-creation page-object))
              page
              (number->letter subpage))))
  (:method ((page-object non-boc-page))
    (format nil "~a / ~:[????~;~:*~4,'0d~]"
            (format-time (date-of-creation page-object))
            (first (page-numbers page-object)))))

(defmethod print-object ((object page) stream)
  (print-unreadable-object (object stream :type t)
    (princ (format-page-code object) stream)))

(defmethod print-object ((object generic-page) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (root object) (page-numbers object))))

;;; Operations
;;; = = = = =

;;; Specificity requirements
(defgeneric get-specificity (object specificity)
  (:documentation "Retrieves the appropriate page number
with the provided specificity.")
  (:method ((object page) specificity)
    (nth (position specificity (specificities object)) (page-numbers object))))

(defgeneric (setf get-specificity) (value object specificity)
  (:method (value (object page) specificity)
    (setf (page-numbers object)
          (loop with page-number-list = (page-numbers object)
                with list-position = (position specificity
                                               (specificities object))
                for i from 0 to list-position
                for j = page-number-list then (cdr j)
                collect (cond ((= i list-position) value)
                              ((< i (length page-number-list))
                               (car j))
                              (t nil))))))

(defgeneric specificity (page-object)
  (:documentation "Returns the specificity of the page object.")
  (:method ((object page))
    (loop for page-number = (page-numbers object) then (cdr page-number)
          for specificity in (cons nil (specificities object))
          unless page-number return specificity
          finally (return specificity))))

(defgeneric ensure-book-specific (book specificity)
  (:documentation "Ensures that the book is at least as specific as specificity.
Raises appropriate errors and gives common resolutions.")
  (:method ((book-obj page) specificity)
    (let ((specificity-spec (specificities book-obj))
          (page-number-list (page-numbers book-obj)))
      (unless (member specificity specificity-spec)
        ;; Catch spec errors.
        (restart-case (error 'no-such-specificity
                             :datum specificity :target specificity-spec)
          (use-value (return-value)
            :interactive (lambda ()
                           (format *query-io* "~&Write the return value: ")
                           (list (read)))
            :report "Specify the return value for this function."
            (return-from ensure-book-specific return-value))))
      (flet ((back-to-top (page spec)
               ;; Restart the main loop to ensure everything is still sane.
               (return-from main-loop (ensure-book-specific page spec))))
        ;; Zip through the list to check for discrepancies and allow fixing.
        (loop
          named main-loop
          for test-specificity in specificity-spec
          for test-number = page-number-list then (cdr test-number)
          for list-position from 1
          unless (car test-number)
          do (restart-case (error 'not-specific-enough
                                  :current-specificity test-specificity
                                  :target-specificity specificity)
               (store-value (new-value)
                 :interactive (lambda ()
                                (format *query-io*
                                        "~&Write in a new value for ~a: "
                                        test-specificity)
                                (list (parse-integer (read-line))))
                 :report "Provide a value for the missing specificity."
                 (setf (get-specificity book-obj test-specificity) new-value)
                 (back-to-top book-obj specificity))
               (store-all-values (new-values)
                 :report "Put in new values of book/page/subpage all at once."
                 :interactive (lambda ()
                                (format *query-io*
                                        "~&Write in order the values of ~a: "
                                        specificity-spec)
                                (list (read)))
                 (setf (page-numbers book-obj) new-values)
                 (back-to-top book-obj test-specificity)))
          when (eql test-specificity specificity)
          do (when (cdr test-number)
               (warn "~s still remains in page-number list!"
                     (cdr test-number)))
          and return book-obj)))))

;;; Restart functions
(defun ignore-specificity-spec (condition)
  (declare (ignore condition))
  (invoke-restart 'use-value t))

(defun implicit-subpage (condition)
  "Allows a missing subpage number to be interpreted as 1."
  (when (eql :subpage (current-specificity condition))
    (invoke-restart 'store-value 1)))

;;; Getters and setters for a specific page.
(defgeneric book-path (object)
  (:documentation "Makes the pathname for the book.
Raises an error if the book is not specific enough.")
  (:method :before ((object page))
    ;; Assert that there is is enough information.
    (handler-bind ((no-such-specificity #'ignore-specificity-spec)
                   (warning #'muffle-warning))
      (ensure-book-specific object :book)))
  (:method ((object book-of-conworlds-page))
    (merge-pathnames
     (make-pathname
      :directory (list :relative (format nil "Book ~a"
                                         (get-specificity object :book))))
     (root object)))
  (:method ((object dated-page))
    (merge-pathnames
     (make-pathname
      :directory (list :relative (format-time (date-of-creation object))))
     (root object))))

(defgeneric page-path-wild (page)
  (:documentation "Makes the pathname for a specific page.
Only returns the wild format. ")
  (:method :before ((page-object page))
    (handler-bind ((no-such-specificity #'ignore-specificity-spec))
      (ensure-book-specific page-object :page)))
  (:method ((page-object page-using-wildcards))
    (let ((page (get-specificity page-object :page))
          (subpage (get-specificity page-object :subpage)))
      (merge-pathnames
       (make-pathname
        :name (pathname-name (format nil "~2,'0d~:[?~;~:*~c~]-*"
                                     page (number->letter subpage)))
        :type "jpg")
       (book-path page-object)))))

(defgeneric page-path (page)
  (:documentation "Finds the pathname for a specific page.")
  (:method :before ((page-object page))
    (handler-bind ((no-such-specificity #'ignore-specificity-spec))
      (ensure-book-specific page-object :page)))
  (:method ((page-object page-using-wildcards))
    (let ((search-results (directory (page-path-wild page-object))))
      (cond ((= 1 (length search-results))
             (first search-results))
            ((= 0 (length search-results))
             (warn "No items found for specified page number."))
            (t (warn "Too many items found for specified page number.")
               (values (first search-results)
                       (rest search-results))))))
  (:method ((object non-boc-page))
    (merge-pathnames
     (make-pathname
      :directory (list :relative (format-time (date-of-creation object)))
      :name (format nil "SCAN~4,'0d" (get-specificity object :page))
      :type "jpg")
     (root object))))

;;; Existence and Ignoredness
(defgeneric book-exists-p (book)
  (:documentation "Checks if a book exists.
 Raises errors if the book is not specific enough.")
  (:method :before ((object page))
    (ensure-book-specific object :book))
  (:method ((page-object page))
    (probe-file (book-path page-object))))

(defgeneric book-ignored-p (book)
  (:documentation "Checks if a book is ignored.")
  (:method :before ((object page))
    (handler-bind ((warning #'muffle-warning))
      (ensure-book-specific object :book)))
  (:method ((object book-of-conworlds-page))
    (->> *config*
      (gethash "ignored")
      (gethash "boc")
      (nth (1- (get-specificity object :book)))
      (eql t))))

(defgeneric page-exists-p (page)
  (:documentation "Checks if a page exists.")
  (:method ((page-object page))
    (handler-bind ((not-specific-enough #'implicit-subpage)
                   (warning #'muffle-warning))
      (ensure-book-specific page-object :subpage)
      (page-path page-object))))

(defgeneric page-ignored-p (page)
  (:documentation "Checks if a book is ignored.")
  (:method ((page-object book-of-conworlds-page))
    (ensure-book-specific page-object :page)
    (with-accessors ((page page) (book book)) page-object
      (with-expression-threading ()
        *config*
        (gethash "ignored" :||)
        (gethash "boc" :||)
        (nth (1- book) :||)
        (and (listp :||)
             (member page :||))))))

;;; Auto-determination
(defgeneric calculate-book (book spec)
  (:documentation "Automatically calculates the book number.
If spec is :next, return the first book that is not used or ignored.
If spec is :cur, return the last book that is used and not ignored.
If spec is a wandering page, extract its paging-behaviour and use that.
This is only used for pages that have a book number;
it will return nil if not found.")
  ;; wandering-page placed in another file.
  (:method ((page-number-slot page) (spec symbol))
    (when (member :book (specificities page-number-slot))
      (loop for potential-book from 1
            do (setf (get-specificity page-number-slot :book) potential-book)
            unless (or (book-ignored-p page-number-slot)
                       (book-exists-p page-number-slot))
            return (ecase spec
                     (:next page-number-slot)
                     (:cur (decf (get-specificity page-number-slot :book))
                      page-number-slot))))))
