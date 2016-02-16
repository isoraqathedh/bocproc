;;;; Books and pages

#|
Classes and methods that relate to representing and finding books and pages.
|#

(in-package :bocproc)
;;; Generic classes for a page.
(defclass page ()
  ((page-number-list :initarg :page-numbers
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

;;; Mixins
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

(defclass non-boc-conworld-page (dated-page page-using-wildcards book-of-conworlds-page)
  ((specificities :initform '(:page :subpage))
   (root-path :initform (books-location-subdir "Book of Conworlds" "Non-BoC")))
  (:documentation "Represents a book of conworlds page
without an associated book."))

(defclass non-boc-page (dated-page page)
  ((specificities :initform '(:page))
   (root-path :initform (books-location-subdir "Unsorted by Date")))
  (:documentation "Represents another scan."))

;;; Printing controls

(defmethod initialize-instance :after
    ((instance dated-page) &key &allow-other-keys)
  (setf (date-of-creation instance) (local-time:now)))

(defmethod initialize-instance :after
    ((instance page) &key page-numbers &allow-other-keys)
  (setf (page-numbers instance)
        (loop with array = (make-array (length (specificities instance))
                                       :element-type '(or fixnum null)
                                       :initial-element nil)
              for number in page-numbers
              for counter from 0
              do (setf (aref array counter) number)
              finally (return array))))

;;; Specificities
;;; = = = = = = =
#| This is an important part of a book.
A specificity is how, er, specific a page object is.
That is, how many out of all possible dimensions it needs to define
in order to exactly pinpoint a location in page-space are so.
For instance, with specificities (:FOO :BAR :BAZ :QUUX),
There are five total specificities (all four of the above and also NIL)
corresponding to the following:

NIL: #(NIL NIL NIL NIL)
FOO: #(# NIL NIL NIL)
BAR: #(# # NIL NIL)
BAZ: #(# # # NIL)
QUUX: #(# # # #)

(Where # represents an arbitrary number)

It is implicit that the specificity of the last element
will be able to uniquely identify the file name that the page corresponds to.

Certain functions will only work when an object is specific enough.
To ensure that the object is specific enough, use `ensure-book-specific'.
|#
(defgeneric get-specificity (object specificity)
  (:documentation "Retrieves the appropriate page number
with the provided specificity.")
  (:method ((object page) specificity)
    (aref (page-numbers object) (position specificity (specificities object)))))

(defgeneric (setf get-specificity) (value object specificity)
  (:method (value (object page) specificity)
    (setf (aref (page-numbers object)
                (position specificity (specificities object)))
          value)))

(defmacro specificity-bind ((&rest vars-and-specs) object &body body)
  "Binds the specificities of a page to variables."
  (let ((object^ (gensym)))
    `(let ((,object^ ,object))
       (symbol-macrolet ,(loop for (var spec) in vars-and-specs
                               collect `(,var (get-specificity ,object^ ,spec)))
         ,@body))))

;;; Printing controls
(defun format-time (timestamp)
  (local-time:format-timestring
   nil timestamp :format '(:year "-" (:month 2) " (" :short-month ")")))

(defgeneric format-page-code (page)
  (:documentation "Writes the printed representation of a page object.")
  (:method ((object book-of-conworlds-page))
    (specificity-bind ((book :book) (page :page) (subpage :subpage)) object
      (format nil "~:[?~;~:*~d~] / ~:[??~;~:*~2,'0d~]~:[?~;~:*~c~]"
              book page (number->letter subpage))))
  (:method ((page-object non-boc-conworld-page))
    (specificity-bind ((page :page) (subpage :subpage)) page-object
      (format nil "~a / ~:[??~;~:*~2,'0d~]~:[?~;~:*~c~]"
              (format-time (date-of-creation page-object))
              page
              (number->letter subpage))))
  (:method ((page-object non-boc-page))
    (specificity-bind ((page :page)) page-object
        (format nil "~a / ~:[????~;~:*~4,'0d~]"
             (format-time (date-of-creation page-object))
             page))))

(defmethod print-object ((object page) stream)
  (print-unreadable-object (object stream :type t)
    (princ (format-page-code object) stream)))

(defmethod print-object ((object generic-page) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (root object) (page-numbers object))))

(defgeneric ensure-book-specific (book specificity)
  (:documentation "Ensures that the book is at least as specific as specificity.
Raises appropriate errors and gives common resolutions.")
  (:method ((book-obj page) specificity)
    (let ((specificity-spec (specificities book-obj))
          (page-number-list (page-numbers book-obj)))
      (when (null specificity)
        ;; All books are at least NIL specificity.
        (return-from ensure-book-specific book-obj))
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
      (loop
        named main-loop
        for test-specificity in specificity-spec
        for test-number across page-number-list
        unless test-number
        do (flet ((back-to-top (page spec)
                    ;; Restart the main loop to ensure everything is still sane.
                    (return-from main-loop (ensure-book-specific page spec))))
             (format t "Got here.")
             (restart-case (error 'not-specific-enough
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
                (back-to-top book-obj test-specificity))))
        when (eql test-specificity specificity)
        #|do (when #
        (warn "~s still remains in page-number list!"
        (cdr test-number)))
        and |# return book-obj))))

;;; Restart functions
(defun ignore-specificity-spec (condition)
  (declare (ignore condition))
  (invoke-restart 'use-value t))

(defun implicit-subpage (condition)
  "Allows a missing subpage number to be interpreted as 1."
  (when (eql :subpage (current-specificity condition))
    (invoke-restart 'store-value 1)))

;;; Operations
;;; = = = = =
;;; Determining the path.
(defun expand-wild-pathname (wild-path &optional allow-multiple-files)
  "Expands out a wild pathname and returns the first one.
Warns if there isn't exactly one result returned,
unless allow-multiple-files is non-nil."
  (let ((matched-files (directory wild-path)))
    (cond ((= 1 (length matched-files))
           (first matched-files))
          ((= 0 (length matched-files))
           (warn "No items found for specified page number."))
          (allow-multiple-files matched-files)
          (t (warn "Too many items found for specified page number.")
             (values (first matched-files)
                     (rest matched-files))))))

(defgeneric construct-boc-filename (book-object specificity &optional use-wild)
  (:documentation "Constructs the filename off of an object.")
  (:method ((book-object book-of-conworlds-page) specificity &optional use-wild)
    (specificity-bind ((page :page) (subpage :subpage)) book-object
      (cond ((eql specificity :book) (if use-wild :wild))
            (t (when (and (eql specificity :subpage)
                          (null subpage))
                 (setf subpage 1))
               (pathname-name
                (format nil "~2,'0d~:[?~;~:*~c~]-*"
                        page (number->letter subpage))))))))

(defgeneric construct-folder-name (book-object)
  (:documentation "Returns the folder name associated with the book.")
  (:method ((object book-of-conworlds-page))
    (specificity-bind ((book :book)) object
      (format nil "Book ~a" book)))
  (:method ((object dated-page))
    (format-time (date-of-creation object))))

(defgeneric path-up-to (object specificity &key use-wild expand-wild)
  (:documentation "Returns the path up to the specificity.
If use-wild is non-nil, then provide a wild pathname if relevant.")
  (:method :before ((object page) specificity &key &allow-other-keys)
    (handler-bind ((warning #'muffle-warning)
                   (not-specific-enough #'implicit-subpage))
      (ensure-book-specific object specificity)))
  (:method ((object page) (specificity symbol)
            &key use-wild expand-wild)
    (specificity-bind ((book :book)) object
      ;; Find the wild path in any case,
      ;; as it is required to find the ordinary path.
      (let ((wild-path
              (merge-pathnames
               (make-pathname
                :directory (list :relative (construct-folder-name object))
                :name (construct-boc-filename object specificity use-wild)
                :type (if (eql specificity :book)
                          (if use-wild :wild)
                          "jpg"))
               (root object))))
        ;; Now decide if we bail out here.
        (cond (use-wild wild-path)
              ((or expand-wild (find specificity '(:page :subpage)))
               (expand-wild-pathname wild-path)))))))

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
