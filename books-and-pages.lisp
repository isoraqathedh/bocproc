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
 where all the objects that this class represents are stored in.")
   (series-key :reader series-key
               :allocation :class
               :documentation "A short string indicating the code of the page.")
   (cutoffs :reader cutoffs
            :allocation :class
            :documentation "The possible values each page number can take."))
  (:documentation "Represents a single file
that is either a page or part of one.
Should not be instantiated directly;
if a page that isn't covered in the other classes is required
use generic-page."))

(defvar *cutoffs* '(1 . 99)
  "Default ranges for page numbers to be under.")

(defclass generic-page (page)
  ((root-path :initarg :root
              :initform (uiop/common-lisp:user-homedir-pathname))
   (specifcities :initarg :specificities)
   (series-key :initform "generic"))
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
   (cutoffs :initform '(nil nil (1 . 26)))
   (root-path :initform (books-location-subdir "Book of Conworlds"))
   (series-key :initform "boc"))
  (:documentation "Represents a page in the book of conworlds book."))

(defclass boc-purple-page (book-of-conworlds-page)
  ((series-key :initform "purple"))
  (:documentation "Represents a page in the purple series of a BoC book."))

(defclass non-boc-conworld-page (dated-page book-of-conworlds-page)
  ((specificities :initform '(:page :subpage))
   (cutoffs :initform '(nil (1 . 26)))
   (root-path :initform (books-location-subdir "Book of Conworlds" "Non-BoC"))
   (series-key :initform "nboc"))
  (:documentation "Represents a book of conworlds page
without an associated book."))

(defclass non-boc-page (dated-page page)
  ((specificities :initform '(:page))
   (cutoffs :initform '((0 . 9999)))
   (root-path :initform (books-location-subdir "Unsorted by Date"))
   (series-key :initform "uncategorised"))
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

(defgeneric specificity (object)
  (:documentation "Retrieves the specificity of an object.")
  (:method ((object page))
    (loop for number across (page-numbers object)
          for specificity in (specificities object)
          and prev-specificity = nil then specificity
          unless number return prev-specificity
          finally (return specificity))))

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
      (format nil "~:[?~;~:*~d~]/~:[??~;~:*~2,'0d~]~:[?~;~:*~c~]"
              book page (number->letter subpage))))
  (:method ((page-object non-boc-conworld-page))
    (specificity-bind ((page :page) (subpage :subpage)) page-object
      (format nil "~a/~:[??~;~:*~2,'0d~]~:[?~;~:*~c~]"
              (format-time (date-of-creation page-object))
              page
              (number->letter subpage))))
  (:method ((page-object non-boc-page))
    (specificity-bind ((page :page)) page-object
        (format nil "~a/~:[????~;~:*~4,'0d~]"
             (format-time (date-of-creation page-object))
             page)))
  (:method ((object page))))

(defmethod print-object ((object page) stream)
  (print-unreadable-object (object stream :type t)
    (princ (format-page-code object) stream)))

(defmethod print-object ((object generic-page) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (root object) (page-numbers object))))

(defvar *do-ensure* nil
  "Determines whether or not checking for well-formedness.")

(defgeneric ensure-book-specific (book specificity)
  (:documentation "Ensures that the book is at least as specific as specificity.
Raises appropriate errors and gives common resolutions.")
  (:method :around (book specificity)
    (when *do-ensure*
      (call-next-method)))
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
;;; Get cutoffs

(defgeneric get-cutoffs (book specificity starting-book)
  (:documentation "Returns the two cutoffs of a specificity in a book,
or the default if there is none")
  (:method ((book page) specificity starting-book)
    (let ((test-cutoffs (with-expression-threading () specificity
                          (position :|| (specificities book))
                          (nth :|| (cutoffs book)))))
      (cons (or
             (car test-cutoffs)
             (when starting-book
               (1+ (get-specificity starting-book specificity)))
             (car *cutoffs*))
            (or (cdr test-cutoffs) (cdr *cutoffs*))))))

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

(defgeneric construct-filename (book-object &optional use-wild)
  (:documentation "Constructs the filename off of an object.")
  (:method ((book-object book-of-conworlds-page) &optional use-wild)
    (specificity-bind ((page :page) (subpage :subpage)) book-object
      (pathname-name
       (ecase (specificity book-object)
         (:book (if use-wild "*" ""))
         (:page (format nil "~2,'0d?-*" page))
         (:subpage (format nil "~2,'0d~c-*" page (number->letter subpage)))))))
  (:method ((object non-boc-page) &optional use-wild)
    (declare (ignore use-wild))
    (when (eql (specificity object) :page)
      (specificity-bind ((page :page)) object
        (format nil "SCAN~4,'0d" page)))))

(defgeneric construct-folder-name (book-object)
  (:documentation "Returns the folder name associated with the book.")
  (:method ((object book-of-conworlds-page))
    (specificity-bind ((book :book)) object
      (format nil "Book ~d" book)))
  (:method ((object boc-purple-page))
    (specificity-bind ((book :book)) object
      (format nil "Book p~d" book)))
  (:method ((object dated-page))
    (format-time (date-of-creation object))))

(defgeneric get-path (object &key use-wild expand-wild)
  (:documentation "Returns the path up to the specificity.
If use-wild is non-nil, then provide a wild pathname if relevant.")
  (:method ((object page) &key use-wild expand-wild)
    ;; Find the wild path in any case,
    ;; as it is required to find the ordinary path.
    (let* ((specificity (specificity object))
           (wild-path
            (merge-pathnames
             (make-pathname
              :directory (list :relative (construct-folder-name object))
              :name (construct-filename object (or use-wild expand-wild))
              :type (cond ((and (typep object 'page-using-wildcards)
                                (or use-wild expand-wild)) :wild)
                          ((find specificity '(:page :subpage)) "jpg")))
             (root object))))
      ;; Now decide if we bail out here.
      (if (and (typep object 'page-using-wildcards)
               expand-wild
               (wild-pathname-p wild-path))
          (expand-wild-pathname wild-path)
          wild-path))))

;;; Existence and Ignorance
(defgeneric book-exists-p (book)
  (:documentation "Checks if a page exists at that specificity level.")
  (:method ((page-object page))
    (handler-bind ((warning #'muffle-warning)
                   (type-error
                     ;; probe-file errors when given nil;
                     ;; probe-file* chokes on Unicode filenames.
                     ;; This will have to do.
                     (lambda (c)
                       (declare (ignore c))
                       (return-from book-exists-p nil))))
      (probe-file
       (get-path
        page-object
        :expand-wild (not (eql (specificity page-object) :book)))))))

(defgeneric book-ignored-p (book)
  (:documentation "Checks if a page is ignored at that specificity level.")
  (:method ((object book-of-conworlds-page))
    (when (find :book (specificities object))
      ;; No pages are ignored if they are not arranged in books.
      (let ((specificity (specificity object)))
        (specificity-bind ((book :book) (page :page)) object
          (with-expression-threading ()
            *config*
            (assoc :ignore-list :||) #'cdr
            (assoc (series-key object) :|| :test #'string=) #'cdr
            (assoc book :||) #'cdr
            (ecase specificity
              (:book (eql t :||))
              ((:page :subpage) (find page :||))))))))
  (:method ((book non-boc-conworld-page)))
  (:method ((book non-boc-page))))

;;; Auto-determination
(defun expand-to-specificities (book page-determination-plist)
  "Expands PAGE-DETERMINATION-PLIST into the specificities understood by BOOK."
  (let ((specificities-list (specificities book))
        (next (getf page-determination-plist :next)))
    (if next
        (loop with match-number = (or (position next specificities-list)
                                      (error "The specificity ~a ~
                                                 is not defined." next))
              for spec in specificities-list
              for counter from 0
              collect (cond
                        ((< counter match-number) :cur)
                        ((= counter match-number) :next)
                        ((> counter match-number) :first)))
        (loop for spec-key in specificities-list
              for spec-val = (or (getf page-determination-plist spec-key)
                                 (error "The specificity ~a is not found"
                                        spec-key))
              collect spec-val))))

(defgeneric calculate-page-number (book specificity requested-value
                                   starting-point)
  (:documentation "Automatically calculates
the value associated with the specificity.
If requested-value is :next, return the first book that is not used or ignored.
If requested-value is :cur, return the previous book to :next.
If requested-value is a number, simply slot the number into the specificity.
Starts at the starting-point if provided.
Returns the original object.")
  ;; wandering-page placed in another file.
  (:method ((page-number-slot page) specificity (end-condition (eql :first))
            starting-point)
    (setf (get-specificity page-number-slot specificity)
          ;; When we start a new section,
          ;; we don't particularly care where we started from.
          (car (get-cutoffs page-number-slot specificity nil))))
  (:method ((page-number-slot page) specificity (direct-value number)
            starting-point)
    (setf (get-specificity page-number-slot specificity) direct-value))
  (:method ((page-number-slot page) specificity (end-condition symbol)
            starting-point)
    (destructuring-bind (start . stop)
        (get-cutoffs page-number-slot specificity starting-point)
      (when (< stop start)
        (error "There's no room to put anything more than page ~d ~
                in the cutoff range ~d → ~d!" start start stop))
      (loop for test-number from start to stop do
            (setf (get-specificity page-number-slot specificity) test-number)
            (when (not (or (book-ignored-p page-number-slot)
                           (book-exists-p page-number-slot)))
              (ecase end-condition
                (:cur (decf (get-specificity page-number-slot specificity)))
                (:next))
              (return page-number-slot))))))

(defgeneric construct-book-object (book-designator slots-spec starting-point
                                   &optional kill-existing)
  (:documentation "Calculate all positions of the page-number.
Kill-existing controls whether or not the original values
are wiped before calculation.
You should not alter this parameter normally.")
  (:method ((book-designator string) slots-spec starting-point
            &optional kill-existing)
    (declare (ignore kill-existing))
    (construct-book-object (make-instance
                            (alexandria:switch (book-designator
                                                :test #'string-equal)
                              ("boc" 'book-of-conworlds-page)
                              ("purple" 'boc-purple-page)
                              ("nboc" 'non-boc-conworld-page)
                              ("uncategorised" 'non-boc-page)))
                           slots-spec
                           starting-point
                           nil))
  (:method ((book-designator symbol) slots-spec starting-point
            &optional kill-existing)
    (construct-book-object (symbol-name book-designator)
                           slots-spec kill-existing))
  (:method :before ((test-object page) slots-spec starting-point
                    &optional (kill-existing t))
    ;; Clear all the page numbers before attempting computation
    (when kill-existing
      (loop for i from 0 below (length (specificities test-object))
            do (setf (aref (specificities test-object) i) nil))))
  (:method ((test-object page) (ending-condition-plist list) starting-point
            &optional (kill-existing t))
    (declare (ignore kill-existing))
    (loop for spec in (specificities test-object)
          for ending-condition in (expand-to-specificities
                                   test-object ending-condition-plist)
          do (calculate-page-number
              test-object spec ending-condition starting-point)
          finally (return test-object))))

(defun allocate-multiple-books (book-designator slots-specs)
  "Allocates page numbers in batch for a single series."
  (loop for slot-spec in slots-specs
        for next-page = (construct-book-object book-designator
                                               slot-spec
                                               nil)
        then (construct-book-object book-designator
                                    slot-spec
                                    next-page)
        collect next-page))
