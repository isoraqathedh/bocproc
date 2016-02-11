;;;; Books and pages

#|
Classes and methods that relate to representing and finding books and pages.
|#

(in-package :bocproc)

;;; Basic variables and helper functions
(defvar *config-file* (asdf:system-relative-pathname
                       :bocproc "config" :type "yaml"))

(defparameter *books-location*
  (merge-pathnames
   (make-pathname :directory '(:relative "Documents" "My Scans"))
   (uiop/common-lisp:user-homedir-pathname)))

(defparameter *config* ()
  "Configuration hash-table.")

(defun number->letter (number)
  (ecase number
    (1  #\a) (2  #\b) (3  #\c) (4  #\d) (5  #\e)
    (6  #\f) (7  #\g) (8  #\h) (9  #\i) (10 #\j)
    (11 #\k) (12 #\l) (13 #\m) (14 #\n) (15 #\o)
    (16 #\p) (17 #\q) (18 #\r) (19 #\s) (20 #\t)
    (21 #\u) (22 #\v) (23 #\w) (24 #\x) (25 #\y) (26 #\z)))

(defun load-config-file ()
  "Reads the config file into *config*."
  (with-open-file (s *config-file* :external-format :utf-8)
    (setf *config*
          (-> s
            uiop:slurp-stream-string
            cl-yy:yaml-load
            car
            cadr))))

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
  ((problem-book :initarg :missing-value-accessor
                 :reader missing-value-accessor)
   (target-specificity :initarg :target-specificity
                       :reader target-specificity))
  (:report (lambda (condition stream)
             (format stream "~a returned NIL, ~
which is not specific enough for ~a."
                     (missing-value-accessor condition)
                     (target-specificity condition)))))

(defclass dated-page ()
  ((date-of-creation :accessor date-of-creation))
  (:documentation "Mixin of pages
whose file name depends on the date of creation."))

;;; Specific types of page
(defclass book-of-conworlds-page (page)
  ((root-path :initform (books-location-subdir "Book of Conworlds")))
  (:documentation "Represents a page in the book of conworlds book."))

(defclass non-boc-conworld-page (book-of-conworlds-page dated-page)
  ((root-path :initform (books-location-subdir "Book of Conworlds" "Non-BoC")))
  (:documentation "Represents a book of conworlds page
without an associated book."))

(defclass non-boc-page (page dated-page)
  ((root-path :initform (books-location-subdir "Unsorted by Date")))
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
    (with-accessors ((book book) (page page) (subpage subpage)) page
      (format nil "~:[?~;~d~] / ~:[??~;~:*~2,'0d~]~c"
              book
              page
              (cond ((null subpage) #\?)
                    ((<= 1 subpage 26)
                     (char "abcdefghijklmnopqrstuvwxyz" (1- subpage)))))))
  (:method ((page-object non-boc-conworld-page))
    (with-accessors ((page page) (subpage subpage) (date date-of-creation))
        page-object
      (format nil "~a / ~:[??~;~:*~2,'0d~]~c"
              (format-time date)
              page
              (cond ((null subpage) #\?)
                    ((<= 1 subpage 26)
                     (char "abcdefghijklmnopqrstuvwxyz" (1- subpage)))))))
  (:method ((page-object non-boc-page))
    (with-accessors ((page page) (date date-of-creation)) page-object
      (format nil "~a / ~:[????~;~:*~4,'0d~]" (format-time date) page))))

(defmethod print-object ((object page) stream)
  (print-unreadable-object (object stream :type t)
    (princ (format-page-code object) stream)))

;;; Operations
(defgeneric ensure-book-specific (book specificity)
  (:documentation "Ensures that the book is at least as specific as specificity.
Raises an error and gives common resolutions.")
  (:method ((book-obj page) specificity)
    (let ((specificity-spec '(nil :book :page :subpage))
          (getters (list (constantly t) #'book #'page #'subpage))
          (setters (list nil #'(setf book) #'(setf page) #'(setf subpage))))
      (unless (member specificity specificity-spec)
        (error "~a is not a specificity-spec." specificity))
      (loop for test-specificity in specificity-spec
            and prev-specificity = nil then test-specificity
            for getter in getters
            for setter in setters
            unless (funcall getter book-obj)
            do (restart-case (error 'not-specific-enough
                                    :missing-value-accessor setter
                                    :target-specificity test-specificity)
                 (new-value (new-value &optional (slot-writer setter))
                   :interactive (lambda ()
                                  (format *query-io*
                                          "Write in a new value for ~a: "
                                          setter)
                                  (list (parse-integer (read-line))))
                   :report "Provide a value for the missing specificity."
                   (funcall slot-writer new-value book-obj)
                   (ensure-book-specific book-obj test-specificity))
                 (new-values (new-book new-page new-subpage)
                   :report "Put in new values of book/page/subpage all at once."
                   :interactive (lambda ()
                                  (format *query-io*
                                          "Three-element list of (book page subpage): ")
                                  (read))
                   (setf (book book-obj) new-book
                         (page book-obj) new-page
                         (subpage book-obj) new-subpage)
                   (ensure-book-specific book-obj test-specificity)))
            when (eql test-specificity specificity) do (loop-finish)
            finally (return book-obj)))))

(defgeneric book-path (book)
  (:documentation "Makes the pathname for the book.
Raises an error if the book is not specific enough.")
  (:method ((book book-of-cownworlds-page))
    (ensure-book-specific book :book)
    (with-accessors ((book book) (root root)) book
      (merge-pathnames
       (make-pathname :directory (list :relative (format nil "Book ~a" book)))
       root)))
  (:method ((book non-boc-conworlds-pate))
    (ensure-book-specific book :book)
    (with-accessors ((root root)) book
      (merge-pathnames
       (make-pathname :directory (list :relative ()))))))

(defgeneric page-path-wild (page)
  (:documentation "Makes the pathname for a specific page.
Only returns the wild format. ")
  (:method ((page-object book-of-conworlds-page))
    (ensure-book-specific page-object :page)
    (with-accessors ((page page) (subpage subpage)) page-object
      (merge-pathnames
       (make-pathname :name (pathname-name
                             (format nil "~a~a-*"
                                     page
                                     (if subpage (number->letter subpage) "?")))
                      :type "jpg")
       (book-path page-object)))))

(defgeneric page-path (page)
  (:documentation "Finds the pathname for a specific page.")
  (:method :before ((page-object page))
    (ensure-book-specific page-object :page))
  (:method ((page-object book-of-conworlds-page))
    (let ((search-results (directory (page-path-wild page-object))))
      (cond ((= 1 (length search-results))
             (first search-results))
            ((= 0 (length search-results))
             (warn "No items found for specified page number."))
            (t (warn "Too many items found for specified page number.")
               (values (first search-results)
                       (rest search-results))))))
  (:method ((object non-boc-page))
    (with-accessors ((page page) (date date-of-creation) (root root)) object
      (merge-pathnames
       (make-pathname :directory (list :relative (format-time date))
                      :name (format nil "SCAN~4,'0d" page)
                      :type "jpg")
       root))))

(defgeneric book-exists (book)
  (:documentation "Checks if a book exists.
 Raises errors if the book is not specific enough.")
  (:method ((page-object page))
    (ensure-book-specific page-object :book)
    (probe-file (book-path page-object))))

(defgeneric book-ignored-p (book)
  (:documentation "Checks if a book is ignored.")
  (:method ((book-object book-of-conworlds-page))
    (ensure-book-specific page-object :book)
    (with-accessors ((book book)) book-object
      (->> *config*
        (gethash "ignored")
        (gethash "boc")
        (nth (1- book))
        (eql t)))))

(defgeneric page-exists-p (page)
  (:documentation "Checks if a page exists.")
  (:method ((page-object page))
    (ensure-book-specific page-object :subpage)
    (page-path page-object)))

(defgeneric page-ignored-p (page)
  (:documentation "Checks if a book is ignored.")
  (:method ((page-object book-of-conworlds-page))
    (ensure-book-specific page-object :page)
    (with-accessors ((page page) (book book)) page-object
      (with-expression-threading (*config*)
        (gethash "ignored" :||)
        (gethash "boc" :||)
        (nth (1- book) :||)
        (and (listp :||)
             (member page :||))))))

;;; Auto-determination
(defgeneric calculate-page )
