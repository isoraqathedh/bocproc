;;;; Wandering pages

#| This file deals with "wandering pages"
– see documentation of the class for more information. |#

(in-package :bocproc)

;;; Parameters
(defparameter *processing-parameters*
  '(:title :tags :comment :overwritable #|:rotate :crop|#)
  "List of currently active tags.")

;; Each processing parameter has a handful of details
;; that are useful in doing something.
;; These are encoded in the symbol-plists for now,
;; but may they'll graduate to full objects or structures.
(setf
 ;; Corresponding Exiftool arguments.
 (get :title :exiftool-arg) "Title"
 (get :tags :exiftool-arg) "Subject"
 (get :comment :exiftool-arg) "Comment"

 ;; Some arguments can contain compound values
 ;; which are represented in list.
 ;; It is useful to have that marked out.
 (get :tags :compound-value) t

 ;; Default values for tags.
 (get :title :default) "Untitled"
 (get :overwritable :default) :overwrite)

;;; The class
(defclass wandering-page ()
  ((file :initarg :file
         :initform *books-location*
         :reader file
         :documentation "The file name that the wandering-page is tied to.")
   (processing-parameters :initform (make-hash-table)
                          :accessor processing-parameters)
   (paging-series :initarg :series
                  :initform ""
                  :accessor paging-series
                  :documentation "Which series this page belongs to.")
   (paging-behaviour :initarg :paging-behaviour
                     :initform ()
                     :accessor paging-behaviour
                     :documentation "How this page should be paged."))
  (:documentation "Represents a wandering page –
pages whose page numbers are unknown.
This is because they are tied to filenames that don't match any standard ones.
However, they often hold more information than their static counterparts –
they have titles, categories, genres, and all the stuff
that would be in the metadata (that can then be injected via exiftool.)"))

(defgeneric get-parameter (object parameter)
  (:method ((object wandering-page) parameter)
    (gethash parameter (processing-parameters object))))

(defgeneric (setf get-parameter) (value object parameter)
  (:method (value (object wandering-page) parameter)
    (setf (gethash parameter (processing-parameters object)) value)))

(defmethod initialize-instance :after ((object wandering-page)
                                       &rest initargs
                                       &key file paging-behaviour
                                       &allow-other-keys)
  (setf (slot-value object 'file)
        (etypecase file
          (string (uiop:parse-native-namestring file))
          (pathname file)
          (null *books-location*))
        (paging-behaviour object) paging-behaviour)
  (dolist (parameter *processing-parameters*)
    (setf (get-parameter object parameter)
          (getf initargs parameter (get parameter :default)))))

(defgeneric %format-wandering-page (object)
  (:method ((object wandering-page))
    (format nil "~:[no-filename~*~;~:*~a.~a~]~:[~;†~] \"~a\" ~a"
            (pathname-name (file object))
            (pathname-type (file object))
            (get-parameter object :overwritable)
            (get-parameter object :title)
            (get-parameter object :tags))))

(defmethod print-object ((object wandering-page) stream)
  (print-unreadable-object (object stream :type t)
    (princ (%format-wandering-page object) stream)))

;;; Married pages
(defclass married-page ()
  ((page-slot :initarg :page-slot
              :accessor page-slot)
   (metadata :initarg :metadata
             :accessor metadata))
  (:documentation "Represents a married page, which is a wandering-page
with a specified page-number."))

(defmacro define-married-page-method (reader-method)
  "Quick and dirty macro for defining reader methods for married-pages."
  `(defmethod ,reader-method ((object married-page))
     (,reader-method
      (funcall
       (cond ((find-method #',reader-method nil '(wandering-page) nil)
              #'metadata)
             ((find-method #',reader-method nil '(page) nil)
              #'page-slot)
             (t (error "No method found for the reader-method ~a"
                       #',reader-method))) object))))

(define-married-page-method file)
(define-married-page-method series-key)
(define-married-page-method format-page-code)
(define-married-page-method %format-wandering-page)
(defmethod get-parameter ((object married-page) parameter)
  (get-parameter (metadata object) parameter))
(defmethod (setf get-parameter) (value (object married-page) parameter)
  (setf (get-parameter (metadata object) parameter) value))

(defmethod print-object ((object married-page) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~a: ~a) ~a"
            (series-key object)
            (format-page-code object)
            (%format-wandering-page object))))

(defgeneric marry-page (wandering-page state)
  (:documentation "Assigns a page number to a wandering-page
 and puts them together into a married-page class.")
  (:method ((wandering-page wandering-page) (state null))
    (make-instance
     'married-page
     :metadata  wandering-page
     :page-slot (construct-book-object
                 (paging-series wandering-page)
                 (paging-behaviour wandering-page)
                 state))))

;;; Category determination
(defun tag-type (tag)
  "Finds the tag plist relating to the given tag."
  (with-expression-threading () *config*
    (assoc :tags :||) #'cdr
    (assoc tag :|| :test #'string=) #'cdr))

(defun genre (tags)
  "Finds the genre/type of all the tags given."
  (with-expression-threading ()
    (mapcar (lambda (tag) (-> tag tag-type (getf :type)))
            tags)
    (remove :special :||)
    (if (every #'eql :|| (cdr :||))
        (car :||) t)))

(defun get-name-variants (&rest variants)
  "Creates a function that, when given a tag,
runs through the list of variants and stops when a non-nil variant is found,
which it then returns. If all of them return nil, then nil is returned."
  (lambda (tag)
    (let ((tag-plist (tag-type tag)))
      (loop for i in variants
            when (getf tag-plist i) return it))))

(defun tag-manifestations (tags)
  "Computes the exact tag combination for all tags in the list."
  (destructuring-bind (&key metadata tumblr filename)
      (with-expression-threading () :tag-props
        (assoc :|| *config*) #'cdr
        (assoc (genre tags) :||) #'cdr)
    (list
     :metadata
     (cons metadata
           (remove nil (mapcar (get-name-variants :ascii-name :name)
                               tags)))
     :tumblr
     (cons tumblr
           (remove nil (mapcar (get-name-variants :tumblr :name)
                               tags)))
     :filename
     (let ((tags-less-specials
             (remove-if
              (lambda (a)
                (-> a tag-type (getf :type) (eql :special)))
              tags)))
       (if (= 1 (length tags-less-specials))
           (first tags-less-specials)
           filename)))))

;;; Filename conjoinment
(defgeneric construct-filename-with-metadata (page-slot wandering-page)
  (:documentation "Constructs the filename with the metadata provided.")
  (:method ((page-slot book-of-conworlds-page) (wandering-page wandering-page))
    (specificity-bind ((page :page) (subpage :subpage)) page-slot
      (format nil "~2,'0d~c-~a-~a"
              page
              (number->letter subpage)
              (-> wandering-page tags tag-manifestations (getf :filename))
              (title wandering-page))))
  (:method ((page-slot page) wandering-page)
    (declare (ignore wandering-page))
    ;; For the others, the metadata does not impact the filename
    (construct-filename page-slot)))

(defgeneric get-path-with-metadata (page-slot wandering-page)
  (:documentation "Constructs the complete path with the metadata provided.")
  (:method ((page-slot page) (wandering-page wandering-page))
    (merge-pathnames
     (make-pathname
      :directory (list :relative (construct-folder-name page-slot))
      :name (construct-filename-with-metadata page-slot wandering-page)
      :type (pathname-type (file wandering-page)))
     (root page-slot))))

;;; Make argument:
(defun write-argument (stream option &optional operator (value ""))
  "Writes an argument of the form -OPTION=VALUE into STREAM.
SET-OPERATOR determines if it is an \"append\" (+=) or \"set\" (-) operation."
  (format stream "-~a~a~a"
          option
          (ecase operator (:append "+=") (:set "=") ((nil) ""))
          value))

(defun write-exiftool-argument (stream option &optional operator (value ""))
  "Like write-argument, but always writes the argument on a freshline."
  (fresh-line stream)
  (write-argument stream option operator value))

(defun format-exiftool-args (stream option value\(s\))
  "Writes the arguments that correspond to setting the OPTION to VALUE(S).
If VALUE(S) is a list, then write arguments that first clears the original value
and then appends each value to the thing."
  (if (listp value\(s\))
      ;; A list of values means that appending is needed.
      (loop initially (write-exiftool-argument
                       stream (get option :exiftool-arg) :set)
            for i in value\(s\)
            do (write-exiftool-argument
                stream (get option :exiftool-arg) :append i))
      ;; else just set it directly
      (write-exiftool-argument
       stream (get option :exiftool-arg) :set value\(s\))))

(defun dump-exiftool-args (file wandering-page &optional (newp t))
  "Dumps all exiftool args to some file,
that exiftool can then read again through the -@ option."
  (with-open-file (open-file file :direction :output
                                  :if-does-not-exist :create
                                  :if-exists (if newp :supersede :append)
                                  :external-format :utf-8)
    ;; Title
    (format-exiftool-args open-file :title
                          (get-parameter wandering-page :title))
    ;; Comments
    (format-exiftool-args open-file :comment
                          (get-parameter wandering-page :comment))
    ;; Tags/Categories/Subjects
    (format-exiftool-args open-file :tags
                          (-> wandering-page
                            (get-parameter :tags)
                            (getf :metadata)))
    ;; Overwrite or not
    (case (get-parameter wandering-page :overwritable)
      (:overwrite-preserving-metadata
       (write-exiftool-argument open-file "overwrite_original_in_place"))
      (:overwrite
       (write-exiftool-argument open-file "overwrite_original")))
    ;; The file name to be processed
    (format open-file "~&~a"
            (-> wandering-page file namestring uiop:native-namestring))
    ;; Filename separator
    (write-exiftool-argument open-file "execute")))
