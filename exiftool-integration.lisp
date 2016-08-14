;;;; Exiftool integration

#| Allows scanned pages to have EXIF data embedded into them via exiftool.
 |#

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
 (get :title 'exiftool-arg) "Title"
 (get :tags 'exiftool-arg) "Subject"
 (get :comment 'exiftool-arg) "Comment"
 (get :date-of-creation 'exiftool-arg) "CreateDate"

 ;; Some arguments can contain compound values
 ;; which are represented in list.
 ;; It is useful to have that marked out.
 (get :tags 'compound-value) t

 ;; Default values for tags.
 (get :title 'default) "Untitled"
 (get :overwritable 'default) :overwrite)


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

;;; Make argument:
(defun write-argument (stream option &optional operator (value ""))
  "Writes an argument of the form -OPTION=VALUE into STREAM.
SET-OPERATOR determines if it is an \"append\" (+=) or \"set\" (-) operation."
  (format stream "-~a~a~a"
          option
          (ecase operator (:append "+=") (:set "=") ((nil) ""))
          value))

(defun write-exiftool-argument (stream option &optional operator (value ""))
  "Like write-argument, but leaves a new line at the end."
  (write-argument stream option operator value)
  (terpri stream))

(defun format-exiftool-args (stream option value\(s\))
  "Writes the arguments that correspond to setting the OPTION to VALUE(S).
If VALUE(S) is a list, then write arguments that first clears the original value
and then appends each value to the thing."
  (if (listp value\(s\))
      ;; A list of values means that appending is needed.
      (loop initially (write-exiftool-argument
                       stream (get option 'exiftool-arg) :set)
            for i in value\(s\)
            do (write-exiftool-argument
                stream (get option 'exiftool-arg) :append i))
      ;; else just set it directly
      (write-exiftool-argument
       stream (get option 'exiftool-arg) :set value\(s\))))

(defun %dump-exiftool-args (stream page)
  "Dumps all exiftool args to some stream."
  ;; Time
  (format-exiftool-args
   stream :date-of-creation
   (format-timestring
    nil (or (get-page-property page :date-of-creation) (now))
    :format '((:year 4) #\: (:month 2) #\: (:day 2) #\Space
              (:hour 2) #\: (:min 2) #\: (:sec 2) :gmt-offset)
    :timezone (get-timezone)))
  ;; Title
  (format-exiftool-args
   stream :title (get-page-property page :title))
  ;; Comments
  (format-exiftool-args
   stream :comment (get-page-property page :comment))
  ;; Tags/Categories/Subjects
  (format-exiftool-args
   stream :tags (-> page
                  (get-page-property :tags)
                  tag-manifestations
                  (getf :metadata)))
  ;; Overwrite or not
  (case (get-page-property page :overwritable)
    (:overwrite-preserving-metadata
     (write-exiftool-argument stream "overwrite_original_in_place"))
    (:overwrite
     (write-exiftool-argument stream "overwrite_original")))
  ;; The file name to be processed
  (format stream "~a~%"
          (-> page (get-page-property :file) namestring uiop:native-namestring))
  ;; Filename separator
  (write-exiftool-argument stream "execute"))

(defun dump-exiftool-args (file page &optional (newp t))
  "Dumps all exiftool args to some file,
that exiftool can then read again through the -@ option."
  (with-open-file (open-file file :direction :output
                                  :if-does-not-exist :create
                                  :if-exists (if newp :supersede :append)
                                  :external-format :utf-8)
    (%dump-exiftool-args open-file page)))
