(in-package #:info.isoraqathedh.bocproc.core)

;;; Page objects
(defclass page-details () ())

(defclass page-available (page-details) ())
(defconstant +page-available+ (make-instance 'page-available))

(defclass page-ignored (page-details) ())
(defconstant +page-ignored+ (make-instance 'page-ignored))

(defclass page-info (page-details)
  ((title :accessor title
          :initform ""
          :type 'string)
   (tags :accessor tags
         :initform ()
         :type 'list)
   (comment :accessor comment
            :initform ""
            :type 'string)
   (register-date :accessor register-date
                  :initform nil)
   (filename :accessor filename
             :initform nil)
   (entry-date :accessor entry-date
               :initform nil)
   (start-date :accessor start-date
               :initform nil)
   (finish-date :accessor finish-date
                :initform nil)
   (scan-date :accessor scan-date
              :initform nil)))

(defclass page ()
  ((page-number :accessor page-number
                :initarg :number
                :type 'page-number)
   (info :accessor info
         :initarg :info
         :type 'page-details)))

;;; Page storage and loading
(defvar *page-database*
  (make-hash-table :test 'equal))

(defun register-page-record (record)
  (destructuring-bind
      (&key page scan-date title subject comment location &allow-other-keys)
      record
    (let ((info (make-instance 'page-info))
          (num (make-instance 'page-number
                              :base (get-stable-entity (first page)))))
      (setf (page-number num) (rest page)

            (title info) title
            (comment info) comment
            (filename info) location

            (scan-date info)
            (local-time:parse-timestring scan-date
                                         :date-time-separator #\Space)

            (tags info)
            (mapcar #'get-stable-entity subject))
      (setf (gethash (url num) *page-database*) info))))

;;; Page finding
(defgeneric find-page-info (page-number)
  (:method ((page-number page-number))
    (cond
      (t #||Case if the page is ignored||#
       +page-ignored+)
      (t #||Case if page is available||#
       +page-available+)
      (t #||Case if page is already in use||#))))

(defgeneric imbue-page-info (page-number)
  (:method ((page-number page-number))
    (make-instance 'page
                   :number page-number
                   :info (find-page-info page-number))))
