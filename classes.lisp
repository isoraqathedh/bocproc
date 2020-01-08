(in-package #:info.isoraqathedh.bocproc.core)

(defvar +bocproc-uuid+
  (uuid:make-uuid-from-string "87832309-5EE5-48D0-B2C7-41E88531B360"))

(defvar +alphabet+ "abcdefghijklmnopqrstuvwxyz")

(defclass stable-entity ()
  ((slug-symbol :accessor slug-symbol
                :initarg :slug-symbol
                :type 'symbol)))

(defmethod print-object ((object stable-entity) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (slug-symbol object))))

(defparameter *stable-entities* ())

(defun push-stable-entity (stable-entity)
  (pushnew stable-entity *stable-entities* :key #'slug-symbol))

(defun get-stable-entity (name &optional type)
  (let ((thing (find name *stable-entities* :key #'slug-symbol)))
    (if (null type) thing
        (and (typep thing type) thing))))

(defclass affinity (stable-entity)
  ((name :accessor name
         :initarg :name)))

(defun simple-character-p (character)
  (find character (concatenate 'string
                       (string-upcase +alphabet+)
                       "0123456789-_")))

(defun slugify (string)
  (let* ((slug-package '#:info.isoraqathedh.bocproc.entities)
         (slug-symbol
           (thread-expr:with-expression-threading (x)
             (string-upcase string)
             (substitute #\- #\Space x)
             (substitute-if-not #\_ #'simple-character-p x)
             (string-trim "-" x)
             (or (find-symbol x slug-package)
                 (intern x slug-package)))))
    (export slug-symbol slug-package)
    slug-symbol))

(defun push-stable-entity (name slug type &rest type-initargs)
  (let ((true-slug (etypecase slug
                     (null (slugify name))
                     (string (slugify slug))
                     (symbol slug)))
        instance)
    (or (get-stable-entity true-slug type)
        (progn
          (setf instance (apply #'make-instance type :name name
                                                     :slug true-slug
                                                     type-initargs))
          (push instance *stable-entities*)
          instance))))

(defun define-affinity (name &optional slug)
  (push-stable-entity name slug 'affinity))

(defclass tag (stable-entity)
  ((name :accessor name
         :initarg :name)
   (affinity :accessor affinity
             :type 'affinity
             :initarg :affinity)
   (other-properties :initform ()
                     :initarg :props)))

(defun define-tag (name affinity &optional slug)
  (let ((affinity (get-stable-entity affinity 'affinity))
        (true-slug (etypecase slug
                     (null (slugify name))
                     (string (slugify slug))
                     (symbol slug))))
    (or (get-stable-entity true-slug 'tag)
        )))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s @ ~s"
            (slug-symbol object)
            (slug-symbol (affinity object)))))

(defclass series (stable-entity)
  ((name :accessor name
         :initarg :name)
   (root :accessor root
         :initarg :root)
   (filename-syntax :accessor filename-syntax
                    :initarg :filename-syntax)
   (page-specification :accessor page-specification
                       :initarg :page-specification)))

(defclass page ()
  ((page-number :accessor :name
                :initarg :page-number)
   (uuid :reader uuid)
   (tags :accessor tags
         :initarg :tags)
   (title :accessor title
          :initarg :title)
   (comment :accessor comment
            :initarg :comment)
   (create-date :accessor create-date
                :initform (local-time:now))))

(defclass page-number ()
  ((base :accessor base
         :initarg :base)
   (numbers :accessor numbers)))
