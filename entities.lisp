(in-package #:info.isoraqathedh.bocproc.core)

(defvar +bocproc-uuid+
  (uuid:make-uuid-from-string "87832309-5EE5-48D0-B2C7-41E88531B360"))

(defvar +alphabet+ "abcdefghijklmnopqrstuvwxyz")

;;; Generic : Stable entity
(defclass stable-entity ()
  ((slug-symbol :accessor slug-symbol
                :initarg :slug-symbol
                :type 'symbol)))

(defgeneric listify (object)
  (:method-combination append :most-specific-last)
  (:method append ((object stable-entity))
    (list (slugify (symbol-name (type-of object)))
          (slug-symbol object))))

(defmethod print-object ((object stable-entity) stream)
  (prin1 (listify object) stream))

(defparameter *stable-entities* ())

(defun get-stable-entity (name &optional type)
  (let ((thing (find name *stable-entities* :key #'slug-symbol)))
    (if (null type) thing
        (and (typep thing type) thing))))

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

(defun simple-character-p (character)
  (find character (concatenate 'string
                               (string-upcase +alphabet+)
                               "0123456789-_")))

;;; Affinity
(defclass affinity (stable-entity)
  ((name :accessor name
         :initarg :name)))

(defun define-affinity (name &optional slug)
  (push-stable-entity name slug 'affinity))

(defmethod listify append ((object affinity))
  (list :name ,(name object)))

;;; Tag
(defclass tag (stable-entity)
  ((name :accessor name
         :initarg :name)
   (affinity :accessor affinity
             :type 'affinity
             :initarg :affinity)
   (other-properties :initform ()
                     :initarg :props)))

(defmethod listify append ((object tag))
  `(:name ,(name object)
    :affinity ,(slug-symbol (affinity object))
    ,@(slot-value object 'other-properties)))

(defun define-tag (name affinity &optional slug)
  (let ((affinity (get-stable-entity affinity 'affinity))
        (true-slug (etypecase slug
                     (null (slugify name))
                     (string (slugify slug))
                     (symbol slug))))
    (or (get-stable-entity true-slug 'tag)
        )))

;;; Series
(defclass series (stable-entity)
  ((name :accessor name
         :initarg :name)
   (root :accessor root
         :initarg :root)
   (filename-syntax :accessor filename-syntax
                    :initarg :filename-syntax)
   (page-specification :accessor page-specification
                       :initarg :page-specification)))

(defmethod listify append ((object series))
  (list :name (name object)
        :root (root object)
        :filename-syntax (filename-syntax object)
        :page-specification (page-specification object)))

;;; Page
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

;;; Page number
(defclass page-number ()
  ((base :accessor base
         :initarg :base)
   (numbers :accessor numbers)))
