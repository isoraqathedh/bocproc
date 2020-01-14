(in-package #:info.isoraqathedh.bocproc.core)

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
  (if *print-readably*
      (prin1 (listify object) stream)
      (print-unreadable-object (object stream :type t)
        (format stream "~s" (slug-symbol object)))))

(defparameter *stable-entities* ())

(defun get-stable-entity (name &optional type)
  (let ((thing (find name *stable-entities* :key #'slug-symbol)))
    (if (null type) thing
        (and (typep thing type) thing))))

(defun simple-character-p (character)
  (find character (concatenate 'string
                               (string-upcase +alphabet+)
                               "0123456789-_")))

(defgeneric make-stable-entity (type args))

(defun store-stable-entity (serialised-entity)
  (pushnew (make-stable-entity (first serialised-entity)
                               (rest serialised-entity))
           *stable-entities*
           :key #'slug-symbol))

(defmacro define-stable-entity-function (type-symbol)
  `(defmacro ,type-symbol (&rest construction-args)
     `(funcall #'make-stable-entity ',',type-symbol
               ',construction-args)))

;;; Affinity
(defclass affinity (stable-entity)
  ((name :accessor name
         :initarg :name)))

(defmethod listify append ((object affinity))
  (list :name (name object)))

(defmethod make-stable-entity ((affinity (eql 'bpc-entities::affinity)) args)
  (destructuring-bind (slug-symbol &key name) args
    (make-instance 'affinity :slug-symbol slug-symbol
                             :name name)))

(define-stable-entity-function bpc-entities::affinity)

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
  `(,(slug-symbol (affinity object))
    :name ,(name object)
    ,@(slot-value object 'other-properties)))

(defmethod make-stable-entity ((tag (eql 'bpc-entities::tag)) args)
  (destructuring-bind (slug-symbol affinity
                       &rest all-args &key name &allow-other-keys) args
    (remf all-args :name)
    (make-instance 'tag
                   :affinity (get-stable-entity affinity 'affinity)
                   :slug-symbol slug-symbol
                   :name name
                   :props all-args)))

(define-stable-entity-function bpc-entities::tag)

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

(defmethod make-stable-entity ((tag (eql 'bpc-entities::series)) args)
  (destructuring-bind (slug-symbol
                       &key root filename-syntax page-specification)
      args
    (make-instance 'series :slug-symbol slug-symbol
                           :root root
                           :filename-syntax filename-syntax
                           :page-specification page-specification)))

;;; Page
(defclass page ()
  ((page-number :accessor page-number
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

(defmethod listify append ((object page-number))
  (cons (base object) (coerce (page-number object) 'list)))

;;; Sorting
(defun entity< (left right)
  (cond ((not (eql (type-of left) (type-of right)))
         (let ((type-order '(affinity tag series))
               (sentinel 999))
           (< (or (position (type-of left) type-order :test #'eql) sentinel)
              (or (position (type-of right) type-order :test #'eql) sentinel))))
        ;; More sorting to come later
        ;; Default: compare slugs
        (t
         (string-lessp (symbol-name (slug-symbol left))
                       (symbol-name (slug-symbol right))))))
