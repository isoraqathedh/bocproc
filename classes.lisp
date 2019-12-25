(in-package #:info.isoraqathedh.bocproc.core)

(defvar +bocproc-uuid+
  (uuid:make-uuid-from-string "87832309-5EE5-48D0-B2C7-41E88531B360"))

(defclass stable-entity ()
  ((slug-symbol :accessor slug-symbol
                :initarg :slug-symbol
                :type 'symbol)))

(defmethod print-object :around ((object stable-entity) stream)
  (print-unreadable-object (object stream :type t)
    (call-next-method)))

(defmethod print-object ((object stable-entity) stream)
  (format stream "~s" (slug-symbol object)))

(defclass affinity (stable-entity)
  ((name :accessor name
         :initarg :name)))

(defclass tag (stable-entity)
  ((name :accessor name
         :initarg :name)
   (affinity :accessor affinity
             :type 'affinity
             :initarg :affinity)
   (other-properties :initform ()
                     :initarg :props)))

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
