;;;; objects.lisp

#| File for defining objects. |#

(in-package #:bocproc)

;;; Books and series
(defclass book-series ()
  ((series
    :type symbol
    :initarg :series
    :documentation "Series identifier. A keyword."
    :reader series)
   (specificities
    :initarg :specificities
    :documentation #.(concatenate 'string
                                  "An n×3 array"
                                  " with the names of the specificities"
                                  " and also their cutoffs. "
                                  " The rows are symbol, min, max.")
             :reader specificities)
   (format
    :initarg :format
    :documentation "The format of the filenames of this series."
    :reader book-format))
  (:documentation "Class for defining book series."))

(defmethod print-object ((object book-series) stream)
  (with-slots (series) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a" series))))

(defclass book-page (book-series)
  ((page-numbers :initarg :page-numbers
                 :initform (vector)
                 :type (vector number)
                 :accessor page-numbers)
   (properties :initarg properties
               :initform (make-hash-table)
               :accessor properties))
  (:documentation "A manifestation of a book-series,
with a definite page number."))

(defmethod print-object ((object book-page) stream)
  (with-accessors ((properties properties)) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a ~a" (series object) (page-numbers object))
      (loop for k being the hash-keys of properties
            for v being the hash-values of properties
            do (format stream "~_~s ~s" k v)))))

(defclass page-generator (book-page)
  ((locally-ignored
    :initform nil
    :initarg :locally-ignored
    :accessor locally-ignored
    :documentation "A list of page numbers that are locally ignored.")
   (fresh
    :initform nil
    :accessor fresh
    :documentation "A flag showing whether or not there are no pages filled."))
  (:documentation "A generator of page numbers."))

(defmethod print-object ((object page-generator) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a ~{~a~^ ~}" (series object) (page-numbers object))))

;;; BPC files
(defclass bocproc-state ()
  ((version :initarg :version
            :initform nil
            :reader state-version)
   (files-to-process :initform ()
                     :accessor files-to-process)
   (current-page :initform (make-hash-table :test #'equal)
                 :accessor current-page)
   (generator :initarg :generator
              :initform nil
              :accessor generators)
   (verbose :initform t
            :accessor verbosep
            :documentation #.(concatenate
                              'string
                              "Determines whether or not "
                              "the motions should be printed.")))
  (:documentation "An object that represents the state of the processor."))

(defclass multi-image-tumblr-post ()
  ((files-to-process :initarg :files-to-process
                     :initform nil
                     :reader files-to-process)
   (net-caption :initarg :net-caption
                :initform nil
                :reader net-caption
                :documentation "The overall caption that the post will have.")
   (net-tags :initarg :net-tags
             :initform nil
             :reader net-tags
             :documentation "The overall tags that the post will have."))
  (:documentation "An object representing a multiple-image tumblr post.

FILES-TO-PROCESS contain a list of regular page objects
which all have their usual properties.
NET-CAPTION and NET-TAGS represent the overall caption and tags
that the final image post will have."))
