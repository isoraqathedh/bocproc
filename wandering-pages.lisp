;;;; Wandering pages

#| This file deals with "wandering pages"
– see documentation of the class for more information. |#

(in-package :bocproc)

;;; The class
(defclass wandering-page ()
  ((file :initarg :file
         :initform *books-location*
         :reader file
         :documentation "The file name that the wandering-page is tied to.")
   (title :initarg :title
          :initform "Untitled"
          :accessor title)
   (categories :initarg :categories
               :initform ()
               :accessor categories
               :documentation "A list of tags related to the object."))
  (:documentation "Represents a wandering page –
pages whose page numbers are unknown.
This is because they are tied to filenames that don't match any standard ones.
However, they often hold more information than their static counterparts –
they have titles, categories, genres, and all the stuff
that would be in the metadata (that can then be injected via exiftool.)"))

(defmethod print-object ((object wandering-page) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~:[no-filename~*~;~:*~a.~a~] \"~a\" ~a"
            (pathname-name (file object))
            (pathname-type (file object))
            (title object)
            (categories object))))

(defmethod initialize-instance :after ((object wandering-page) &key file)
  (setf (slot-value object 'file)
        (etypecase file
          (string (uiop:parse-native-namestring file))
          (pathname file))))

;;; Category determination
;;; Filename conjoinment
