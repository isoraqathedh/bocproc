(in-package :bocproc-exp)

(defclass book-series ()
  ((series
    :type symbol
    :initarg :series
    :documentation "Series identifier. A keyword.")
   (specificities
    :initarg :specificities
    :documentation #.(concatenate 'string
                                  "An n×3 array"
                                  " with the names of the specificities"
                                  " and also their cutoffs. "
                                  " The rows are symbol, min, max."))
   (format
    :initarg :format
    :documentation "The format of the filenames of this series."))
  (:documentation "Class for defining book series."))

(defclass book-page (book-series)
  ((page-numbers :initarg :page-numbes
                 :initform (vector)
                 :type (vector number))))
