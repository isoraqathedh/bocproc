;;;; boc-meta.lisp
;;;; This file is not used for the moment.

(in-package #:bocproc)

(defparameter *books-location*
  (merge-pathnames
   (make-pathname :directory '(:relative
                               "Documents" "My Scans" "Book of Conworlds"))
   (uiop/common-lisp:user-homedir-pathname)))

(defun book-number-path (number)
  (merge-pathnames
   (make-pathname :directory (list :relative (format nil "Book ~d" number))
                  :name :wild
                  :type "jpg")
   *books-location*))

(defun page (book page &optional (subpage "a"))
  (find-if #'(lambda (file)
               (string= (format nil "~2,'0d~a-" page subpage)
                        (file-namestring file) :end2 4))
           (directory (book-number-path book))))

(defun page-timestamps (book-number)
  (mapcar #'(lambda (file)
              (local-time:universal-to-timestamp
               (file-write-date file)))
          (directory (book-number-path book-number))))

(defun map-page-in-book (book-number key)
  (mapcar key (directory (book-number-path book-number))))

(defun collate (data sort-function &key default by-frequency)
  (let ((zipper (make-hash-table)))
    (dolist (i data)
      (incf (gethash i zipper default)))
    (sort (loop for k being the hash-keys of zipper
                for v being the hash-values of zipper
                collect (cons k v))
          (if by-frequency #'< sort-function)
          :key (if by-frequency #'cdr #'car))))
