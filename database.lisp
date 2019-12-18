(in-package :bocproc)

(defun create-query ()
  (with-output-to-string (x)
    (dolist (tag (cdr (assoc :tags *config*)))
      (let ((tag-name (string-downcase (symbol-name (car tag))))
            (tag-plist (cdr tag)))
        ;; First, all the normal things
        (format x "INSERT INTO topic_symbols
(lisp_name, short_symbol, full_name, symbol_type)
VALUES ('~(~a~)', '~a', '~a', ~a);~%"
                tag-name
                (getf tag-plist :symbol)
                (getf tag-plist :name)
                (case (getf tag-plist :type)
                  (:language 1)
                  (:world 2)
                  (:special 3)
                  (t "NULL")))
        ;; Then alter the status
        (when (getf tag-plist :status)
          (format x "UPDATE topic_symbols SET status = ~a WHERE lisp_name = '~a';~%"
                  (case (getf tag-plist :status)
                    (:future 2)
                    (t "NULL"))
                  tag-name))
        ;; Then if there's anything else, throw it in the other database.
        (loop with main-tags = '(:symbol :name :type :status)
              for (k v) on tag-plist by #'cddr
              if (not (member k main-tags))
              do (format x "INSERT INTO topic_symbols_extra (topic_symbol, key, value)
VALUES ('~a', '~(~a~)', '~a');~%"
                         tag-name k v))))
    (format x "UPDATE topic_symbols SET status = 1 WHERE status IS NULL;~%")))

(defvar *database-file* )
