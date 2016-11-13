;;;; tumblr-integration.lisp

(in-package #:bocproc)

(defun post-image-to-tumblr (file comments tags)
  "Post image(s) to tumblr, and retrieve the post object."
  (humbler:post
   (humbler:blog/post-photo
    (config :blog)
    file
    :state :queue
    :caption comments
    :tags (getf (tag-manifestations tags) :tumblr))
   (config :blog)))

(defun tumblr-photos-size (photo)
  "Retrieve the image URL of the largest image in a single image object."
  (humbler:url
   (first ; The first size is the original one
    (humbler:sizes photo))))

(defun photo-URLs (post)
  "Retrieve the image URLs of the tumblr post."
  (mapcar #'tumblr-photos-size (humbler:photos post)))

(defun find-name (name state)
  "Find the book-page with name NAME in STATE."
  (find name (files-to-process state)
        :key (lambda (book-page)
               (get-page-property book-page :name))))

(defun find-names (names state)
  "Find the book-pages with name NAMES in STATE."
  (loop for i in names
        collect (or (find-name i state)
                    (cerror "Name ~s not found in ~s" i state))))

(defun construct-multi-post (book-pages override-caption override-tags)
  "Make a multiple-image tumblr post template."
  (make-instance 'multi-image-tumblr-post
                 :files-to-process
                 (mapcar (get-page-property-function :file) book-pages)
                 :net-caption
                 (or override-caption
                     (with-expression-threading ()
                       (mapcar (get-page-property-function :comment) book-pages)
                       (remove "" :||)
                       (remove nil :||)
                       (if (or (= 1 (length :||))
                               (every #'string= :|| (cdr :||)))
                           (first :||)
                           (error "Conflicting captions found: ~s" :||))))
                 :net-tags
                 (or override-tags
                     (reduce #'union book-pages
                             :key (get-page-property-function :tags)))))

(defgeneric %post-to-tumblr (thing)
  (:documentation "Post THING to tumblr.

If it is a single book-page, create a post with one image in it.
If it is a multi-image-tumblr-post, post that image.")
  (:method ((thing book-page))
    (when (get-page-property thing :tumblr)
      (let ((post (post-image-to-tumblr (get-page-property thing :file)
                                        (get-page-property thing :comment)
                                        (get-page-property thing :tags))))
        (setf (get-page-property thing :image-url)
              (first (photo-URLs post))))))
  (:method ((thing multi-image-tumblr-post))
    (let ((post (post-image-to-tumblr (files-to-process thing)
                                      (net-caption thing)
                                      (net-tags thing))))
      (loop for page in (files-to-process thing)
            for url in (photo-URLs post)
            do (setf (get-page-property page :image-url) url)))))
