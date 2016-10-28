;;;; tumblr-integration.lisp

(in-package #:bocproc)

(defun post-to-tumblr (file comments tags)
  "Post image(s) to tumblr, and retrieve the post object."
  (humblr:post
   (humbler:blog/post-photo
    (config :blog)
    file
    :state :queue
    :caption comments
    :tags tags)
   (config :blog)))

(defun tumblr-photos-size (photo)
  "Retrieve the image URL of the largest image in a single image object."
  (humbler:url
   (first ; The first size is the original one
    (humbler:sizes photo))))

(defun photo-URLs (post)
  "Retrieve the image URLs of the tumblr post."
  (mapcar #'tumblr-photos-size (humbler:photos post)))

(defun construct-tumblr-post (images caption tags)
  "Create a tumblr image post using the Humbler API."
  (humbler:blog/post-photo )
  (make-instance
   'humbler:photo-post
   :photos (make-instance 'humbler:photo )
   :state :queue
   :caption ))
