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
