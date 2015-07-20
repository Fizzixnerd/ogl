(in-package :glf)

(defun make-hello-window ()
  (create-window :width 1024 :height 768 :title "Hello Window!"))

(defun hello2-main ()
  (when (not (glfw:initialize))
    (error "Failed to start GLFW.")
    (return-from hello2-main nil))

  (make-hello-window)
  (gl:viewport 0 0 1024 768)
  (gl:clear)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1)
  (gl:load-identity)

  (glfw:show-window))

(hello2-main)
