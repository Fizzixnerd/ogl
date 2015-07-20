(in-package :glf)

(defmethod set-viewport (width height)
  (gl:viewport 0 0 width height)
  ;; problem is the line below.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun hello4-main ()
  (when (not (glfw:initialize))
      (error "Failed to initialize GLFW.")
      (return-from hello4-main nil))
  
  (with-init-window (:width 1024
		     :height 768
		     :title "Tutorial 01"
		     :samples 4
		     :context-version-major 3
		     :context-version-minor 3
		     :opengl-forward-compat t
		     :opengl-profile :opengl-core-profile)
    (when (not *window*)
      (error "Failed to open GLFW window.  Oh noes!")
      (return-from hello4-main nil))
    ;(setf %gl:*gl-get-proc-address* #'get-proc-address)
    (gl:clear-color 0 0 0 0)
    (set-viewport 1024 768)
    (set-input-mode :sticky-keys t)
    (iterate:iter
      (until (window-should-close-p))
      (swap-buffers)
      (poll-events)
      (when (get-key :escape)
	(set-window-should-close)))
    (terminate)))

(hello4-main)
