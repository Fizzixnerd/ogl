(in-package :glf)

(defclass Hello-Window (glut:window)
  ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 250 :height 250
		     :mode '(:single :rgb) :title "hello, world!"))

(defmethod glut:display-window :before ((w Hello-Window))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:display ((w Hello-Window))
  (gl:clear :color-buffer)

  (gl:color 1 1 1)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex 0.75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))

  (gl:flush))

(defun rb-hello ()
  (glut:display-window (Hello-Window)))

(rb-hello)
