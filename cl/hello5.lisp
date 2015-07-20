(in-package :glf)

(defun eofp (stream)
  (let ((next-char (read-char stream nil nil)))
    (when next-char
      (unread-char next-char stream))
    (not next-char)))

(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))

(defmacro strcatf (place &rest strings)
  (let ((p place))
    `(setf ,p (strcat ,p ,@strings))))

(defun dump-stream-into-string (stream)
  (let ((str ""))
    (iter
      (until (eofp stream))
      (strcatf str (read-line stream) (format nil "~c" #\Newline)))
    str))

(defclass MyWindow (glut:window)
  ()
  (:default-initargs :width 1024 :height 768
		     :mode '(:double :rgb) :title "Tutorial 1"
		     :samples 4 :context-version-major 2
		     :context-version-minor 0))

(defclass VAO ()
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (va :accessor vertex-array)
   (program :accessor program)
   (angle :accessor angle :initform 0.0)))

(defparameter *vao* nil)
(setf *vao* (VAO))

(defmethod glut:display-window :before ((w MyWindow))
  (clear-color 0 0 0 0)
  (matrix-mode :projection)
  (load-identity)
  (ortho -1 1 -1 1 -1 1)
  (prerender *vao*))

(defmethod glut:display ((w MyWindow))
  (clear :color-buffer)

  (bind-buffer :array-buffer (vertex-buffer *vao*))
  (draw-arrays :triangles 0 3)
  (glut:swap-buffers))

(defun rb-hello ()
  (glut:display-window (MyWindow)))

(rb-hello)
  
