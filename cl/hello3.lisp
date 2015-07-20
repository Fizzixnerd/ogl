(in-package :glf)

(defparameter *vao* nil)
(setf *vao* (VAO))

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

(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape)
	     (eq action :press))
    (set-window-should-close)))

(defclass VAO ()
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (va :accessor vertex-array)
   (program :accessor program)
   (angle :accessor angle :initform 0.0)))

(defun prerender (vao)
  (let ((arr (alloc-gl-array :float 9))
	(verts #(-1.0 -1.0  0.0
		 0.0  1.0  0.0
		 1.0  1.0  0.0))
	(buffers (gen-buffers 2)))
    (setf (vertex-buffer vao) (elt buffers 0)
	  (index-buffer vao) (elt buffers 1))
    (bind-buffer :array-buffer (vertex-buffer vao))
    (dotimes (i (length verts))
      (setf (glaref arr i) (aref verts i)))
    (buffer-data :array-buffer :static-draw arr)
    (free-gl-array arr))
  (bind-buffer :array-buffer 0)

  (setf (vertex-array vao) (gen-vertex-array))
  (bind-vertex-array (vertex-array vao))

  (bind-buffer :array-buffer (vertex-buffer vao))
  (enable-vertex-attrib-array 0)
  (vertex-attrib-pointer 0 3 :float nil 0 (null-pointer))
  (bind-vertex-array 0)

  (let ((vs (create-shader :vertex-shader))
	(fs (create-shader :fragment-shader))
	(vs-text (with-open-file (vs-stream "vs.glsl")
		   (dump-stream-into-string vs-stream)))
	(fs-text (with-open-file (fs-stream "fs.glsl")
		   (dump-stream-into-string fs-stream))))
    (setf (vertex-shader vao) vs
	  (fragment-shader vao) fs)
    (shader-source vs vs-text)
    (compile-shader vs)
    (shader-source fs fs-text)
    (compile-shader fs)

    (print (get-shader-info-log vs))
    (print (get-shader-info-log fs))

    (setf (program vao) (create-program))

    (attach-shader (program vao) vs)
    (attach-shader (program vao) fs)

    (link-program (program vao))
    (use-program (program vao))))

(defmethod render ()
  (clear :color-buffer)

  (bind-buffer :array-buffer (vertex-buffer *vao*))
  (draw-arrays :triangles 0 3))

(defmethod set-viewport (width height)

  (viewport 0 0 width height)
  (matrix-mode :projection)
  (load-identity)
  (ortho -50 50 -50 50 -1 1)
  (matrix-mode :modelview)
  (load-identity))

(def-window-size-callback window-size-callback (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun basic-window-example ()
  (with-init-window (:title "Window Test"
		     :width 600 :height 400
		     :samples 4
		     :context-version-major 2
		     :context-version-minor 0)
    (setf %gl:*gl-get-proc-address* #'get-proc-address)
    (set-key-callback 'key-callback)
    (set-window-size-callback 'window-size-callback)
    (clear-color 0 0 0 0)
    (prerender *vao*)
    (iter
      (until (window-should-close-p))
      (render)
      (swap-buffers)
      (poll-events))))

(basic-window-example)
