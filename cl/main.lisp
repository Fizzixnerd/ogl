(in-package :glf)

(defun make-gl-array (type count &optional init-contents)
  (let ((arr (alloc-gl-array type count))
	(index 0))
    (iter
      (for el in-sequence init-contents)
      (setf (glaref arr index) el)
      (incf index))
    arr))

(defparameter triangle-attributes (make-gl-array :float 9 #(0.0 1.0 0.0
							    1.0 -1.0 0.0
							    -1.0 -1.0 0.0)))
(defparameter vbo-triangle nil)
(defparameter program nil)
(defparameter vs nil)
(defparameter fs nil)
(defparameter attribute-pos nil)

(defclass MyWindow (glut:window)
  ()
  (:default-initargs :width 1280 :height 720
		     :mode '(:double :rgb :alpha :depth)
		     :context-version-major 2 :context-version-minor 0))

(defmethod glut:display-window :before ((w MyWindow))
  (setf program (create-program))
  (setf vs (make-shader "vs.glsl" :vertex-shader))
  (setf fs (make-shader "fs.glsl" :fragment-shader))
  
  (attach-shader program vs)
  (attach-shader program fs)
  (link-program program)
  (print (get-program-info-log program))
  (use-program program)

  (setf attribute-pos (get-attrib-location program "pos"))
  (print attribute-pos)
  (when (= attribute-pos -1)
    (error "~s could not be bound!" 'attribute-pos))
  
  (setf vbo-triangle (gen-buffers 1))
  (setf vbo-triangle (car vbo-triangle))
  (bind-buffer :array-buffer vbo-triangle)
  (buffer-data :array-buffer :static-draw triangle-attributes)
  (bind-buffer :array-buffer 0))

(defmethod glut:display ((w MyWindow))
  (clear-color 0.0 0.0 0.0 0.0)
  (clear :color-buffer :depth-buffer)
  (enable :blend :depth-test)
  (blend-func :src-alpha :one-minus-src-alpha)
  (use-program program)
  (bind-buffer :array-buffer vbo-triangle)
  (enable-vertex-attrib-array attribute-pos)
  (vertex-attrib-pointer attribute-pos 3 :float nil 0 (null-pointer))
  (draw-arrays :triangles 0 3)
  (glut:swap-buffers))

(display-window (MyWindow))
  
