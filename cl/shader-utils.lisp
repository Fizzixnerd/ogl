(in-package :glf)

(defun eofp (stream)
  (handler-case (let ((next-char (read-char stream nil nil)))
		  (when next-char
		    (unread-char next-char stream))
		  (not next-char))
    (end-of-file () t)))

(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))

(defmacro strcatf (place &rest strings)
  (let ((p place))
    `(setf ,p (strcat ,p ,@strings))))

(defun read-stream-into-string (stream)
  (let ((str ""))
    (iter
      (until (eofp stream))
      (strcatf str (read-line stream) (format nil "~c" #\Newline)))
    str))

(defgeneric make-shader (pathspec shader-type)
  (:documentation "Create and return a shader object of the specified
  type from the contents of the file referenced by pathspec."))

(defmethod make-shader (pathspec (shader-type symbol))
  (let ((source (read-stream-into-string (open pathspec)))
	(shader (create-shader shader-type)))
    (shader-source shader (list source))
    (compile-shader shader)
    (print (get-shader-info-log shader))
    shader))
    
    
