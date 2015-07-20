(cl:in-package :cl-user)

(require 'walkerlisp)
(require 'cl-opengl)
(require 'cl-glut)
(require 'cl-glfw3)
(require 'iterate)
(require 'cffi)

(defpackage :opengl-fun
  (:use :cl
	:walkerlisp
	:cl-glut
	:cl-opengl
	:iterate)
  (:shadowing-import-from :walkerlisp
			  #:push
			  #:pop
			  #:elt
			  #:first
			  #:last
			  #:defclass
			  #:length
			  #:remove
			  #:count
			  #:map
			  #:sort)
  (:shadowing-import-from :cl-glut
			  #:special
			  #:close
			  #:get)
  (:shadowing-import-from :iterate
			  #:terminate)
  (:shadowing-import-from :cl-opengl
			  #:finish)
  (:import-from :cffi
		#:null-pointer)
  (:nicknames :glf))

