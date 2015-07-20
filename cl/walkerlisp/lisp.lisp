;; asdf

(require 'iterate)
(require 'alexandria)
(require 'named-readtables)
(require 'cl-ppcre)
(require 'trivial-garbage)
(require 'closer-mop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package

(defpackage :walkerlisp
  (:use #:cl
	#:iterate
	#:alexandria
	#:named-readtables
	#:cl-ppcre
	#:trivial-garbage
	#:closer-mop)
  (:shadowing-import-from #:closer-mop
			  #:defgeneric
			  #:defmethod
			  #:standard-generic-function)
  (:shadow #:push
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
  (:nicknames :wl))

(provide 'walkerlisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(in-package :wl)

(defreadtable :walkerlisp (:merge :common-lisp) (:case :invert))
(setf *print-case* :downcase)
(in-readtable :walkerlisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(in-package :wl)

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (let* ((predicate-name (intern (concatenate 'string (string name) "?")))
	 (specific-predicate-name (intern (concatenate 'string (string predicate-name) "?")))
	 (ctor-name (intern (concatenate 'string "make-" (string name))))
	 (namef (intern (concatenate 'string (string name) "f"))))
    `(prog1 (closer-mop:defclass ,name ,direct-superclasses ,direct-slots ,@options)
       (closer-mop:defgeneric ,ctor-name (nargs &rest initargs))
       (closer-mop:defmethod ,ctor-name ((nargs null) &rest initargs)
	 (apply #'make-instance ',name initargs))
       (closer-mop:defmethod ,ctor-name ((nargs (eql 0)) &rest initargs)
	 (declare (ignore initargs))
	 (make-instance ',name))
       (cl:defun ,namef (&rest initargs)
	 (let ((ctor-function #',ctor-name))
	   (if (keywordp (car initargs))
	       `(funcall ,ctor-function nil ,@initargs)
	       `(funcall ,ctor-function ,(length initargs) ,@initargs))))
       (cl:defmacro ,name (&rest initargs)
	 (apply #',namef initargs))
       (closer-mop:defgeneric ,predicate-name (obj))
       (closer-mop:defmethod ,predicate-name (obj)
	 (subtypep (type-of obj) ',name))
       (closer-mop:defgeneric ,specific-predicate-name (obj))
       (closer-mop:defmethod ,specific-predicate-name (obj)
	 (type= (type-of obj) ',name)))))

(defmacro defctor (class-name nargs &body body)
  (let ((ctor-name (intern (concatenate 'string "make-" (string class-name)))))
    (if (eql t nargs)
	`(cl:defmethod ,ctor-name ((nargs number) &rest initargs)
	   ,@body)
	;; else
	`(cl:defmethod ,ctor-name ((nargs (eql ,nargs)) &rest initargs)
	   ,@body))))

(defmacro defdtor (class-name &body slots-to-destruct)
  (let ((ctor-name (intern (concatenate 'string "make-"   (string class-name)))))
    (iter
      (for slot-def in (class-slots (find-class class-name)))
      (let* ((slot-name (slot-definition-name slot-def))
	     (slot-value (slot-value obj-name slot-name)))
	`(progn 
	   (defmethod initialize-instance :after ()
	     (tg:finalize #'(lambda () (del slot-val
					 
					 )))))))))

(defmacro defwith (macro-name class-name &rest arguments)
  `(defmacro ,macro-name (obj-name &rest arguments)))

(defmacro defprint ((obj-name class-name) control-string &rest fmt-args)
  `(defmethod print-object ((,obj-name ,class-name) stream)
     (format stream ,control-string ,@fmt-args)))

(defmacro multivb (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form
     ,@body))

(defmacro cond= (test-value &rest clauses)
  (let ((test-value-name (gensym)))
      (iter
	(for clause in clauses)
	(setf (car clause) `(equal ,(car clause) ,test-value-name)))
      `(let ((,test-value-name ,test-value))
	 (cond ,@clauses))))

;; this literally breaks sbcl
;;(defmacro if ((then &rest then-forms) &optional (else &rest else-forms)))

(export '(defclass defctor defdtor defwith defprint multivb cond= initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default printer

(defprint (obj standard-object)
    "#<~a~%~{~{~a~^: ~}~^~%~}>"
  (class-name (class-of obj))
  (iter
    (for slot-def in (class-slots (class-of obj)))
    (collect (list (slot-definition-name slot-def)
		   (slot-value obj (slot-definition-name slot-def))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructors

(in-package :wl)

(defgeneric del (obj)
  (:documentation
   "The generic destructor method for all objects"))

(defmethod del (obj)
  (iter
    (for slot-def in (class-slots (class-of obj)))
    (funcall #'del (slot-value obj (slot-definition-name slot-def)))))

(defmethod del ((lst list))
  (iter
    (for el in lst)
    (del el))
  nil)

(defmethod del ((ht hash-table))
  (iter
    (for (k v) in-hashtable ht)
    (del k)
    (del v))
  nil)

(defmethod del ((vec vector))
  (iter
    (for el in-vector vec)
    (del el))
  nil)

(defmethod del ((s stream))
  (not (close s)))

(export '(del))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files

(in-package :wl)

(defclass Stream (Resource-Wrapper)
  ((contents
    :type '(or stream null)
    :reader contents
    :initarg :contents
    :initform nil)))

(defclass File (Stream)
  ())

(defgeneric open? (strm)
  (:documentation
   "Return t if Stream strm is open, nil otherwise."))

(defmethod open? ((strm Stream))
  (open-stream-p (contents strm)))

;;; IDEA: defsyntax form
;;
;; (defsyntax let (bindings &body body)
;;   ;; the lambda-list is a regular lambda list
;;   (where (bindings = (at-least 1 (var-name (optional var-type) '<- var-value))))
;;   ...)
;;   ;; All the magic happens _after_ the lambda-list.
;;
;; ;; Yes, I know let is a special form.  It's just here for argument's sake.
;;

(export '(Stream File open?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; containers

(in-package :wl)

(defgeneric push (container obj)
  (:documentation
   "Return a new Container with `obj' `push!'d onto it.
   This is an insane way of doing it.  push should only be available
   for immutable structures."))

(defgeneric last (lst))
(defgeneric last! (lst val))
(defsetf last set-last!)

(defgeneric contains? (obj elt)
  (:documentation
   "Return `t' if `obj' contains element `elt', `nil' otherwise."))
(defgeneric list-keys (hashtable))
(defgeneric list-values (hashtable))

(defgeneric single? (container)
  (:documentation
   "Return `t' if `container' only one object, `nil' otherwise."))

(defgeneric copy (obj)
  (:documentation
   "Return a freshly allocated copy of object `obj'."))

(defmethod contains? ((sym symbol) (elt character))
  (let ((elt (if (upper-case-p elt)
		 (char-downcase elt)
		 (char-upcase elt))))
    (contains? (string sym) elt)))

(defmethod contains? ((seq sequence) elt)
  (cl:some #'(lambda (seq-elt) (equal elt seq-elt))
	   seq))

(defmethod contains? ((ht hash-table) elt)
  (multivb (_ contains?) (elt ht elt)
    (declare (ignore _))
    contains?))

(defmethod push! ((container vector) obj)
  (cl:vector-push-extend obj container)
  container)

(defmethod elt (obj idx)
  (cl:elt obj idx))

(defmethod elt! ((obj t) idx val)
  (setf (cl:elt obj idx) val))

(defmethod elt ((hash hash-table) key)
  (gethash key hash))

(defmethod elt! ((hash hash-table) key val)
  (setf (gethash key hash) val))

(defmethod list-keys ((hashtable hash-table))
  (hash-table-keys hashtable))

(defmethod list-values ((hashtable hash-table))
  (hash-table-values hashtable))

(defmethod last (obj)
  (cl:last obj))

(defmethod last ((lst list))
  (alexandria:lastcar lst))

(defmethod last! ((lst list) val)
  (if (and (cdr lst) (consp (cdr lst)))
      (set-last! (cdr lst) val)
      ;; else
      (setf (cdr lst) val)))

(defmethod single? ((lst list))
  (and (consp lst)
       (not (cdr lst))))

(export '(push! last last! elt elt! list-keys list-values contains? copy single?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ABCs

(defclass Iterator ()
  ()
  (:documentation
   "An object which is used to iterate over an Iterable."))

(defclass Comparable ()
  ()
  (:documentation
   "A class of objects which can be compared using functions `<' `<='
   `=' `>=' and `>'."))

(defclass Ordered (Comparable)
  ()
  (:documentation
   "A class of objects which have a total ordering defined on them by
   virtue of the comparison operators."))

(defclass Container ()
  ()
  (:documentation
   "An object which holds other objects within it."))

(defgeneric length (container)
  (:documentation
   "Return the number of objects within `Container' `container'."))

(defmethod length ((obj t))
  ;; Fallback for CL objects.
  (cl:length obj))

(defgeneric push! (container obj)
  (:documentation
   "Push object `obj' onto `Container' `container', modifying container in
   place.  Where exactly in the container obj ends up is dependant
   upon container's type.  Return container."))

(defgeneric push (container obj))

(defgeneric remove! (container obj-or-function)
  (:documentation
   "If `obj-or-function' is a function, remove all elements of
   container for which function called upon them returns non-`nil'.
   If obj-or-function is not a function, remove all elements of
   container which are `eql' to it."))

(defgeneric remove (container obj-or-function))

(defgeneric count (container obj-or-function))

(defgeneric map! (container fnc &rest containers))

(defgeneric map (container fnc &rest containers))

(defgeneric contains? (container obj))

(defgeneric any? (container predicate))

(defgeneric every? (container predicate))

(defgeneric select (container predicate))

(defclass Iterable ()
  ()
  (:documentation
   "An object which can be iterated over."))

(defgeneric get-iterator (iterable)
  (:documentation
   "Retreive an `Iterator' object over `Iterable' `iterable'."))

(defgeneric pop! (iterable)
  (:documentation
   "Pops next object out of `Iterable' `iterable' and return it as its
   primary value.  Returns iterable as its secondary value. Repeatedly
   popping objects returns them in iteration order."))

(defgeneric pop (iterable))

(defclass Indexable ()
  ()
  (:documentation
   "An object which can be indexed by `elt'.  Indices need not be `integer's."))

(defgeneric contains-key? (indexable idx)
  (:documentation
   "Return t if `Indexable' `indexable' has a value associated with
   the key `idx'."))

(defgeneric elt (indexable idx)
  (:documentation
   "Return the element of `indexable' indexed by `idx'."))

(defgeneric elt! (indexable idx val)
  (:documentation
   "Set the place indexed by `idx' in `indexable' to the value `val'.
   Used by `setf'; see (setf elt).  A user may define a method on this generic to
   allow (setf elt)-ing on user-defined types."))

(defsetf elt elt!)

(defclass Sequence (Container Iterable Indexable)
  ()
  (:documentation
   "An `Indexable' and `Iterable' `Container' which has a total ordering defined on
   its contents by virtue of its indices being `integer's."))

(defgeneric sort! (sequence)
  (:documentation
   "Sort in place the contents of the `Sequence' of `Ordered' elements `sequence'.
   Return sequence."))

(defgeneric sort (sequence))

(defgeneric first (sequence))

(defgeneric last (sequence))

(defclass Resource ()
  ((res))
  (:documentation
   "An Object which must be finalized when garbage collected.
   One never deals with them directly, but instead only accesses them
   through a Resource-Wrapper.  This is because
   `trivial-garbage:finalize' stipulates that an object may or may not
   be available at the point of finalization, and they are _not_
   available on CMUCL and SBCL in particular."))

(defclass Wrapper ()
  ((contents
    :initarg :contents))
  (:documentation
   "An object that wraps other objects."))

(defclass CL-Wrapper (Wrapper)
  ()
  (:documentation
   "An object that wraps a Common Lisp object."))

(defmethod elt ((clw CL-Wrapper) idx)
  (cl:elt (contents clw) idx))

(defmethod elt! ((clw CL-Wrapper) idx val)
  (setf (cl:elt (contents clw) idx) val))

(defclass Resource-Wrapper (Wrapper)
  ((contents
    :type 'Resource))
  (:documentation
   "A `Wrapper' which wraps a `Resource'.  When a `Resource-Wrapper'
   is garbage-collected, it will call `del' upon its `contents'.  It
   is therefore important to only access the Resource through the
   wrapper; saving a reference only to the Resource _itself_ may
   result in it being destroyed unexpectedly, if no reference to the
   wrapper exist anymore."))

(defmethod initialize-instance :after ((rw Resource-Wrapper) &key)
  (let ((resource (slot-value rw 'contents)))
    (finalize rw #'(lambda () (del resource)))))

(defclass Monad ()
  ())

(defclass Actor ()
  ())

(defclass Promise ()
  ())

(defclass Future ()
  ())

(defclass Thread ()
  ())

(defclass Mutex ()
  ())

(defclass Lock ()
  ())

(export '(elt elt! contains-key? push! push pop pop! get-iterator
	  remove remove!  count map contains? any? every? sort sort!
	  first last last!  length Container Container? Container??))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dict

(defclass Dict (CL-Wrapper Indexable Container)
  ((contents
    :type 'hash-table
    :initform (make-hash-table))))

(defmethod elt ((dict Dict) key)
  (elt (contents dict) key))

(defmethod elt! ((dict Dict) key val)
  (setf (elt (contents dict) key) val))

(defctor Dict t
  (let ((dict (Dict)))
    (iter
      (for (key . val) in initargs)
      (setf (elt dict key) val))
    dict))

(defmethod copy ((dict Dict))
  (with-slots ((contents contents)) dict
    (let* ((new-hash-table (copy-hash-table contents))
	   (new-dict (Dict)))
      (setf (slot-value new-dict 'contents) new-hash-table)
      new-dict)))
    

(defmethod push! ((dict Dict) (key-value-cons cons))
  (setf (elt dict (car key-value-cons)) (cdr key-value-cons)))

(defmethod contains? ((dict Dict) value)
  (contains? (contents dict) value))

(defmethod list-keys ((dict Dict))
  (list-keys (contents dict)))

(defmethod list-values ((dict Dict))
  (list-values (contents dict)))

(defmethod contains-key? ((dict Dict) key)
  (contains? (list-keys dict) key))

(export '(Dict Dict? Dict?? make-Dict contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector

(defclass Vector (CL-Wrapper Sequence Container)
  ((contents
    :type 'vector
    :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod push! ((vec Vector) val)
  (push! (contents vec) val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list

;; Doesn't work because lst doesn't care if you setf it.  Try:
;; (let ((l (list 'a 'b 'c)))
;;   (push! l 'd)
;;   l)
;; to see what I mean.
;; (defmethod push! ((lst list) obj)
;;   (setf lst (cons obj lst)))

(defmethod push ((lst list) obj)
  (cons obj lst))

(defmethod pop ((lst list))
  (cdr lst))
