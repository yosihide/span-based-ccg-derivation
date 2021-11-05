(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.pattern
  (:use :cl
	:cl-ppcre
	:span-based-ccg-derivation.util
	:span-based-ccg-derivation.category)
  (:export :mapping-get
	   :unify-mapping
	   :pattern-match?))

(in-package :span-based-ccg-derivation.pattern)

;; mapping
(defun make-mapping ()
  (make-hash-table :test 'equal))

(defun mapping-get (var m)
  (gethash var m))

(defun mapping-set! (m var cat)
  (setf (gethash var m) cat)
  m)

(defgeneric unify-obj (x y))

(defmethod unify-obj (x y)
  (declare (ignore x y))
  nil)

(defmethod unify-obj ((x category) (y category))
  (unify-category x y))

(defmethod unify-obj ((x list) (y list))
  (unify-arguments x y))

(defun unify-mapping (m1 m2)

  (when (or (null m1) (null m2))
    (return-from unify-mapping nil))

  (let ((m (make-mapping)))
    (loop
       :for var1 :being :the :hash-keys :of m1
       :using (:hash-value obj1)
       :do
       (multiple-value-bind (obj2 present?) (mapping-get var1 m2)
	 (cond
	   (present?
	    (multiple-value-bind (obj match?) (unify-obj obj1 obj2)
	      (cond
		(match?
		 (mapping-set! m var1 obj))
		(t
		 (return-from unify-mapping nil)))))
	   (t
	    (mapping-set! m var1 obj1)))))

    (loop
       :for var2 :being :the :hash-keys :of m2
       :using (:hash-value obj2)
       :do
       (when (not (nth-value 1 (mapping-get var2 m1)))
	 (mapping-set! m var2 obj2)))
    m))

;; pattern matching
;; ?X :variable
(defun var? (string)
  (string= "?" (subseq string 0 1)))

(defun pattern-match? (pattern cat)
  (labels
   ((match (pcat cat)
      (cond
       ((and (typep pcat 'basic-category)
	     (var? (basic-category-symbol pcat)))
	(let ((m (make-mapping)))
	  (mapping-set! m (basic-category-symbol pcat) cat)
	  (list m)))

       ((and (typep pcat 'basic-category) (typep cat 'basic-category))
	(when (and (string= (basic-category-symbol pcat) (basic-category-symbol cat))
		   (equal (basic-category-feature pcat) (basic-category-feature cat)))
	  (list (make-mapping))))

       ((category-doller-convention? pcat)
	(let ((mappings nil)
	      (variable (basic-category-symbol (argument-category (complex-category-argument pcat)))))
	  (loop
	     :for args := nil :then (cons (complex-category-argument cat-result) args)
	     :for cat-result := cat :then (complex-category-result cat-result)
	     :repeat (1+ (category-arity cat))
	     :do
	     (dolist (m (match (complex-category-result pcat) cat-result))
	       (awhen (unify-mapping m (mapping-set! (make-mapping) variable args))
	         (push it mappings))))
	  mappings))

       ((or (and (category-complex-forward? pcat) (category-complex-forward? cat))
	    (and (category-complex-backward? pcat) (category-complex-backward? cat)))
	(let ((mappings nil))
	  (dolist (m1 (match (complex-category-result pcat) (complex-category-result cat)))
	    (dolist (m2 (match (argument-category (complex-category-argument pcat)) (argument-category (complex-category-argument cat))))
	      (awhen (unify-mapping m1 m2)
		     (push it mappings))))
	  mappings))

       (t
	nil))))

   (match (string->category pattern) cat)))
