(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.category
  (:use :cl
	:cl-ppcre
	:span-based-ccg-derivation.util)
  (:export :category
	   :make-category
	   :category-conj?

	   :basic-category
	   :make-basic-category
	   :basic-category-symbol
	   :basic-category-feature

	   :complex-category
	   :make-complex-category
	   :complex-category-result
	   :complex-category-argument
	   :category-complex-forward?
	   :category-complex-backward?
	   :category-doller-convention?

	   :+forward+
	   :+backward+
	   :make-argument
	   :argument-category
	   :argument-directionality

	   :category-arity
	   :category-number-of-slots
	   :adjunct?
	   :has-feature?
	   :remove-feature
	   :make-conj-category
	   :remove-conj-category
	   :category=
	   :unify-category
	   :unify-argument
	   :unify-arguments
	   :attach-argument
	   :remove-argument

	   :string->category
	   :string->argument
	   :category->string
	   :argument->string
	   ))

(in-package :span-based-ccg-derivation.category)

;; category
(defstruct category
  conj?
  )

(defstruct (basic-category (:include category))
  symbol
  feature
  )

(defstruct (complex-category (:include category))
  result
  argument
  )

;; argument
(defconstant +forward+ #\/)
(defconstant +backward+ #\\)
(defconstant +doller-convention+ #\|)

(defstruct argument
  directionality
  category
  )

;; properties
(defun category-complex-forward? (cat)
  (and (typep cat 'complex-category)
       (char= (argument-directionality (complex-category-argument cat)) +forward+)))

(defun category-complex-backward? (cat)
  (and (typep cat 'complex-category)
       (char= (argument-directionality (complex-category-argument cat)) +backward+)))

(defun category-doller-convention? (cat)
  (and (typep cat 'complex-category)
       (char= (argument-directionality (complex-category-argument cat)) +doller-convention+)))

(defun category-arity (cat)
  (typecase cat
    (basic-category
     0)
    (complex-category
     (1+ (category-arity (complex-category-result cat))))))

(defun adjunct? (cat)
  (and (typep cat 'complex-category)
       (let ((x (complex-category-result cat))
	     (y (argument-category (complex-category-argument cat))))
	 (and (not (has-feature? x))
	      (not (has-feature? y))
	      (category= x y)))))

(defun category-number-of-slots (cat)
  (cond
    ((typep cat 'basic-category)
     0)
    ((adjunct? cat)
     1)
    (t
     (1+ (category-number-of-slots (complex-category-result cat))))))

;; feature
(defgeneric has-feature? (x))

(defmethod has-feature? ((cat basic-category))
  (if (basic-category-feature cat) t))

(defmethod has-feature? ((cat complex-category))
  (or (has-feature? (complex-category-result cat))
      (has-feature? (argument-category (complex-category-argument cat)))))

(defgeneric remove-feature (x))

(defmethod remove-feature ((cat basic-category))
  (let ((new (copy-basic-category cat)))
    (setf (basic-category-feature new) nil)
    new))

(defmethod remove-feature ((cat complex-category))
  (attach-argument (remove-feature (complex-category-result cat))
		   (remove-feature (complex-category-argument cat))))

(defmethod remove-feature ((arg argument))
  (make-argument :directionality (argument-directionality arg)
		 :category (remove-feature (argument-category arg))))

(defun make-conj-category (cat)
  (let ((new (copy-category cat)))
    (setf (category-conj? new) t)
    new))

(defun remove-conj-category (cat)
  (let ((new (copy-category cat)))
    (setf (category-conj? new) nil)
    new))

;; category=
(defgeneric category= (x y))

(defmethod category= (c1 c2)
  nil)

(defmethod category= ((c1 basic-category) (c2 basic-category))
  (and (eq (category-conj? c1) (category-conj? c2))
       (equal (basic-category-feature c1) (basic-category-feature c2))
       (string= (basic-category-symbol c1) (basic-category-symbol c2))))

(defmethod category= ((c1 complex-category) (c2 complex-category))
  (and (eq (category-conj? c1) (category-conj? c2))
       (category= (complex-category-result c1) (complex-category-result c2))
       (argument= (complex-category-argument c1) (complex-category-argument c2))))

(defun argument= (a1 a2)
  (and (char= (argument-directionality a1) (argument-directionality a2))
       (category= (argument-category a1) (argument-category a2))))

;; unification
(defgeneric unify-category (x y))

(defmethod unify-category (c1 c2)
  nil)

(defmethod unify-category ((c1 basic-category) (c2 basic-category))
  (when (and (eq (category-conj? c1) (category-conj? c2))
	     (string= (basic-category-symbol c1) (basic-category-symbol c2)))
    (cond
     ((equal (basic-category-feature c1) (basic-category-feature c2))
      (values c1 t))
     ((null (basic-category-feature c1))
      (values c2 t))
     ((null (basic-category-feature c2))
      (values c1 t))
     (t
      nil))))

(defmethod unify-category ((c1 complex-category) (c2 complex-category))
  (let ((r (unify-category (complex-category-result c1) (complex-category-result c2)))
	(a (unify-argument (complex-category-argument c1) (complex-category-argument c2)))
	(c nil))
    (when (and r a
	       (eq (category-conj? c1) (category-conj? c2)))
      (setf c (attach-argument r a))
      (setf (category-conj? c) (category-conj? c1))
      (values c t))))

(defun unify-argument (a1 a2)
  (awhen (and (char= (argument-directionality a1) (argument-directionality a2))
	      (unify-category (argument-category a1) (argument-category a2)))
	 (values (make-argument :directionality (argument-directionality a1)
				:category it)
		 t)))

(defun unify-arguments (a1 a2)
  (when (= (length a1) (length a2))
    (let ((args (mapcar #'unify-argument a1 a2)))
      (when (every #'(lambda (x) (not (null x))) args)
	(values args t)))))

(defun attach-argument (cat arg)
  (make-complex-category :result cat :argument arg))

(defun remove-argument (cat n)
  (declare (fixnum n))
  (labels
   ((aux (cat n args)
      (cond
       ((< n 0)
	(error "~a is a negative integer." n))
       ((= n 0)
	(values cat args))
       (t
	(aux (complex-category-result cat) (- n 1) (cons (complex-category-argument cat) args))))))
   (aux cat n nil)))

;; category <-> string
(defun slash-position (string)
  (cond
    ((not (char= (char string 0) #\())
     (position-if #'(lambda (x) (member x (list +forward+ +backward+ +doller-convention+) :test #'char=)) string))
    (t
     (let ((i 1)
	   (d 1))
       (declare (fixnum i d))
       (loop
	  :for x :across (subseq string 1)
	  :do
	    (incf i)
	    (cond
	      ((char= x #\()
	       (incf d))
	      ((char= x #\))
	       (decf d)
	       (when (= d 0)
		 (return-from slash-position i)))
	      (t
	       nil)))
       (error "unbalanced parentheses: ~a" string)))))

(defun remove-outer-parenthesis (string)
  (cond
    ((char= (char string 0) #\()
     (subseq string 1 (- (length string) 1)))
    (t
     string)))

(defun decompose-string (string)
  (let ((i (slash-position string)))
    (cond
     ((null i)
      (values nil nil))
     (t
      (values (remove-outer-parenthesis (subseq string 0 i))
	      (subseq string i))))))

(defun string->category (string)
  (let ((conj? nil))

    ;; conj feature
    (when (scan "\\[conj\\]" string)
      (setf string (regex-replace-all "\\[conj\\]" string ""))
      (setf conj? t))

    (multiple-value-bind (result argument) (decompose-string string)
      (cond
       ;; basic
       ((null result)
	(make-basic-category :symbol (regex-replace-all "\\[[^\\[\\]]+\\]" string "")
			     :feature (scan-to-strings "\\[[^\\[\\]]+\\]" string)
			     :conj? conj?))
       ;; complex
       (t
	(make-complex-category :conj? conj?
			       :result (string->category result)
			       :argument (string->argument argument)))))))

(defun string->argument (string)
  (make-argument :directionality (char string 0)
		 :category (string->category (remove-outer-parenthesis (subseq string 1)))))

(defun cat->str (cat)
  (typecase cat
    (basic-category
     (let ((feature ""))
       (awhen (basic-category-feature cat)
         (setf feature it))
       (concatenate 'string (basic-category-symbol cat) feature)))
    (complex-category
     (format nil "(~a~a)"
	     (cat->str (complex-category-result cat))
	     (argument->string (complex-category-argument cat))))))

(defun category->string (cat)
  (let ((result (remove-outer-parenthesis (cat->str cat))))
    (cond
      ((category-conj? cat)
       (concatenate 'string result "[conj]"))
      (t
       result))))

(defun argument->string (arg)
  (format nil "~a~a"
	  (argument-directionality arg)
	  (cat->str (argument-category arg))))
