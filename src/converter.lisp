(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.converter
  (:use :cl
	:cl-ppcre
	:span-based-ccg-derivation.util
	:span-based-ccg-derivation.category
	:span-based-ccg-derivation.pattern
	:span-based-ccg-derivation.tree
	:span-based-ccg-derivation.derivation)
  (:export :derivation->sb-derivation
	   :sb-derivation->derivation
	   :derivation->ptb
	   :ptb->derivation
	   ))

(in-package :span-based-ccg-derivation.converter)

(defun ->angle (string)
  (regex-replace-all "\\(" (regex-replace-all "\\)" string ">") "<"))

(defun angle-> (string)
  (regex-replace-all "<" (regex-replace-all ">" string ")") "("))

;; root chain
(defun make-root-chain-label (chain)
  (format nil "{RT~{_~a~}}" (mapcar #'(lambda (c) (->angle (category->string c))) chain)))

(defun extract-root-chain (label)
  (awhen (scan-to-strings "{RT_[^{}]*}" label)
    (mapcar #'(lambda (x) (string->category (angle-> (subseq x 1))))
	    (all-matches-as-strings "_[^_]+" (subseq it 3 (- (length it) 1))))))

;; unary rule
(defun unary-chains (l-chain r-chain)
  (let ((result ""))
    (setf result
	  (format nil "~a~a"
		  (if l-chain
		      (format nil "{UL~{_~a~}}"
			      (mapcar #'(lambda (x) (->angle (category->string x))) l-chain))
		    "")
		  (if r-chain
		      (format nil "{UR~{_~a~}}"
			      (mapcar #'(lambda (x) (->angle (category->string x))) r-chain))
		    "")))
    (when (not (string= result ""))
      result)))

(defun extract-unary-chains (label)
  (let ((l-chain nil)
	(r-chain nil))
    (awhen (scan-to-strings "{UL_[^{}]*}" label)
      (setf l-chain
	    (mapcar #'(lambda (x) (string->category (angle-> x)))
		    (split "_" (subseq it 4 (- (length it) 1))))))
    (awhen (scan-to-strings "{UR_[^{}]*}" label)
      (setf r-chain
	    (mapcar #'(lambda (x) (string->category (angle-> x)))
		    (split "_" (subseq it 4 (- (length it) 1))))))
    (list l-chain r-chain)))

;; generalized composition (Here, function application is treated as a special case of function composition.)
(defun make-composition (op n Y)
  (format nil "{~a_~a_~a}" op n (->angle (category->string Y))))

(defun decompose-composition (label)
  (let ((parts (split "_" (subseq label 1 (- (length label) 1)))))
    (list (read-from-string (nth 1 parts)) (string->category (angle-> (nth 2 parts))))))

;; type-raised-composition
(defun make-type-raised-composition (op n args)
  (format nil "{~a_~a~{_~a~}}" op n (mapcar #'(lambda (a) (->angle (argument->string a))) args)))

(defun decompose-type-raised-composition (label)
  (let ((parts (split "_" (subseq label 1 (- (length label) 1)))))
    (list (read-from-string (nth 1 parts))
	  (mapcar #'(lambda (x) (string->argument (angle-> x))) (nthcdr 2 parts)))))

;; substitution
(defun make-substitution (op Y)
  (format nil "{~a_~a}" op (->angle (category->string Y))))

(defun decompose-substitution (label)
  (string->category (angle-> (nth 1 (split "_" (subseq label 1 (- (length label) 1)))))))

;; conj rule
(defun make-conj-rule ()
  "{CONJ_}")

(defun forward-composition? (node)
  (let* ((left (node-label (node-ref node 0)))
	 (right (node-label (node-ref node 1)))
	 (parent (node-label node)))

    (when (or (category-conj? left)
	      (category-conj? right)
	      (category-conj? parent))
      (return-from forward-composition? nil))

    ;; X'/(X'|alpha) (X|alpha)|beta => X|beta
    (dolist (m1 (pattern-match? "?X/(?X|?alpha)" left))
      (dolist (m2 (pattern-match? "(?X|?alpha)|?beta" right))
	(dolist (m3 (pattern-match? "?X|?beta" parent))
	  (awhen (unify-mapping m1 (unify-mapping m2 m3))
	    (when (not (has-feature? (mapping-get "?X" m1)))
	      (return-from forward-composition?
	        (make-type-raised-composition "FT" (length (mapping-get "?beta" it)) (mapping-get "?alpha" it))))))))

    ;; X/Y Y|alpha => X|alpha
    (dolist (m1 (pattern-match? "?X/?Y" left))
      (dolist (m2 (pattern-match? "?Y|?alpha" right))
	(dolist (m3 (pattern-match? "?X|?alpha" parent))
	  (awhen (unify-mapping m1 (unify-mapping m2 m3))
	    (return-from forward-composition?
	      (make-composition "F" (length (mapping-get "?alpha" it)) (mapping-get "?Y" it)))))))))

(defun backward-composition? (node)
  (let ((left (node-label (node-ref node 0)))
	(right (node-label (node-ref node 1)))
	(parent (node-label node)))

    (when (or (category-conj? left)
	      (category-conj? right)
	      (category-conj? parent))
      (return-from backward-composition? nil))

    ;; (X|alpha)|beta X'\(X'|alpha) => X|beta
    (dolist (m1 (pattern-match? "(?X|?alpha)|?beta" left))
      (dolist (m2 (pattern-match? "?X\\(?X|?alpha)" right))
	(dolist (m3 (pattern-match? "?X|?beta" parent))
	  (awhen (unify-mapping m1 (unify-mapping m2 m3))
	    (when (not (has-feature? (mapping-get "?X" m2)))
	      (return-from backward-composition?
	        (make-type-raised-composition "BT" (length (mapping-get "?beta" it)) (mapping-get "?alpha" it))))))))

    ;; Y|alpha X\Y => X|alpha
    (dolist (m1 (pattern-match? "?Y|?alpha" left))
      (dolist (m2 (pattern-match? "?X\\?Y" right))
	(dolist (m3 (pattern-match? "?X|?alpha" parent))
	  (awhen (unify-mapping m1 (unify-mapping m2 m3))
	    (return-from backward-composition?
	      (make-composition "B" (length (mapping-get "?alpha" it)) (mapping-get "?Y" it)))))))))

;; Y|Z (X\Y)|Z => X|Z
(defun backward-substitution? (node)
  (let ((left (node-label (node-ref node 0)))
	(right (node-label (node-ref node 1)))
	(parent (node-label node)))

    (when (or (category-conj? left)
	      (category-conj? right)
	      (category-conj? parent))
      (return-from backward-substitution? nil))

    (dolist (m1 (pattern-match? "?Y|?alpha" left))
      (dolist (m2 (pattern-match? "(?X\\?Y)|?alpha" right))
	(dolist (m3 (pattern-match? "?X|?alpha" parent))
	  (awhen (unify-mapping m1 (unify-mapping m2 m3))
	    (when (= (length (mapping-get "?alpha" it)) 1)
	      (return-from backward-substitution?
	        (make-substitution "BS" (mapping-get "?Y" it))))))))))

(defparameter *punct* '("." "," ":" ";" "LRB" "RRB"))
(defparameter *conj-and-punct* (cons "conj" *punct*))

(defun conj-punct-rule? (node)
  (let ((left (node-label (node-ref node 0)))
	(right (node-label (node-ref node 1)))
	(parent (node-label node)))

    ;; X X[conj] => X
    (when (and (not (category-conj? left))
	       (category-conj? right)
	       (not (category-conj? parent))
	       (category= left parent))
      (return-from conj-punct-rule?
	(make-conj-rule)))

    ;; conj-or-punct X => Y
    (when (and (typep left 'basic-category)
	       (find (basic-category-symbol left) *conj-and-punct* :test #'string=))
      (cond
	;; If X = Y, then conj-or-punct is treated as a modifier
	((category= right parent)
	 (return-from conj-punct-rule?
	   (make-type-raised-composition "FT" 0 nil)))
	(t
	 (return-from conj-punct-rule?
	   (make-composition "F" 0  right)))))

    ;; X conj-or-punct => Y
    (when (and (typep right 'basic-category)
	       (find (basic-category-symbol right) *conj-and-punct* :test #'string=))
      (cond
	;; If X = Y, then conj-or-punct is treated as a modifier
	((category= left parent)
	 (return-from conj-punct-rule?
	   (make-type-raised-composition "BT" 0 nil)))
	(t
	 (return-from conj-punct-rule?
	   (make-composition "B" 0 left)))))))

;; NP N\N => NP is modified (N\N is treated as NP\NP)
(defun error-correction (derivation)
  (let ((left (node-label (node-ref derivation 0)))
	(right (node-label (node-ref derivation 1)))
	(parent (node-label derivation)))
    (when (and (pattern-match? "NP" parent)
	       (pattern-match? "NP" left)
	       (pattern-match? "N\\N" right))
      (make-type-raised-composition "BT" 0 nil))))

(defun recover-category (parent label)

  (acond
   ((scan-to-strings "{(B|BT|F|FT|BS|CONJ)_[^{}]*}" label)
    (setf label it))
   (t
    (error "~a includes no rule schema label." label)))

  ;; If the parent's label is X[conj], the rule schema label is ignored.
  ;; For punctuation marks, "conj" category is temporarily assigned. The function "post-process!" replaces "conj" with a punctuation category.
  (when (category-conj? parent)
    (cond
      ((scan "^{F_" label)
       (return-from recover-category
	 (list (string->category "conj")
	       (nth 1 (decompose-composition label)))))
      ((scan "^{FT_" label)
       (return-from recover-category
	 (list (string->category "conj")
	       parent)))
      (t
       (return-from recover-category
	 (list (string->category "conj")
	       (remove-conj-category parent))))))

  (cond
    ;; X/Y Y|alpha => X|alpha
    ((scan "^{F_" label)
     (let ((len (category-arity parent)))
       (destructuring-bind (n Y) (decompose-composition label)
	 (setf len (min n len))
	 (multiple-value-bind (X alpha) (remove-argument parent len)
	   (list (attach-argument X (make-argument :directionality +forward+ :category Y))
		 (reduce #'attach-argument alpha :initial-value Y))))))

   ;; Y|alpha X\Y => X|alpha
   ((scan "^{B_" label)
    (let ((len (category-arity parent)))
      (destructuring-bind (n Y) (decompose-composition label)
        (setf len (min n len))
	(multiple-value-bind (X alpha) (remove-argument parent len)
	  (list (reduce #'attach-argument alpha :initial-value Y)
		(attach-argument X (make-argument :directionality +backward+ :category Y)))))))

   ;; X'/(X'|alpha) (X|alpha)|beta => X|beta
   ((scan "^{FT_" label)
    (let ((len (category-arity parent)))
      (destructuring-bind (n alpha) (decompose-type-raised-composition label)
	(setf len (min n len))
	(multiple-value-bind (X beta) (remove-argument parent len)
          (let* ((X-new (remove-feature X)))
	    (list (attach-argument X-new (make-argument :directionality +forward+ :category (reduce #'attach-argument alpha :initial-value X-new)))
		  (reduce #'attach-argument beta :initial-value (reduce #'attach-argument alpha :initial-value X))))))))

   ;; (X|alpha)|beta X'\(X'|alpha) => X|beta
   ((scan "^{BT_" label)
    (let ((len (category-arity parent)))
      (destructuring-bind (n alpha) (decompose-type-raised-composition label)
	(setf len (min n len))
	(multiple-value-bind (X beta) (remove-argument parent len)
          (let* ((X-new (remove-feature X)))
	    (list (reduce #'attach-argument beta :initial-value (reduce #'attach-argument alpha :initial-value X))
		  (attach-argument X-new (make-argument :directionality +backward+ :category (reduce #'attach-argument alpha :initial-value X-new)))))))))

   ;; Y|Z (X\Y)|Z => X|Z
   ((scan "^{BS_" label)
    (let ((len (min 1 (category-arity parent)))
	  (Y (decompose-substitution label)))
      (cond
       ((= len 1)
	(multiple-value-bind (X Zargs) (remove-argument parent 1)
	  (list (attach-argument Y (first Zargs))
		(attach-argument (attach-argument X (make-argument :directionality +backward+ :category Y)) (first Zargs)))))
       (t
	(list Y
	      (attach-argument parent (make-argument :directionality +backward+ :category Y)))))))

   ;; X X[conj] => X
   ((scan "^{CONJ_" label)
    (list parent
	  (make-conj-category parent)))

   (t
    (error "unknown operation"))))

;; pre-processing
(defun remove-nb-feature! (cat)
  (typecase cat
    (basic-category
     (when (equal (basic-category-feature cat) "[nb]")
       (setf (basic-category-feature cat) nil)))
    (complex-category
     (remove-nb-feature! (complex-category-result cat))
     (remove-nb-feature! (argument-category (complex-category-argument cat))))))

(defun pre-process-nb-feature! (derivation)
  (remove-nb-feature! (node-label derivation))
  (typecase derivation
    (d-terminal-node
     nil)
    (d-internal-node
     (dolist (c (children derivation))
       (pre-process-nb-feature! c))))
  derivation)

(defun add-dummy-root (derivation)
  (make-d-internal-node :children (list derivation)))

(defun pre-process! (derivation)
  (add-dummy-root (pre-process-nb-feature! derivation)))

;; post-process
(defun add-nb-feature? (cat)
  (typecase cat
    (basic-category
     cat)
    (complex-category
     (cond
       ((pattern-match? "NP/N" cat)
	(cond
	  ((category-conj? cat)
	   (string->category "NP[nb]/N[conj]"))
	  (t
	   (string->category "NP[nb]/N"))))
       (t
	(make-complex-category :result (add-nb-feature? (complex-category-result cat))
			       :argument (make-argument :directionality (argument-directionality (complex-category-argument cat))
							:category (add-nb-feature? (argument-category (complex-category-argument cat))))
			       :conj? (category-conj? cat)))))))

(defun post-process! (derivation &key parent left right)
  (labels
      ;; The node "derivation" is the left child when there is a "right" sibling.
      ((left-child? ()
	 (if right t))
       (right-child? ()
	 (if left t))
       ;; basic
       (NP? (cat)
	 (and (typep cat 'basic-category)
	      (string= "NP" (basic-category-symbol cat))))

       (S? (cat)
	 (and (typep cat 'basic-category)
	      (string= "S" (basic-category-symbol cat))))

       (Sdcl? (cat)
	 (and (typep cat 'basic-category)
	      (string= "S" (basic-category-symbol cat))
	      (equal "[dcl]" (basic-category-feature cat))))
       ;; complex
       (SbNP? (cat)
	 (aand (pattern-match? "?X\\?Y" cat)
	       (let ((x (mapping-get "?X" (first it)))
		     (y (mapping-get "?Y" (first it))))
		 (and (S? x) (NP? y)))))

       (SbNPbSbNP? (cat)
	 (aand (pattern-match? "?X\\?Y" cat)
	       (let ((x (mapping-get "?X" (first it)))
		     (y (mapping-get "?Y" (first it))))
		 (and (SbNP? x) (SbNP? y)))))

       (SbNPfSbNP? (cat)
	 (aand (pattern-match? "?X/?Y" cat)
	       (let ((x (mapping-get "?X" (first it)))
		     (y (mapping-get "?Y" (first it))))
		 (and (SbNP? x) (SbNP? y)))))

       (SbS? (cat)
	 (aand (pattern-match? "?X\\?Y" cat)
	       (let ((x (mapping-get "?X" (first it)))
		     (y (mapping-get "?Y" (first it))))
		 (and (S? x) (S? y)))))

       (SfS? (cat)
	 (aand (pattern-match? "?X/?Y" cat)
	       (let ((x (mapping-get "?X" (first it)))
		     (y (mapping-get "?Y" (first it))))
		 (and (S? x) (S? y)))))

       (SdclbSdcl? (cat)
	 (aand (pattern-match? "?X\\?Y" cat)
	       (let ((x (mapping-get "?X" (first it)))
		     (y (mapping-get "?Y" (first it))))
		 (and (Sdcl? x) (Sdcl? y)))))

       (SdclfSdcl? (cat)
	 (aand (pattern-match? "?X/?Y" cat)
	       (let ((x (mapping-get "?X" (first it)))
		     (y (mapping-get "?Y" (first it))))
		 (and (Sdcl? x) (Sdcl? y))))))

    (typecase derivation
      (d-terminal-node

       ;; recover [nb] feature
       (awhen (add-nb-feature? (node-label derivation))
         (setf (node-label derivation) it)
	 (setf (d-terminal-node-predarg-cat derivation) (category->string it)))

       (acond
	;; punctuation marks
	;; temporarily assigned "conj" is replaced with a punctuation category.
	((pattern-match? "conj" (node-label derivation))
	 (awhen (find (d-terminal-node-pos derivation) *punct* :test #'string=)
	   (setf (node-label derivation) (string->category it))
	   (setf (d-terminal-node-predarg-cat derivation) it)))

	;; C&C parser's gen_left_punct
	((and (left-child?)
	      (category= (remove-feature (node-label parent))
			 (remove-feature (node-label right)))
	      (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	((aand (left-child?)
	       (pattern-match? "?X\\?Y" (node-label parent))
	       (let ((x (remove-feature (mapping-get "?X" (first it))))
		     (y (remove-feature (mapping-get "?Y" (first it))))
		     (z (remove-feature (node-label right))))
		 (and (category= x y) (category= y z)))
	       (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	((and (left-child?)
	      (string= "," (d-terminal-node-pos derivation))
	      (SbNPbSbNP? (node-label parent))
	      (NP? (node-label right)))
	 (setf (node-label derivation) (string->category ","))
	 (setf (d-terminal-node-predarg-cat derivation) ","))

	;; C&C parser's gen_right_punct
	((and (right-child?)
	      (category= (remove-feature (node-label parent))
			 (remove-feature (node-label left)))
	      (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	((and (right-child?)
	      (SfS? (node-label parent))
	      (NP? (node-label left))
	      (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	((and (right-child?)
	      (SfS? (node-label parent))
	      (SdclbSdcl? (node-label left))
	      (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	((and (right-child?)
	      (SbS? (node-label parent))
	      (SdclfSdcl? (node-label left))
	      (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	((and (right-child?)
	      (SbNPbSbNP? (node-label parent))
	      (SdclfSdcl? (node-label left))
	      (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	((and (right-child?)
	      (SbNPfSbNP? (node-label parent))
	      (SdclfSdcl? (node-label left))
	      (find (d-terminal-node-pos derivation) *punct* :test #'string=))
	 (setf (node-label derivation) (string->category it))
	 (setf (d-terminal-node-predarg-cat derivation) it))

	;; C&C parser's gen_right_comma_typechange
	((and (right-child?)
	      (SfS? (node-label parent))
	      (NP? (node-label left))
	      (string= "," (d-terminal-node-pos derivation)))
	 (setf (node-label derivation) (string->category ","))
	 (setf (d-terminal-node-predarg-cat derivation) ","))

	((and (right-child?)
	      (or (SfS? (node-label parent))
		  (SbNPfSbNP? (node-label parent))
		  (SbNPbSbNP? (node-label parent))
		  (SbS? (node-label parent)))
	      (SdclfSdcl? (node-label left))
	      (string= "," (d-terminal-node-pos derivation)))
	 (setf (node-label derivation) (string->category ","))
	 (setf (d-terminal-node-predarg-cat derivation) ","))

	((and (right-child?)
	      (SfS? (node-label parent))
	      (SdclbSdcl? (node-label left))
	      (string= "," (d-terminal-node-pos derivation))
	 (setf (node-label derivation) (string->category ","))
	 (setf (d-terminal-node-predarg-cat derivation) ",")))

	;; conjunction
	((and (string= (d-terminal-node-pos derivation) "CC")
	      (not (string= (word derivation) "&")))
	 (cond
	   ;; C&C parser's gen_conj
	   ((and (left-child?)
		 (not (pattern-match? "conj" (node-label right))) ; conj_reject
		 (or (category= (remove-feature (node-label parent))
				(remove-feature (node-label right)))
		     (aand (pattern-match? "?X\\?Y" (node-label parent))
			   (let ((x (remove-feature (mapping-get "?X" (first it))))
				 (y (remove-feature (mapping-get "?Y" (first it))))
				 (z (remove-feature (node-label right))))
			     (and (category= x y) (category= y z))))))
	    (setf (node-label derivation) (string->category "conj"))
	    (setf (d-terminal-node-predarg-cat derivation) "conj")))))

       derivation)

      (d-internal-node
       (cond
	 ((unary? derivation)
	  (make-d-internal-node :label (or (add-nb-feature? (node-label derivation)) (node-label derivation))
				:head (head derivation)
				:children (list (post-process! (node-ref derivation 0)))))
	 (t
	  (make-d-internal-node :label (or (add-nb-feature? (node-label derivation)) (node-label derivation))
				:head (head derivation)
				:children (list (post-process! (node-ref derivation 0) :parent derivation :right (node-ref derivation 1))
						(post-process! (node-ref derivation 1) :parent derivation :left (node-ref derivation 0))))))))))

(defun rule-schema (node)
  (or (forward-composition? node)
      (backward-composition? node)
      (backward-substitution? node)
      (conj-punct-rule? node)
      (error-correction node)
      ))

(defun extract-unary-chain (derivation)
  (cond
   ((unary? derivation)
    (multiple-value-bind (d chain) (extract-unary-chain (node-ref derivation 0))
      (values d (append1 chain (node-label (node-ref derivation 0))))))
   (t
    (values derivation nil))))

(defun attach-chain (derivation chain)
  (cond
   ((null chain)
    derivation)
   (t
    (attach-chain (make-d-internal-node :label (first chain)
					:head 0
					:children (list derivation))
		  (rest chain)))))

;; conversion
(defun derivation->sb-derivation-aux (derivation)
  (typecase
   derivation
   (d-terminal-node
    (make-terminal-node :label (d-terminal-node-pos derivation) :word (word derivation)))
   (d-internal-node
    (multiple-value-bind (left l-chain) (extract-unary-chain (node-ref derivation 0))
      (multiple-value-bind (right r-chain) (extract-unary-chain (node-ref derivation 1))
        (let ((label (rule-schema derivation)))
	  (awhen (unary-chains l-chain r-chain)
	    (setf label (concatenate 'string label it)))
	  (make-internal-node :label label :children (list (derivation->sb-derivation-aux left) (derivation->sb-derivation-aux right)))))))))

(defun derivation->sb-derivation (derivation)
  (let ((result nil))
    (multiple-value-bind (d chain) (extract-unary-chain (pre-process! derivation))
      (setf result (derivation->sb-derivation-aux d))
      (setf (node-label result) (concatenate 'string (make-root-chain-label chain) (node-label result))))
    result))

;; inverse conversion
(defun sb-derivation->derivation-aux (node &key chain)
  (typecase
   node
   (terminal-node
    (attach-chain (make-d-terminal-node :label (first chain)
					:word (word node)
					:mod-pos (node-label node)
					:orig-pos (node-label node)
					:predarg-cat (category->string (first chain)))
		  (rest chain)))

   (internal-node
    (destructuring-bind (l-cat r-cat) (recover-category (first chain) (node-label node))
      (destructuring-bind (ul ur) (extract-unary-chains (node-label node))
        (attach-chain (make-d-internal-node :label (first chain)
					    :head 0
					    :children (list (sb-derivation->derivation-aux (node-ref node 0) :chain (append1 ul l-cat))
							    (sb-derivation->derivation-aux (node-ref node 1) :chain (append1 ur r-cat))))
		      (rest chain)))))))

(defun sb-derivation->derivation (node &key (default-root-chain '("S[dcl]")))
  (post-process! (sb-derivation->derivation-aux (if (unary? node) (node-ref node 0) node)
						:chain (or (extract-root-chain (node-label node)) (mapcar #'string->category default-root-chain)))))

;; baseline
(defun derivation->ptb (derivation)
  (typecase derivation
    (d-terminal-node
     (make-internal-node :label (->angle (category->string (node-label derivation)))
			 :children (list (make-terminal-node :label (d-terminal-node-pos derivation) :word (word derivation)))))
    (d-internal-node
     (make-internal-node :label (->angle (category->string (node-label derivation)))
			 :children (mapcar #'derivation->ptb (children derivation))))))

(defun ptb->derivation (node)
  (cond
    ((typep (node-ref node 0) 'terminal-node)
     (let ((c (node-ref node 0)))
       (make-d-terminal-node :label (string->category (angle-> (node-label node)))
			     :word (word c)
			     :mod-pos (node-label c)
			     :orig-pos (node-label c)
			     :predarg-cat (angle-> (node-label node)))))
    (t
     (make-d-internal-node :label (string->category (angle-> (node-label node)))
			   :head 0
			   :children (mapcar #'ptb->derivation (children node))))))
