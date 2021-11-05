(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.markup
  (:use :cl
	:cl-ppcre
	:span-based-ccg-derivation.category
	:span-based-ccg-derivation.tree
	:span-based-ccg-derivation.derivation
	:span-based-ccg-derivation.util)
  (:export :create-markedup-file))

(in-package :span-based-ccg-derivation.markup)

(defun read-markedup (file)
  (with-open-file (in file)
    (do ((line)
	 (tab (make-hash-table :test 'equal)))
	((null (setf line (read-line in nil nil)))
	 tab)
      (cond
	((or (string= line "")
	     (char= (char line 0) #\#)
	     (char= (char line 0) #\=)
	     (char= (char line 0) #\Space)
	     (char= (char line 0) #\Tab))
	 nil)
	(t
	 (setf (gethash (category->string (string->category line)) tab) t))))))

(defun oov-categories (auto markedup)
  (let ((m-tab (read-markedup markedup))
	(result (make-hash-table :test 'equal)))
    (with-open-file (in auto)
      (do-stream-derivation (d in)
	(dolist (stag (supertags d))
	  (let ((str (category->string stag)))
	    (when (not (gethash str m-tab))
	      (setf (gethash str result) t))))))
    (hash-keys result)))

;; Marked-up categories
(defstruct (markedup-basic-category (:include basic-category))
  id
  index
  )

(defstruct (markedup-complex-category (:include complex-category))
  id
  index
  )

(defun category->markedup-category (cat)
  (typecase cat
    (basic-category
     (make-markedup-basic-category
      :symbol (basic-category-symbol cat)
      :feature (basic-category-feature cat)))
    (complex-category
     (make-markedup-complex-category
      :result (category->markedup-category (complex-category-result cat))
      :argument (make-argument
		 :directionality (argument-directionality (complex-category-argument cat))
		 :category (category->markedup-category (argument-category (complex-category-argument cat))))))))

;; This function assigns slot "index" to the result (basic or adjunct) category of "cat"
(defun assign-index! (cat index)
  (labels
      ((assign! (cat)
	 (cond
	   ((typep cat 'basic-category)
	    (setf (markedup-basic-category-index cat) (format nil "<~a>" index)))
	   ((adjunct? cat)
	    (setf (markedup-complex-category-index cat) (format nil "<~a>" index)))
	   (t
	    (assign! (complex-category-result cat))))))
    (assign! cat)
    cat))

(defun assign-id! (cat &optional (ids '("Y" "Z" "W" "V" "U" "T" "R" "Q" "A" "B" "C" "D" "E" "F")))
  (labels
      ((add_! (cat)
	 (typecase cat
	   (basic-category
	    (setf (markedup-basic-category-id cat) "_"))
	   (complex-category
	    (setf (markedup-complex-category-id cat) "_")
	    (add_! (complex-category-result cat))
	    (add_! (argument-category (complex-category-argument cat))))))

       (set-id! (cat ids)
	 (when (null ids)
	   (return-from assign-id! nil))
	 (typecase cat
	   (basic-category
	    (setf (markedup-basic-category-id cat) (first ids)))
	   (complex-category
	    (setf (markedup-complex-category-id cat) (first ids))))
	 (rest ids))

       (assign! (cat ids)
	 (cond
	   ((typep cat 'basic-category)
	    (set-id! cat ids))
	   ((adjunct? cat)
	    (let ((rest-ids (assign! (complex-category-result cat) ids)))
	      (assign! (argument-category (complex-category-argument cat)) ids)
	      (set-id! cat rest-ids)))
	   (t
	    (let* ((rest-ids1 (assign! (complex-category-result cat) ids))
		   (rest-ids2 (assign! (argument-category (complex-category-argument cat)) rest-ids1)))
	      (set-id! cat rest-ids2)))))

       (traverse! (cat ids)
	 (cond
	   ((typep cat 'basic-category)
	    ids)
	   ((adjunct? cat)
	    (assign! (complex-category-result cat) ids)
	    (assign! (argument-category (complex-category-argument cat)) ids))
	   (t
	    (let ((rest-ids (traverse! (complex-category-result cat) ids)))
	      (assign! (argument-category (complex-category-argument cat)) rest-ids))))))
    (add_! cat)
    (traverse! cat ids)
    cat))

(defun add-S-Xfeature! (cat)
  (labels
      ((add-X! (cat)
	 (typecase cat
	   (basic-category
	    (when (string= (basic-category-symbol cat) "S")
	      (setf (basic-category-feature cat) "[X]")))
	   (complex-category
	    (add-X! (complex-category-result cat))
	    (add-X! (argument-category (complex-category-argument cat))))))
       (traverse (cat)
	 (cond
	   ((typep cat 'basic-category)
	    nil)
	   ((adjunct? cat)
	    (add-X! cat))
	   (t
	    (traverse (complex-category-result cat))
	    (traverse (argument-category (complex-category-argument cat)))))))
    (traverse cat)
    cat))

(defun markup (cat)

  (setf cat (category->markedup-category cat))

  ;; assign slot indexes
  (loop
     :for i :from (category-number-of-slots cat) :downto 1
     :for c := cat :then (complex-category-result c)
     :for ac := (argument-category (complex-category-argument c)) :then (argument-category (complex-category-argument c))
     :do
       (assign-index! ac i))

  (when (null (assign-id! cat))
    (return-from markup nil))

  (add-S-Xfeature! cat)

  (markedup-category->string cat))

(defun markedup-category->string (cat)
  (typecase cat
    (basic-category
     (format nil "~a~a{~a}~a"
	     (basic-category-symbol cat)
	     (or (basic-category-feature cat) "")
	     (markedup-basic-category-id cat)
	     (or (markedup-basic-category-index cat) "")))
    (complex-category
     (format nil "(~a~a){~a}~a"
	     (markedup-category->string (complex-category-result cat))
	     (markedup-argument->string (complex-category-argument cat))
	     (markedup-complex-category-id cat)
	     (or (markedup-complex-category-index cat) "")))))

(defun markedup-argument->string (arg)
  (format nil "~a~a"
	  (argument-directionality arg)
	  (markedup-category->string (argument-category arg))))

(defun create-markedup-file (&key auto markedup output)
  (with-open-file (out output :direction :output)
    ;; copy markedup
    (with-open-file (in markedup)
      (do ((line))
	  ((null (setf line (read-line in nil nil))))
	(write-line line out)))

    ;; add new lexical categories
    (dolist (str (oov-categories auto markedup))
      (let* ((cat (string->category str))
	     (mcat (markup cat)))
	(when mcat
	  (format out "~%~a~%  ~a ~a~%"
		  str
		  (category-number-of-slots cat)
		  mcat))))))
