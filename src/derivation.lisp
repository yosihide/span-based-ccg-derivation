(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.derivation
  (:use :cl
	:cl-ppcre
	:span-based-ccg-derivation.category
	:span-based-ccg-derivation.tree)
  (:export :d-internal-node
	   :make-d-internal-node
	   :head
	   :d-terminal-node
	   :make-d-terminal-node
	   :d-terminal-node-pos
	   :d-terminal-node-predarg-cat
	   :supertags
	   :read-derivation
	   :read-derivations
	   :do-stream-derivation
	   :write-derivation
	   :write-derivations
	   ))

(in-package :span-based-ccg-derivation.derivation)

(defstruct (d-internal-node (:include internal-node))
  head
  )

(defun head (node)
  (d-internal-node-head node))

(defstruct (d-terminal-node (:include terminal-node))
  mod-pos
  orig-pos
  predarg-cat
  )

(defun d-terminal-node-pos (node)
  (d-terminal-node-mod-pos node))

(defun supertags (derivation)
  (typecase derivation
    (d-terminal-node
     (list (node-label derivation)))
    (d-internal-node
     (mapcan #'supertags (children derivation)))))

;; reader, writer
(defvar +eos+ (gensym))

;;; reader
(defun whitespace? (x)
  (member x '(#\Linefeed #\Newline #\Page #\Return #\Space #\Tab) :test #'char=))

(defun expected-char (char stream &key skip?)
  (when (not (char= (peek-char t stream) char))
    (error "error: expected ~a~%" char))
  (when skip?
    (read-char stream)))

(defun make-char-buffer nil
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun push-char-buffer (x buf)
  (vector-push-extend x buf))

(defun get-char-buffer (buf)
  (let ((str (make-array (length buf) :element-type 'character)))
    (dotimes (i (length buf))
      (setf (char str i) (char buf i)))
    (setf (fill-pointer buf) 0)
    str))

(defun read-string (stream &key separator?)
  (declare (function separator?))
  (let ((buf (make-char-buffer)))
    (loop
       :while (not (funcall separator? (peek-char nil stream)))
       :do
	 (push-char-buffer (read-char stream) buf))
    (get-char-buffer buf)))

(defun read-derivation (&optional (stream t) (eof-error-p t) eof-value)

  (when (and (not eof-error-p)
	     (eq (peek-char t stream nil +eos+) +eos+))
      (return-from read-derivation eof-value))

  ;; skip ID
  (when (eq (peek-char nil stream) #\I )
    (read-line stream))

  (expected-char #\( stream :skip? t)
  (expected-char #\< stream :skip? t)
  (peek-char t stream)

  (let ((node-type (read-string stream :separator? #'whitespace?))
	(result nil)
	(children nil))
    (peek-char t stream)
    (cond
      ;; internal-node
      ((string= node-type "T")
       (destructuring-bind (cat head dtrs) (split " " (read-string stream :separator? #'(lambda (x) (char= x #\>))))
	 (setf result
	       (make-d-internal-node :label (string->category cat)
				     :head (read-from-string head)))
	 (peek-char t stream)
	 (expected-char #\> stream :skip? t)
	 (loop
	    :repeat (read-from-string dtrs)
	    :do
	    (push (read-derivation stream) children)))
       (setf (children result) (nreverse children))
       (expected-char #\) stream :skip? t)
       result
       )
      ;; terminal-node
      ((string= node-type "L")
       (destructuring-bind (cat mod-pos orig-pos word predarg-cat)
	   (split " " (read-string stream :separator? #'(lambda (x) (char= x #\>))))
	 (setf result
	       (make-d-terminal-node :label (string->category cat)
				     :mod-pos mod-pos
				     :orig-pos orig-pos
				     :word word
				     :predarg-cat predarg-cat)))
       (expected-char #\> stream :skip? t)
       (expected-char #\) stream :skip? t)
       result)
      (t
       (error "error in read-derivation")))))

(defmacro do-stream-derivation ((x stream &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(let ((,gstrm ,stream))
      (do ((,x))
	  ((eq (setf ,x (read-derivation ,gstrm nil +eos+)) +eos+) ,result-form)
	,@body))))

(defmacro do-file-derivation ((x path &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(with-open-file (,gstrm ,path)
      (do-stream-derivation (,x ,gstrm ,result-form) ,@body))))

(defun read-derivations (file)
  (let ((derivations nil))
    (do-file-derivation (x file)
      (push x derivations))
    (nreverse derivations)))

;; writer
(defun write-derivation (node out)
  (format out "(")
  (case (type-of node)
    (d-internal-node
     (format out "<T ~a ~a ~a>"
	     (category->string (node-label node))
	     (d-internal-node-head node)
	     (length (children node)))
     (dolist (x (children node))
       (format out " ")
       (write-derivation x out))
     (format out " )"))
    (d-terminal-node
     (format out "<L ~a ~a ~a ~a ~a>)"
	     (category->string (node-label node))
	     (d-terminal-node-mod-pos node)
	     (d-terminal-node-orig-pos node)
	     (d-terminal-node-word node)
	     (d-terminal-node-predarg-cat node)))))

(defun write-derivations (nodes file &key id?)
  (with-open-file (out file :direction :output)
    (loop
       :for node :in nodes
       :for i :from 1
       :do
       (when id? (format out "ID=~a~%" i))
       (write-derivation node out)
       (terpri out)))
  t)
