(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.ptb
  (:use :cl
	:span-based-ccg-derivation.tree)
  (:export :read-tree
	   :read-trees
	   :do-stream-tree
	   :write-tree
	   :write-trees
	   ))

(in-package :span-based-ccg-derivation.ptb)

(defvar +eos+ (gensym))

(defun whitespace? (x)
  (member x '(#\Linefeed #\Newline #\Page #\Return #\Space #\Tab) :test #'char=))

(defun separator? (x)
  (or (char= x #\()
      (char= x #\))
      (whitespace? x)))

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

(defun read-string (stream)
  (let ((buf (make-char-buffer)))
    (loop
       :while (not (separator? (peek-char nil stream)))
       :do
       (vector-push-extend (read-char stream) buf))
    (get-char-buffer buf)))

(defun expected-char (char stream &key skip?)
  (when (not (char= (peek-char t stream) char))
    (error "error: expected ~a~%" char))
  (when skip?
    (read-char stream)))

(defun unexpected-char (char stream)
  (when (char= (peek-char t stream) char)
    (error "error: unexpected ~a~%" char)))

(defun read-tree-aux (stream)

  (expected-char #\( stream :skip? t)

  (let ((root-label nil)
	(children nil)
	(result nil))

    ;; read label
    (unexpected-char #\) stream)

    (cond
      ((char= (peek-char t stream) #\()
       (setf root-label "TOP"))
      (t
       (setf root-label (read-string stream))))

    (unexpected-char #\) stream)

    (cond
      ;; internal node
      ((char= (peek-char t stream) #\()
       (loop
	  :while (char= (peek-char t stream) #\()
	  :do
	  (push (read-tree-aux stream) children))
       (setf result
	     (make-internal-node :label root-label
				 :children (nreverse children))))
      ;; terminal
      (t
       (setf result (make-terminal-node :label root-label :word (read-string stream)))))

    (expected-char #\) stream :skip? t)

    result))

(defun read-tree (&optional (stream t) (eof-error-p t) eof-value)
  (if (and (not eof-error-p)
	   (eq (peek-char t stream nil +eos+) +eos+))
      (return-from read-tree eof-value)
      (read-tree-aux stream)))

(defmacro do-stream-tree ((x stream &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(let ((,gstrm ,stream))
      (do ((,x))
	  ((eq (setf ,x (read-tree ,gstrm nil +eos+)) +eos+) ,result-form)
	,@body))))

(defmacro do-file-tree ((x path &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(with-open-file (,gstrm ,path)
      (do-stream-tree (,x ,gstrm ,result-form) ,@body))))

(defun read-trees (file)
  (let ((trees nil))
    (do-file-tree (x file)
      (push x trees))
    (nreverse trees)))

;; writer
(defun write-tree (tree out)
  (let ((label (node-label tree)))
    (format out "(")
    (format out "~a" label)
    (case (type-of tree)
      (internal-node
       (dolist (x (children tree))
	 (format out " ")
	 (write-tree x out)))
      (terminal-node
       (format out " ~a" (word tree))))
    (format out ")")))

(defun write-trees (trees file)
  (with-open-file (out file :direction :output)
    (dolist (tree trees)
      (write-tree tree out)
      (terpri out)))
  t)
