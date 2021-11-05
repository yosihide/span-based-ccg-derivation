(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.tree
  (:use :cl)
  (:export :node
	   :make-node
	   :node-label
	   :internal-node
	   :make-internal-node
	   :children
	   :terminal-node
	   :make-terminal-node
	   :word
	   :unary?
	   :node-ref
	   :internal-nodes
	   :terminal-nodes
	   ))

(in-package :span-based-ccg-derivation.tree)

(defstruct node
  label
  )

(defstruct (internal-node (:include node))
  children
  )

(defun children (node)
  (internal-node-children node))

(defun (setf children) (c node)
  (setf (internal-node-children node) c))

(defstruct (terminal-node (:include node))
  word
  )

(defun word (node)
  (terminal-node-word node))

(defun unary? (node)
  (and (typep node 'internal-node)
       (= (length (children node)) 1)))

(defmacro node-ref (node &rest indexes)
  (if (null indexes)
      node
      `(when (typep ,node 'internal-node)
	 (node-ref (nth ,(car indexes) (children ,node)) ,@(cdr indexes)))))

(defun internal-nodes (node)
  (typecase node
    (terminal-node
     nil)
    (internal-node
     (cons node (mapcan #'internal-nodes (children node))))))

(defun terminal-nodes (node)
  (typecase node
    (terminal-node
     (list node))
    (internal-node
     (mapcan #'terminal-nodes (children node)))))
