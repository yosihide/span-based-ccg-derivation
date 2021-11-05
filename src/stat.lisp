(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :span-based-ccg-derivation.stat
  (:use :cl
	:span-based-ccg-derivation.category
	:span-based-ccg-derivation.tree
	:span-based-ccg-derivation.derivation
	:span-based-ccg-derivation.converter
	:span-based-ccg-derivation.util)
  (:export :oov
	   :analyze-violation
	   :analyze-vocabulary))

(in-package :span-based-ccg-derivation.stat)

(defun supertag-strings (derivation)
  (mapcar #'category->string (supertags derivation)))

(defun oov (&key training test-gold test-predicted)
  (let ((vocabulary (make-hash-table :test 'equal)))
    (with-open-file (in training)
      (do-stream-derivation (d in)
	(dolist (s (supertag-strings d))
	  (setf (gethash s vocabulary) t))))

    (let ((gold-stags-list nil)
	  (predicted-stags-list nil))
      (with-open-file (in test-gold)
	(do-stream-derivation (d in)
	  (push (supertag-strings d) gold-stags-list)))
      (with-open-file (in test-predicted)
	(do-stream-derivation (d in)
	  (push (supertag-strings d) predicted-stags-list)))
      (let ((gold-predicted-count 0)
	    (gold-count 0))
	(loop
	   :for gold-stags :in (reverse gold-stags-list)
	   :for predicted-stags :in (reverse predicted-stags-list)
	   :do
	   (loop
	      :for g-stag :in gold-stags
	      :for p-stag :in predicted-stags
	      :do
	      (when (not (gethash g-stag vocabulary))
		(incf gold-count)
		(format t "~a is out of vocabulary." g-stag)
		(cond
		  ((string= g-stag p-stag)
		   (format t " This was correctly predicted.~%")
		   (incf gold-predicted-count))
		  (t
		   (terpri t))))))
	(format t "-------------------------------------------~%")
	(format t "Number of occurrences of oov categories:~a ~%" gold-count)
	(format t "Number of occurrences of oov categories correctly predicted:~a ~%" gold-predicted-count))))
  nil)

(defun extract-violations (derivation)
  (typecase derivation
    (d-terminal-node
     nil)
    (d-internal-node
     (cond
       ((unary? derivation)
	(extract-violations (node-ref derivation 0)))
       ((not (or (span-based-ccg-derivation.converter::forward-composition? derivation)
		 (span-based-ccg-derivation.converter::backward-composition? derivation)
		 (span-based-ccg-derivation.converter::backward-substitution? derivation)
		 (span-based-ccg-derivation.converter::conj-punct-rule? derivation)))
	(cons (list (category->string (node-label (node-ref derivation 0)))
		    (category->string (node-label (node-ref derivation 1)))
		    (category->string (node-label derivation)))
	      (mapcan #'extract-violations (children derivation))))
       (t
	(mapcan #'extract-violations (children derivation)))))))

(defun analyze-violation (auto)
  (let ((count 0)
	(i 0))
    (with-open-file (in auto)
      (do-stream-derivation (derivation in)
	(incf i)
	(let ((v (length (extract-violations derivation))))
	  (when (> v 0)
	    (incf count v)
	    (format t "~a: ~a~%" i v)))))
    (format t "----------------~%total: ~a~%" count)
    ))

(defun analyze-vocabulary (auto)
  (let ((lexcat-counter (make-hash-table :test 'equal))
	(cat-counter (make-hash-table :test 'equal))
	(sbrlabel-counter (make-hash-table :test 'equal)))
    (with-open-file (in auto)
      (do-stream-derivation (derivation in)
	(dolist (lexcat (mapcar #'(lambda (x) (category->string (node-label x))) (terminal-nodes derivation)))
	  (incf (gethash lexcat lexcat-counter 0))
	  (incf (gethash lexcat cat-counter 0)))
	(dolist (cat (mapcar #'(lambda (x) (category->string (node-label x))) (internal-nodes derivation)))
	  (incf (gethash cat cat-counter 0)))
	(dolist (sbrlabel (mapcar #'node-label (internal-nodes (derivation->sb-derivation derivation))))
	  (incf (gethash sbrlabel sbrlabel-counter 0)))))
    (format t "lexcat: ~a~%" (hash-table-count lexcat-counter))
    (format t "cat: ~a~%" (hash-table-count cat-counter))
    (format t "SBR label: ~a~%" (hash-table-count sbrlabel-counter))))
