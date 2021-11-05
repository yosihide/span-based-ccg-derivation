(defpackage :span-based-ccg-derivation.system
  (:use :cl
	:asdf))

(in-package :span-based-ccg-derivation.system)

(defsystem "span-based-ccg-derivation"
  :depends-on (:cl-ppcre)
  :description "A new representation for CCG derivation"
  :version "0.1"
  :author "Yoshihide Kato"
  :components ((:file "util")
	       (:file "category")
	       (:file "pattern")
	       (:file "tree")
	       (:file "derivation")
	       (:file "ptb")
	       (:file "converter")
	       (:file "markup")
	       (:file "stat")
	       (:file "misc")
	       ))
