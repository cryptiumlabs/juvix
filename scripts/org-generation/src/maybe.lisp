(defpackage #:org-generation/maybe
  (:use #:common-lisp)
  (:export
   :just     :+nothing+ :maybe  :nothing?
   :map-just :copy-just :just-p :just-val
   :make-just))

(in-package :org-generation/maybe)

;; TODO better type it
(defconstant +nothing+ :none)

(defstruct just val)

(defun map-just (f val)
  (if (just-p val)
      (make-just :val (funcall f (just-val val)))
      val))

(deftype maybe ()
    `(or
      (eql :none)
      (satisfies just-p)))

(defun nothing? (x)
  (eq +nothing+ x))
