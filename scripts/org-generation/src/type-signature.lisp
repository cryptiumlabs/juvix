(defpackage #:org-generation/type-signature
  (:use #:cl)
  (:export #:-> #:sig))

(in-package :org-generation/type-signature)

(defmacro sig (f type)
  `(declaim (ftype ,(if (and (listp type) (eq '-> (car type)))
                        (macroexpand-1 type)
                        type)
                   ,f)))

(defmacro -> (&rest args)
  (let ((return-type (car (last args))))
    `(function
      ;; recuresively expand the argument
      ,(mapcar  (lambda (arg)
                 (if (and (listp arg) (eq '-> (car arg)))
                     (macroexpand-1 arg)
                     arg))
                (butlast args))
      ,return-type)))
