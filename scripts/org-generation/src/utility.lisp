(defpackage #:org-generation/utility
  (:use #:cl #:org-generation/type-signature)
  (:nicknames #:og/utility)
  (:export #:repeat
           #:repeat-s
           #:take-until))

(in-package :org-generation/utility)

(defun repeat (n thing)
  "repeats THING N times"
  (loop for i from 0 to (1- n) collect thing))

(sig repeat-s (-> fixnum string string))
(defun repeat-s (n thing)
  (apply #'concatenate 'string (repeat n thing)))

(defun take-until (pred xs)
  (labels ((rec (current acc)
             (if (or (null current) (funcall pred (car current)))
                 (reverse acc)
                 (rec (cdr current) (cons (car current) acc)))))
    (rec xs nil)))
