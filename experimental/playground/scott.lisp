;;; Note some parts of my code uses my standard library
;;; This stdlib: can be loaded by the asdf file in the following link
;;; https://github.com/mariari/Misc-Lisp-Scripts


;; Nats definition----------------------------------------------------

(defun duplicate (n1)
  (lambda (n2)
    (lambda (zer)
      (declare (ignore zer))
      (lambda (succ)
        (declare (ignore succ))
        (lambda (duplicate)
          (funcall (funcall duplicate n1) n2))))))

(defun succ (x)
  (lambda (n)
    (declare (ignore n))
    (lambda (succ)
      (lambda (duplicate)
        (declare (ignore duplicate))
        (funcall succ x)))))

(defun zero (ni)
  (lambda (succ)
    (declare (ignore succ))
    (lambda (duplicate)
      (declare (ignore duplicate))
      ni)))


;; (funcall (duplicate (succ (succ (succ #'zero))))
;;          (succ (succ (succ #'zero))))

;; Nats that expand the lambdas for me!

(defun duplicate-disp (n1)
  (lambda (n2)
    `(lambda (zer)
       (lambda (succ)
         (lambda (duplicate)
           (funcall (funcall duplicate ,n1) ,n2))))))

(defun succ-disp (x)
  `(lambda (n)
     (lambda (succ)
       (lambda (duplicate)
         (funcall succ ,x)))))

(defparameter zero-disp
  `(lambda (ni)
    (lambda (succ)
       (lambda (duplicate)
         ni))))

(funcall (duplicate-disp
          (succ-disp (succ-disp (succ-disp #'zero-disp))))
         (succ-disp (succ-disp (succ-disp #'zero-disp))))

;; Some functions to test this encoding on--------------------------------------
(defun pred (nat)
  (funcall
   (funcall
    (funcall nat #'zero)
    (lambda (n) n))
   (lambda (d1)
     (lambda (d2)
       (funcall (duplicate (pred d1))
                (pred d2))))))

(defun is-even (nat)
  (funcall
   (funcall
    (funcall nat t)
    (lambda (n) (not (is-even n))))
   (lambda (d1)
     (lambda (d2)
       (eq (is-even d1) (is-even d2))))))
