(defpackage #:org-generation/utility
  (:use #:cl #:org-generation/type-signature)
  (:nicknames #:og/utility)
  (:export #:repeat
           #:repeat-s
           #:take-until
           #:file-name
           #:reconstruct-path
           #:update-headline-level))

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

;; remove to an org specific file
(sig update-headline-level (-> string fixnum string))
(defun update-headline-level (line level)
  (if (uiop:string-prefix-p "*" line)
      (concatenate 'string
                   (repeat-s level "*")
                   line)
      line))

;; -----------------------------------------------------------------------------
;; File Naming Helpers
;; -----------------------------------------------------------------------------

(sig file-name (-> pathname &optional fixnum string))
(defun file-name (file &optional (extra-context 1))
  "takes a file and how much extra context is needed to disambiguate the file
Returns a string that reconstructs the unique identifier for the file"
  (let ((name (pathname-name file)))
    ;; directories have no name!
    (cond ((not name)
           (car (last (pathname-directory file))))
          ((= extra-context 1)
           name)
          (t
           (let ((disambigous-path (last (pathname-directory file)
                                         (1- extra-context))))
             (reconstruct-path (append disambigous-path (list name))))))))

(sig reconstruct-path (-> list &optional string string))
(defun reconstruct-path (xs &optional (connector "/"))
  (apply #'concatenate
         'string
         (butlast (mapcan (lambda (s) (list s connector))
                          xs))))
