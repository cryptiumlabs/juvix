;; This Package gives the proper functions for org-generation of a
;; language to work

(defpackage #:org-generation/lisp
  (:use #:cl
        #:org-generation/types
        #:org-generation/maybe
        #:org-generation/type-signature)
  (:nicknames #:og/lisp)
  (:export #:module-comments
           #:import-generation
           #:initalize
           #:*extension*
           #:convert-path))

(in-package :org-generation/lisp)


;; -----------------------------------------------------------------------------
;; Context
;; -----------------------------------------------------------------------------

(defstruct config
  ;; type is [maybe path], but don't wish to force that upon the config
  asdf)

(defstruct context
  ;; this map is a mapping [file -> list file]
  ;; where the list of files is the files said file imports
  ;; this is also why convert path is basically id!
  (file-import (fset:empty-map) :type fset:map))
;; -----------------------------------------------------------------------------
;; Exporting Interface Functions
;; -----------------------------------------------------------------------------


(defparameter *extension* "lisp")

(sig import-generation (-> context fset:map file-info-extended list))
(defun import-generation (context conflict-map file-context)
  (let ((a 2))
    (fset:lookup (context-file-import context) (file-info-extended-path file-context))
    (list a)))

;; TODO :: decide if module-comments should get the context as well
(defun module-comments (file-context &optional (level 0))
  t)


(defun initialize (sexp)
  "Initializes the context for the Haskell configuration"
  (apply #'make-config sexp))


(defun convert-path (file context)
  (declare (ignore context))
  file)
