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
           #:*extension*))

(in-package :org-generation/lisp)


;; -----------------------------------------------------------------------------
;; Context
;; -----------------------------------------------------------------------------

(defstruct context
  ;; type is [maybe path], but don't wish to force that upon the config
  asdf)

;; -----------------------------------------------------------------------------
;; Exporting Interface Functions
;; -----------------------------------------------------------------------------


(defparameter *extension* "lisp")

(defun import-generation (context conflict-map lines)
  t)

;; TODO :: decide if module-comments should get the context as well
(defun module-comments (file-lines &optional (level 0))
  t)


(defun initialize (sexp)
  "Initializes the context for the Haskell configuration"
  (apply #'make-context sexp))
