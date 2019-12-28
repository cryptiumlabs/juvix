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
  (lisp-import-to-org-alias
   (fset:lookup (context-file-import context)
                (file-info-extended-path file-context))
   conflict-map))

(defun module-comments (file-context)
  (let* ((comments (og/utility:take-until
                    (complement (lambda (x)
                                  ;; TODO improve this. support " ;", " "
                                  (or (uiop:emptyp x) (uiop:string-prefix-p ";" x))))
                    (file-info-extended-lines file-context))))
    (mapcar #'strip-lisp-comments comments)))


(defun initialize (sexp)
  "Initializes the context for the Lisp configuration"
  (let ((config (apply #'make-config sexp)))
    config))


(defun convert-path (file context)
  (declare (ignore context))
  file)

;; -----------------------------------------------------------------------------
;; Imports to Org Aliases
;; -----------------------------------------------------------------------------
(sig lisp-import-to-org-alias (-> list fset:map list))
(defun lisp-import-to-org-alias (imports conflict-map)
  "takes a list of lisp imports and transforms them into their org-mode alias"
  (mapcar (lambda (import)
            (or (fset:lookup conflict-map import)
                (pathname-name import)))
          imports))

;; -----------------------------------------------------------------------------
;; Lisp comment clean up
;; -----------------------------------------------------------------------------

(sig strip-lisp-comments (-> sequence sequence))
(defun strip-lisp-comments (line)
  (string-left-trim  (list #\; #\Space #\Tab) line))
