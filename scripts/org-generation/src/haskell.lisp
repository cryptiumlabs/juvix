;; This Package gives the proper functions for org-generation of a
;; language to work

(defpackage #:org-generation/haskell
  (:use #:cl
        #:org-generation/types
        #:org-generation/maybe
        #:org-generation/type-signature)
  (:nicknames #:og/haskell)
  (:export #:module-comments
           #:import-generation
           #:initalize
           #:*extension*
           #:convert-path))

(in-package :org-generation/haskell)

;; -----------------------------------------------------------------------------
;; Context
;; -----------------------------------------------------------------------------

(defstruct context
  (name (error "initalize-context") :type string)
  (dir-before-code (list "src" "app" "test") :type list))

;; -----------------------------------------------------------------------------
;; Exporting Interface Functions
;; -----------------------------------------------------------------------------

(defparameter *extension* "hs")


(sig import-generation (-> context fset:map list list))
(defun import-generation (context conflict-map lines)
  (haskell-import-to-org-alias (relevent-imports-haskell lines (context-name context))
                               conflict-map))

;; TODO :: decide if module-comments should get the context as well
(sig module-comments (-> list fixnum list))
(defun module-comments (file-lines level)
  (let* ((module-comments
           (remove-if (lambda (x)
                        (or (uiop:string-prefix-p "{-#" x) (equal "" x)))
                      (og/utility:take-until (lambda (x) (uiop:string-prefix-p "module" x))
                                             file-lines)))
         (special (car module-comments))
         (valid-module (if special
                           (uiop:string-prefix-p "-- |" special)
                           nil)))
    (flet ((update-headline-level (line)
             (if (uiop:string-prefix-p "*" line)
                 (concatenate 'string
                              (og/utility:repeat-s level "*")
                              line)
                 line)))
      (if valid-module
          (remove-if #'uiop:emptyp
                     (mapcar #'update-headline-level
                      (cons (strip-haskell-comments special t)
                            (mapcar #'strip-haskell-comments (cdr module-comments)))))
          nil))))


(defun initialize (sexp)
  "Initializes the context for the Haskell configuration"
  (apply #'make-context sexp))

(sig convert-path (-> pathname context string))
(defun convert-path (file context)
  "converts a pathname to the haskell import name"
  (labels ((last-dir-before (xs)
             (let ((next
                     (reduce (lambda (acc y)
                               (if acc
                                   acc
                                   (member y xs :test #'equal)))
                             (context-dir-before-code context)
                             :initial-value  nil)))
               (if next
                   (last-dir-before (cdr next))
                   xs))))
    (og/utility:reconstruct-path (append
                                  (last-dir-before (pathname-directory file))
                                  (list (pathname-name file)))
                                 ".")))

;; -----------------------------------------------------------------------------
;; Imports to Org Aliases
;; -----------------------------------------------------------------------------


(sig haskell-import-to-org-alias (-> list fset:map list))
(defun haskell-import-to-org-alias (imports conflict-map)
  "takes a list of Haskell imports and transforms them into their org-mode alias"
  (mapcar (lambda (import)
            (or (fset:lookup conflict-map import)
                (car (last (uiop:split-string import :separator ".")))))
          imports))



(sig relevent-imports-haskell (-> list string list))
(defun relevent-imports-haskell (lines project-name)
  "takes LINES of a source code, and a PROJECT-NAME and filters for imports
that match the project name"
  (let* ((imports     (remove-if (complement (lambda (x)
                                               (uiop:string-prefix-p "import" x)))
                                 lines))
         (modules     (mapcar (lambda (import)
                                ;; TODO make this cadr logic generic
                                (let ((split-import (uiop:split-string import)))
                                  (cond
                                    ((equalp (cadr split-import) "qualified")
                                     (caddr split-import))
                                    (t
                                     (cadr split-import)))))
                              imports))
         (project-imp (remove-if (complement (lambda (m)
                                               (uiop:string-prefix-p project-name m)))
                                 modules)))
    project-imp))

;; -----------------------------------------------------------------------------
;; Haskell comment clean up
;; -----------------------------------------------------------------------------

(sig strip-haskell-comments (-> sequence &optional Boolean sequence))
(defun strip-haskell-comments (line &optional start-doc)
  (subseq line (min (if start-doc 5 3) (length line))))
