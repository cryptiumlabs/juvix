(ql:quickload "cl-org-mode")
(ql:quickload "trivia")
(ql:quickload "fset")

(asdf:load-system :uiop)

(defpackage #:code-generation
  (:use #:common-lisp))

(in-package :code-generation)

;; (uiop:directory-files "../../holder/")
;; Maybe type -----------------------------------------------
;; todo better type it
(defconstant +nothing+ :none)

(defstruct just val)

(deftype maybe ()
    `(or
      (eql :none)
      (satisfies just-p)))

;; Directory and file types ---------------------------------
(defstruct org-directory
  ;; some directories have a file that re-export things
  (file +nothing+ :type maybe)
  ;; a dir consists of either a file-info-p or a org-directory-p
  (dir  nil       :type list))

(defstruct file-info
  (path  ""        :type pathname)
  (alias +nothing+ :type maybe))


(defun generate-org-file (directory)
  ;; (labels ((rec (directory conflicts))))
  (let ((files    (uiop:directory-files directory))
        (sub-dirs (uiop:subdirectories directory)))
    ))
