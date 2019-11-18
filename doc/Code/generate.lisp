(ql:quickload "cl-org-mode")

(asdf:load-system :uiop)

(defpackage #:code-generation
  (:use #:common-lisp))

(in-package :code-generation)

;; (uiop:directory-files "../../holder/")

(defun generate-org-file (directory)
  (let ((files    (uiop:directory-files directory))
        (sub-dirs (uiop:subdirectories directory)))
    ))
