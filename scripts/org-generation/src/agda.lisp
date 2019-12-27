(uiop:define-package :org-generation/agda
  (:use #:cl)
  (:shadow :*extension*)
  (:use-reexport :org-generation/haskell))

(in-package :org-generation/agda)

(defparameter *extension* "agda")
