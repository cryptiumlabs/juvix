(uiop:define-package :org-generation/agda
  (:use #:cl)
  (:use-reexport :org-generation/haskell))

(in-package :org-generation/agda)

(defparameter *extension* "agda")
