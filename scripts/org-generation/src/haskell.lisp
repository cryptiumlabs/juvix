;; This Package gives the proper functions for org-generation of a
;; language to work

(defpackage #:org-generation/haskell
  (:use #:cl #:org-generation/types #:org-generation/maybe)
  (:nicknames #:og/haskell))

(in-package :org-generation/haskell)
