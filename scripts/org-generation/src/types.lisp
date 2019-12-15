(defpackage #:org-generation/types
  (:export #:org-directory
           #:org-directory-p
           #:org-directory-name
           #:copy-org-directory
           #:make-org-directory
           #:org-directory-file
           #:org-directory-name
           #:org-directory-dir
           #:file-info
           #:file-info-p
           #:file-info-name
           #:copy-file-info
           #:make-file-info
           #:file-info-path
           #:file-info-alias)
  (:use #:cl
        #:org-generation/maybe))

(in-package :org-generation/types)

(defstruct org-directory
  ;; some directories have a file that re-export things
  ;; let it be a maybe file-info
  (file +nothing+ :type maybe)
  ;; a dir consists of either a file-info-p or a org-directory-p
  (dir  nil       :type list)
  (name ""        :type string))

(defstruct file-info
  (path  ""        :type pathname)
  (alias +nothing+ :type maybe))
