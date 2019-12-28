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
           #:file-info-alias

           #:file-info-extended
           #:file-info-extended-p
           #:make-file-info-extended
           #:file-info-extended-path
           #:file-info-extended-alias
           #:file-info-extended-lines

           #:extend-file-info)
  (:use #:cl
        #:org-generation/maybe
        #:org-generation/type-signature))

(in-package :org-generation/types)

(defstruct org-directory
  ;; some directories have a file that re-export things
  ;; let it be a maybe file-info
  (file +nothing+ :type maybe)
  ;; a dir consists of either a file-info-p or a org-directory-p
  (dir  nil       :type list)
  (name ""        :type string))

(defstruct file-info
  (path  #p""      :type pathname)
  (alias +nothing+ :type maybe))

(defstruct (file-info-extended (:include file-info))
  (lines nil :type list))


(sig extend-file-info (-> file-info list file-info-extended))
(defun extend-file-info (file-info lines)
  (make-file-info-extended :path  (file-info-path  file-info)
                           :alias (file-info-alias file-info)
                           :lines lines))
