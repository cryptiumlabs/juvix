(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "fset")
  (asdf:load-system :uiop))

(defpackage #:code-generation
  (:use #:common-lisp))

(in-package :code-generation)

;; (uiop:directory-files "../../holder/")
;; -----------------------------------------------------------------------------
;; Maybe type
;; -----------------------------------------------------------------------------

;; todo better type it
(defconstant +nothing+ :none)

(defstruct just val)

(deftype maybe ()
    `(or
      (eql :none)
      (satisfies just-p)))

(defun nothing? (x)
  (eq +nothing+ x))

;; -----------------------------------------------------------------------------
;; Directory and file types
;; -----------------------------------------------------------------------------

(defstruct org-directory
  ;; some directories have a file that re-export things
  (file +nothing+ :type maybe)
  ;; a dir consists of either a file-info-p or a org-directory-p
  (dir  nil       :type list))

(defstruct file-info
  (path  ""        :type pathname)
  (alias +nothing+ :type maybe))

;; -----------------------------------------------------------------------------
;; Main Functionality
;; -----------------------------------------------------------------------------

(defun generate-org-file (directory)
  ;; (labels ((rec (directory conflicts))))
  (let ((files    (uiop:directory-files directory))
        (sub-dirs (uiop:subdirectories directory)))
    3))

;; -----------------------------------------------------------------------------
;; Getting Directory and File lists
;; -----------------------------------------------------------------------------


(defun files-and-dirs (directory)
  "recursively grabs the file and directories
forming a list of org-directory and file info"
  (let* ((files          (uiop:directory-files directory))
         (sub-dirs       (uiop:subdirectories  directory))
         (dirs-annotated (mapcar (lambda (dir)
                                   (let* ((name  (file-name dir))
                                          (file? (find-if
                                                  (lambda (x)
                                                    (equal (file-name x) name))
                                                  files)))
                                     (make-org-directory
                                      :file (if file?
                                                (make-just :val file?)
                                                +nothing+)
                                      :dir  (files-and-dirs dir))))
                                 sub-dirs))
         ;; slow version of set-difference that maintains ordering
         (files-annotated
           (mapcar (lambda (file)
                     (make-file-info :path file))
            (remove-if (lambda (file)
                         (member-if (lambda (dir-ann)
                                      (if (nothing? (org-directory-file dir-ann))
                                          nil
                                          (equal (just-val (org-directory-file dir-ann))
                                                 file)))
                                    dirs-annotated))
                       files))))
    (append files-annotated dirs-annotated)))

(defun lose-dir-information (file-dir-list)
  "forgets the directory information and puts all files into a flat list"
  (mapcan (lambda (file-dir)
            (cond ((and (org-directory-p file-dir)
                        (just-p (org-directory-file file-dir)))
                   (cons (just-val (org-directory-file file-dir))
                         (lose-dir-information (org-directory-dir file-dir))))
                  ((org-directory-p file-dir)
                   (lose-dir-information (org-directory-dir file-dir)))
                  (t
                   (list (file-info-path file-dir)))))
          file-dir-list))

;; -----------------------------------------------------------------------------
;; Handling Conflicting Files
;; -----------------------------------------------------------------------------

(defun consturct-file-alias-map (files)
  "finds any files that share the same identifier
and returns a map of files to their alias"
  (flet ((add-file (map file)
           (let* ((name   (file-name file))
                  (lookup (fset:lookup map name))) ; returns nil if none found
             (fset:with map name (cons file lookup))))
         (add-conflicts (map key conflicting-files)
           (declare (ignore key))
           (if (cdr conflicting-files)
               (fset:map-union map
                               (disambiguate-files conflicting-files
                                                   :all-conflicts t))
               map)))
    (let* ((conflict-map
             (reduce #'add-file files :initial-value (fset:empty-map))))
      (fset:reduce #'add-conflicts conflict-map :initial-value (fset:empty-map)))))


;; may do the job of consturct-file-alias-map, however this algorithm is slow
;; for a large amount of files
(defun disambiguate-files (file-list &key all-conflicts)
  "takes a list of files and returns a map from
the file name to their unique identifier"
  (labels ((rec (remaining-conflicts unique-map dir-path-length)
             (let* ((file-alias   (mapcar (lambda (x)
                                            (list
                                             x
                                             (file-name x dir-path-length)))
                                          remaining-conflicts))
                    (conflict-set (remove-if (lambda (x)
                                               (not (member-if
                                                     (lambda (y)
                                                       (and (equalp (cadr x) (cadr y))
                                                            (not (equalp x y))))
                                                     file-alias)))
                                             file-alias))
                    (non-conflict (remove-if (lambda (x) (member x conflict-set))
                                             file-alias))
                    (updated-map  (reduce (lambda (map alias-alist)
                                            (fset:with map
                                                       (car  alias-alist)
                                                       (cadr alias-alist)))
                                          non-conflict
                                          :initial-value unique-map)))
               (if (null conflict-set)
                   updated-map
                   (rec (mapcar #'car conflict-set)
                        updated-map
                        (1+ dir-path-length))))))
    (rec file-list
         (fset:empty-map)
         (if all-conflicts 2 1))))

;; -----------------------------------------------------------------------------
;; File Naming Helpers
;; -----------------------------------------------------------------------------

(defun file-name (file &optional (extra-context 1))
  "takes a file and how much extra context is needed to disambiguate the file
Returns a string that reconstructs the unique identifier for the file"
  (let ((name (pathname-name file)))
    ;; directories have no name!
    (cond ((not name)
           (car (last (pathname-directory file))))
          ((= extra-context 1)
           name)
          (t
           (let* ((disambigous-path   (last (pathname-directory file)
                                            (1- extra-context)))
                  (reconstructed-path (butlast (mapcan (lambda (s) (list s "/"))
                                                       (append disambigous-path
                                                               (list name))))))
             (apply #'concatenate 'string reconstructed-path))))))


;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------
(consturct-file-alias-map (append (uiop:directory-files "../../holder/")
                                  (uiop:directory-files "../../holder/Src/")))

(disambiguate-files (list #P"~/Documents/Work/Repo/juvix/holder/Src/Library.hs"
                          #P"~/Documents/Work/Repo/juvix/holder/Library.hs"
                          #P"~Documents/Work/Repo/juvix/holder/Libr.hs")
                    :all-conflicts nil)

(files-and-dirs "../../holder/")
