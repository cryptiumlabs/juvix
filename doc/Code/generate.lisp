(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "fset")
  (asdf:load-system :uiop))

(defpackage #:code-generation
  (:use #:common-lisp))

(in-package :code-generation)

;; -----------------------------------------------------------------------------
;; Maybe type
;; -----------------------------------------------------------------------------

;; TODO better type it
(defconstant +nothing+ :none)

(defstruct just val)

(defun map-just (f val)
  (if (just-p val)
      (make-just :val (funcall f (just-val val)))
      val))

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
  ;; let it be a maybe file-info
  (file +nothing+ :type maybe)
  ;; a dir consists of either a file-info-p or a org-directory-p
  (dir  nil       :type list)
  (name ""        :type string))

(defstruct file-info
  (path  ""        :type pathname)
  (alias +nothing+ :type maybe))

;; -----------------------------------------------------------------------------
;; Program Constants
;; -----------------------------------------------------------------------------

(defparameter *acceptable-extensions* (fset:set "hs"))

;; -----------------------------------------------------------------------------
;; Main Functionality
;; -----------------------------------------------------------------------------

(defun generate-org-file (directory output-file)
  ;; TODO remove repeat calls to construct-file-alias-map
  (let ((files                (get-directory-info directory))
        (haskell-conflict-map (conflict-map-to-haskell-import
                               (construct-file-alias-map
                                (lose-dir-information
                                 (files-and-dirs directory))))))
    (with-open-file (file output-file
                          :direction         :output
                          :if-exists         :supersede
                          :if-does-not-exist :create)
      (labels ((rec (level dirs)
                 (cond ((null dirs)
                        nil)
                       ((file-info-p (car dirs))
                        (let ((text (generate-headlines (car dirs)
                                                        haskell-conflict-map
                                                        level)))
                          (mapc (lambda (line)
                                  (write-line line file))
                                text)
                          (rec level (cdr dirs))))
                       (t
                        (let ((text (generate-headlines-directory (car dirs)
                                                                 haskell-conflict-map
                                                                 level)))
                          (mapc (lambda (line)
                                  (write-line line file))
                                text)
                          (rec (1+ level) (org-directory-dir (car dirs)))
                          (rec level (cdr dirs)))))))
        (rec 1 files)))))


;; -----------------------------------------------------------------------------
;; Org Generation
;; -----------------------------------------------------------------------------

(defun generate-headlines-directory (dir conflict-map level &optional (name "Juvix"))
  (if (just-p (org-directory-file dir))
      (generate-headlines (just-val (org-directory-file dir)) conflict-map level name)
      (list (concatenate 'string (org-header (org-directory-name dir) level)))))

;; conflict-map-haskell is to just avoid repeat work of constantly
;; converting the conflict-map to a haskell one
(defun generate-headlines (file-info conflict-map level &optional (name "Juvix"))
  (let* ((lines    (uiop:read-file-lines (file-info-path file-info)))
         (comments (module-comments lines))
         (imports  (import-generation
                    (haskell-import-to-org-alias (relevent-imports lines name)
                                                 conflict-map)))
         (headline (org-header
                    (if (just-p (file-info-alias file-info))
                        (concatenate 'string
                                     (pathname-name (file-info-path file-info))
                                     " "
                                     (create-reference
                                      (just-val (file-info-alias file-info))))
                        (pathname-name (file-info-path file-info)))
                    level)))
    (cons headline
          (append comments imports))))

(defun import-generation (org-aliases &optional (indent-level 0))
  "generates a list where each line is a new line with proper org generation"
  (when org-aliases
    (cons (ident-cycle (under-score "Relies on") indent-level)
          (mapcar (lambda (alias)
                    (ident-cycle (text-reference alias) (1+ indent-level)))
                  org-aliases))))

;; -----------------------------------------------------------------------------
;; Org Formatting
;; -----------------------------------------------------------------------------

(defun org-header (text level)
  (concatenate 'string (repeat-s level "*") " " text))

(defun ident-spacing (ident-level)
  (repeat-s (* 2 ident-level) " "))

(defun ident-symbol (ident-level)
  (case (mod ident-level 3)
    (0 "-")
    (1 "+")
    (2 "*")))

;; TODO replace a lot of concatenates with formats!

(defun under-score (text)
  (concatenate 'string "_" text "_"))

(defun ident-numbers (text ident-level num)
  (concatenate 'string
               (ident-spacing ident-level)
               (write-to-string num)
               " "
               text))
(defun ident-cycle (text ident-level)
  (concatenate 'string
               (ident-spacing ident-level)
               (ident-symbol ident-level)
               " "
               text))


(defun create-reference (alias)
  (concatenate 'string "<<" alias ">>"))

(defun text-reference (alias)
  (concatenate 'string "[[" alias "]]"))

;; -----------------------------------------------------------------------------
;; Imports to Org Aliases
;; -----------------------------------------------------------------------------


(defun haskell-import-to-org-alias (imports conflict-map)
  "takes a list of Haskell imports and transforms them into their org-mode alias"
  (mapcar (lambda (import)
            (or (fset:lookup conflict-map import)
                (car (last (uiop:split-string import :separator ".")))))
          imports))

;; TODO parameterize this over the project name and language!
;; TODO consider if we should add a lifted version of this, adding to file info
;; - _Advantages_
;;   + Modular
;;   + Multiple passes
;; - _Disadvantages_
;;   + extra allocation
;;   + doesn't stream the data into org file generation
(defun relevent-imports (lines project-name)
  "takes LINES of a source code, and a PROJECT-NAME and filters for imports
that match the project name"
  (let* ((imports     (remove-if (complement (lambda (x)
                                               (uiop:string-prefix-p "import" x)))
                                 lines))
         (modules     (mapcar (lambda (import)
                                ;; TODO make this cadr logic generic
                                (cadr (uiop:split-string import)))
                              imports))
         (project-imp (remove-if (complement (lambda (m)
                                               (uiop:string-prefix-p project-name m)))
                                 modules)))
    project-imp))


;; -----------------------------------------------------------------------------
;; Haskell comment clean up
;; -----------------------------------------------------------------------------

(defun module-comments (file-lines)
  (let* ((module-comments
           (remove-if (lambda (x)
                        (or (uiop:string-prefix-p "{-#" x) (equal "" x)))
            (take-until (lambda (x) (uiop:string-prefix-p "module" x)) file-lines)))
         (special (car module-comments))
         (valid-module (if special
                           (uiop:string-prefix-p "-- |" special)
                           nil)))
    (if valid-module
        (remove-if #'uiop:emptyp
                   (cons (strip-haskell-comments special t)
                         (mapcar #'strip-haskell-comments (cdr module-comments))))
        nil)))

(defun strip-haskell-comments (line &optional start-doc)
  (subseq line (min (if start-doc 5 3) (length line))))


;; -----------------------------------------------------------------------------
;; Conflict Map and Haskell Import Conversion
;; -----------------------------------------------------------------------------


(defun conflict-map-to-haskell-import (conflict-map)
  (fset:image (lambda (key value)
                (values (convert-path key) value))
              conflict-map))

;; TODO Parameterize this much better
(defun convert-path (file &optional (dir-before-library "src"))
  (labels ((last-dir-before (xs)
             (let ((next (member dir-before-library xs :test #'equal)))
               (if next
                   (last-dir-before (cdr next))
                   xs))))
    (reconstruct-path (append
                       (last-dir-before (pathname-directory file))
                       (list (pathname-name file)))
                      ".")))


;; -----------------------------------------------------------------------------
;; Getting Directory and File lists
;; -----------------------------------------------------------------------------

(defun get-directory-info (directory)
  (let* ((annote-1     (files-and-dirs directory))
         (conflict-map (construct-file-alias-map (lose-dir-information annote-1))))
    (alias-file-info annote-1 conflict-map)))

(defun files-and-dirs (directory &optional (valid-extensions *acceptable-extensions*))
  "recursively grabs the file and directories
forming a list of org-directory and file info"
  (let* ((sub-dirs       (uiop:subdirectories  directory))
         (files          (remove-if (complement (lambda (x)
                                                  (fset:@ valid-extensions
                                                          (pathname-type x))))
                                    (uiop:directory-files directory)))
         (dirs-annotated (mapcar (lambda (dir)
                                   (let* ((name  (file-name dir))
                                          (file? (find-if
                                                  (lambda (x)
                                                    (equal (file-name x) name))
                                                  files)))
                                     (make-org-directory
                                      :file (if file?
                                                (make-just
                                                 :val (make-file-info :path file?))
                                                +nothing+)
                                      :dir  (files-and-dirs dir)
                                      :name name)))
                                 sub-dirs))
         ;; slow version of set-difference that maintains ordering
         (files-annotated
           (mapcar (lambda (file)
                     (make-file-info :path file))
            (remove-if (lambda (file)
                         (member-if (lambda (dir-ann)
                                      (if (nothing? (org-directory-file dir-ann))
                                          nil
                                          (equal file
                                                 (file-info-path
                                                  (just-val
                                                   (org-directory-file dir-ann))))))
                                    dirs-annotated))
                       files))))
    (append files-annotated dirs-annotated)))

(defun lose-dir-information (file-dir-list)
  "forgets the directory information and puts all files into a flat list"
  (mapcan (lambda (file-dir)
            (cond ((and (org-directory-p file-dir)
                        (just-p (org-directory-file file-dir)))
                   (cons (file-info-path
                          (just-val
                           (org-directory-file file-dir)))
                         (lose-dir-information (org-directory-dir file-dir))))
                  ((org-directory-p file-dir)
                   (lose-dir-information (org-directory-dir file-dir)))
                  (t
                   (list (file-info-path file-dir)))))
          file-dir-list))

(defun mapcar-file-dir (path-f alias-f file-dirs)
  "Applies path-f to all files in a list and alias-f to all alias and files
   to a list of file-info and org-directory"
  (flet ((alias-call (file-info)
           (make-file-info
            :path  (funcall path-f  (file-info-path file-info))
            :alias (funcall alias-f
                            (file-info-alias file-info)
                            (file-info-path file-info)))))
    (mapcar (lambda (file-dir)
              (if (org-directory-p file-dir)
                  (make-org-directory
                   :file (map-just #'alias-call
                                   (org-directory-file file-dir))
                   :dir (mapcar-file-dir path-f
                                         alias-f
                                         (org-directory-dir file-dir))
                   :name (org-directory-name file-dir))
                  (alias-call file-dir)))
            file-dirs)))

(defun alias-file-info (file-dirs alias-map)
  (mapcar-file-dir #'identity
                   (lambda (alias path)
                     (declare (ignore alias))
                     (let ((lookup (fset:lookup alias-map path)))
                       (if lookup
                           (make-just :val lookup)
                           +nothing+)))
                   file-dirs))

;; -----------------------------------------------------------------------------
;; Handling Conflicting Files
;; -----------------------------------------------------------------------------

(defun construct-file-alias-map (files)
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
                                                       (and (equal (cadr x) (cadr y))
                                                            (not (equal x y))))
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
           (let ((disambigous-path (last (pathname-directory file)
                                         (1- extra-context))))
             (reconstruct-path (append disambigous-path (list name))))))))

(defun reconstruct-path (xs &optional (connector "/"))
  (apply #'concatenate
         'string
         (butlast (mapcan (lambda (s) (list s connector))
                          xs))))
;; -----------------------------------------------------------------------------
;; Helpers Utility
;; -----------------------------------------------------------------------------

(defun repeat (n thing)
  "repeats THING N times"
  (loop for i from 0 to (1- n) collect thing))

(defun repeat-s (n thing)
  (apply #'concatenate 'string (repeat n thing)))

(defun take-until (pred xs)
  (labels ((rec (current acc)
             (if (or (null current) (funcall pred (car current)))
                 (reverse acc)
                 (rec (cdr current) (cons (car current) acc)))))
    (rec xs nil)))

;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------
;; (consturct-file-alias-map (append (uiop:directory-files "../../holder/")
;;                                   (uiop:directory-files "../../holder/Src/")))

;; (disambiguate-files (list #P"~/Documents/Work/Repo/juvix/holder/Src/Library.hs"
;;                           #P"~/Documents/Work/Repo/juvix/holder/Library.hs"
;;                           #P"~Documents/Work/Repo/juvix/holder/Libr.hs")
;;                     :all-conflicts nil)

;; (get-directory-info "../../holder/")

;; (files-and-dirs "../../src/")

;; (construct-file-alias-map
;;  (lose-dir-information
;;   (files-and-dirs "../../holder/src/")))

;; (haskell-import-to-org-alias (list "Juvix.Library.PrettyPrint"
;;                                    "Juvix.Interpreter.InteractionNet.Backends.Graph")
;;                              (conflict-map-to-haskell-import
;;                               (construct-file-alias-map
;;                                (lose-dir-information
;;                                 (files-and-dirs "../../src/")))))

;; (relevent-imports (uiop:read-file-lines
;;                    #P"~/Documents/Work/Repo/juvix/holder/src/Library.hs")
;;                   "Juvix")


;; (import-generation
;;  (haskell-import-to-org-alias (list "Juvix.Library.PrettyPrint"
;;                                     "Juvix.Interpreter.InteractionNet.Backends.Graph"
;;                                     "Juvix.Interpreter.InteractionNet.Nets.Default")
;;                               (conflict-map-to-haskell-import
;;                                (construct-file-alias-map
;;                                 (lose-dir-information
;;                                  (files-and-dirs "../../src/"))))))

;; (module-comments (uiop:read-file-lines
;;                   #P"~/Documents/Work/Repo/juvix/holder/src/Library.hs"))

;; (generate-org-file "../../holder/src/" "Test.org")
