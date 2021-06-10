;; -----------------------------------
;; Configuration variables
;; -----------------------------------
(defparameter *default-resolver* 17.3)

;; -----------------------------------
;; General Abstractions Types
;; -----------------------------------
(defstruct stack-yaml
  "this is the main data type of the stack yaml file. We include
relative pathing, so we can properly depend on other stack-yaml
packages."
  (resolver *default-resolver* :type single-float)
  ;; list of pacakges we rely on, which are local dires
  ;; list stack-yaml
  (packages nil :type list)
  ;; list of extra-deps
  ;; list groups
  (extra-deps nil :type list)
  ;; the name of the yaml file
  (name "" :type string)
  ;; needed to know where the other projects are
  ;; by default this is in their sistor directories
  (path-to-other "../" :type string)
  ;; extra is typically used for allow-newer: true
  extra)

(defstruct groups
  "Groups are the main way we group dependencies, often 1 dependency
brings in many more, and so we want to have a comment about what the
list of deps are."
  (comment "" :type string)
  ;; list of dependencies
  ;; list dependency
  (deps '() :type list))

(deftype dependency ()
  "depedency is the dependency sum type consisting of sha | git | bare | github"
  `(or (satisfies dependency-sha-p)
      (satisfies dependency-git-p)
      (satisfies dependency-github-p)
      (satisfies dependency-bare-p)))

(defstruct dependency-sha
  "dependency-sha is for sha based depdencies"
  (name "" :type string)
  (sha  "" :type string))

(defstruct dependency-git
  "git is for git based stack dependencies"
  (name "" :type string)
  commit
  (subdirs nil :type list))

;; todo make the struct inherent depndency-git
(defstruct dependency-github
  "git is for git based stack dependencies"
  (name "" :type string)
  commit
  (subdirs nil :type list))

(defstruct dependency-bare
  "bare dependencies are the rarest and have no corresponding sha
hash"
  (name "" :type string))

;; -----------------------------------
;; Helpers for copmutation
;; -----------------------------------

(defun repeat (n thing)
  "repeats THING N times"
  (loop for i from 0 to (1- n) collect thing))

(defun indent (n string)
  (concatenate 'string (apply #'concatenate 'string (repeat n " "))
               string))

(defun format-list-newline (list)
  (format nil "~{~a~^~%~}" list))

;; taken from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun indent-new-lines-by (number str)
  (replace-all str (format nil "~%") (format nil "~%~a" (indent number ""))))

(defun format-comment (text)
  (let* ((length  (length text))
         ;; 4 due to the amount of extra spaces in the text
         (comment (apply #'concatenate 'string (repeat (+ 4 length) "#"))))
    (format nil "~a~%# ~a #~%~a"
            comment
            text
            comment)))

;; -----------------------------------
;; Operations on the types
;; -----------------------------------

(defun github->git (git)
  (make-dependency-git :name (dependency-github-name git)
                       :commit (dependency-github-commit git)
                       :subdirs (dependency-github-subdirs git)))

(defun string->dep-sha (string)
  "takes a string and maybes produces a sha from it. Returning nil if
it's not a properly formatted string."
  (let ((sha (uiop:split-string string :separator '(#\@))))
    (when (cdr sha)
      (make-dependency-sha :name (car sha) :sha (cadr sha)))))

(defun dep-git->list-string (git &key (github nil))
  "turns a dependecy-git structure into a list of strings"
  (append (list
           (format nil (if github "github: ~a" "git: ~a") (dependency-git-name git)))
          ;; it may not be there
          (when (dependency-git-commit git)
            (list (format nil "commit: ~a" (dependency-git-commit git))))
          ;; finally we have a list of a list here!
          (when (dependency-git-subdirs git)
            (list
             "subdirs:"
             (mapcar (lambda (dep) (format nil "- ~a" dep))
                     (dependency-git-subdirs git))))))


(defun list->string (list)
  "writes a list recursively into a string with a newline, nested
lists are indented by an extra 2 each"
  (format-list-newline
   (mapcar (lambda (str)
             (if (listp str)
                 (indent-new-lines-by 2
                                      (format nil "  ~a" (list->string str)))
                 str))
           list)))

(defun dep-sha->string (sha)
  "generate the yaml sha that is needed in the yaml files"
  (format nil "~a@~a" (dependency-sha-name sha) (dependency-sha-sha sha)))

(defun dep-git->string (git)
  "turns a dependecy-git structure into a string"
  (indent-new-lines-by 2 (list->string (dep-git->list-string git))))

(defun dep-github->string (git)
  "turns a dependecy-git structure into a string"
  (indent-new-lines-by 2 (list->string (dep-git->list-string (github->git git) :github t))))

(defun dep-bare->string (bare)
  "turns a bare dependency structure into a string"
  (format nil "~a" (dependency-bare-name bare)))

(declaim (ftype (function (dependency) (or t string)) dep->string))
(defun dep->string (dep)
  "turns a dependency into a string to be pasted into a YAML file"
  (cond ((dependency-sha-p dep)
         (dep-sha->string dep))
        ((dependency-github-p dep)
         (dep-github->string dep))
        ((dependency-git-p dep)
         (dep-git->string dep))
        (t
         (dep-bare->string dep))))

(defun group->string (group)
  ;; ~{- ~a~^~%~} means format a list with a - infront and a new line
  (format nil "~a~%~{- ~a~^~%~}"
          (format-comment (groups-comment group))
          (mapcar #'dep->string (groups-deps group))))

;; -----------------------------------
;; Operations for stack-yaml->string
;; -----------------------------------

(defun format-packages (stack-yaml)
  "generates the format-packages string"
  (let ((pre-amble (stack-yaml-path-to-other stack-yaml)))
      ;; ~{~a~^~%~} means format a list with new lines
    (format nil "packages:~%~{~a~^~%~}"
            (cons
             "- ."
             (mapcar (lambda (stack-yaml-dep)
                       (format nil "- ~a~a"
                               pre-amble
                               (stack-yaml-name stack-yaml-dep)))
                     (stack-yaml-packages stack-yaml))))))

(defun format-extra-deps (extra-deps)
  ;; ~{~a~^~%~} means format a list with new lines between them
  (when extra-deps
    (format nil "extra-deps:~%~%~{~a~^~%~%~}~%~%"
            (mapcar #'group->string extra-deps))))

(defun format-resolver (resolver)
  (format nil "resolver: lts-~a" resolver))

(defun format-extra (extra)
  (if extra
      (format nil "~%~a" extra)
      ""))

(defun stack-yaml->string (yaml-config)
  (format nil "~a~%~%~a~%~%~a~a"
          (format-resolver (stack-yaml-resolver yaml-config))
          ;; TODO
          (format-packages yaml-config)
          (format-extra-deps (stack-yaml-extra-deps yaml-config))
          (format-extra (stack-yaml-extra yaml-config))))

(defun merge-group (g1 g2)
  "merges 2 groups, taking the comment from the first"
  (make-groups :comment (groups-comment g1)
               :deps (append (groups-deps g1) (groups-deps g2))))

;;; ----------------------------------------------------------------------
;;; Ouptut for YAML generation
;;; ----------------------------------------------------------------------

(defun print-yaml (table)
  (format t (stack-yaml->string table)))

(defun generate-yaml-file (table out-file)
  (with-open-file (stream out-file
                          :direction         :output
                          :if-exists         :supersede
                          :if-does-not-exist :create)
    (format stream (stack-yaml->string table))))
