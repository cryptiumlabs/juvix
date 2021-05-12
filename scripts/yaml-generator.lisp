
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
  (path-to-other "../" :type string))

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

(defun stack-yaml->string (yaml-config)
  (format nil "~a~%~%~a~%~%~a"
          (format-resolver (stack-yaml-resolver yaml-config))
          ;; TODO
          (format-packages yaml-config)
          (format-extra-deps (stack-yaml-extra-deps yaml-config))))

;; -----------------------------------
;; Dependencies for YAML generation
;; -----------------------------------
(defparameter *galois-field*
  (make-dependency-git :name   "https://github.com/serokell/galois-field.git"
                       :commit "576ba98ec947370835a1f308895037c7aa7f8b71"))

(defparameter *elliptic-curve*
  (make-dependency-git :name   "https://github.com/serokell/elliptic-curve.git"
                       :commit "b8a3d0cf8f7bacfed77dc3b697f5d08bd33396a8"))

(defparameter *pairing*
  (make-dependency-git :name   "https://github.com/serokell/pairing.git"
                       :commit "cf86cf1f6b03f478a439703b050c520a9d455353"))

(defparameter *tezos-bake-monitor*
  (make-dependency-git :name "https://gitlab.com/obsidian.systems/tezos-bake-monitor-lib.git"
                       :commit "9356f64a6dfc5cf9b108ad84d1f89bcdc1f08174"
                       :subdirs (list "tezos-bake-monitor-lib")))

(defparameter *capability*
  (string->dep-sha "capability-0.4.0.0@sha256:d86d85a1691ef0165c77c47ea72eac75c99d21fb82947efe8b2f758991cf1837,3345"))

(defparameter *extensible*
  (make-dependency-github :name "metastatedev/extensible-data"
                          :commit "d11dee6006169cb537e95af28c3541a24194dea8"))

;; -----------------------------------
;; Groups for YAML generation
;; -----------------------------------

(defparameter *eac-solver*
  (make-groups :comment "For the EAC Solver"
               :deps (list
                      (make-dependency-github :name "cwgoes/haskell-z3"
                                              :commit "889597234bcdf5620c5a69d3405ab4d607ba4d71"))))

(defparameter *morley-arithmetic-circuit-deps*
  (make-groups :comment "Morley and Arithmetic Circuits deps"
               :deps (list
                      *galois-field*
                      *elliptic-curve*
                      *pairing*)))

(defparameter *sub-morley-arithmetic-circuit-deps*
  (make-groups :comment "Sub dependencies of arithmetic-circuit git"
               :deps (list
                      (string->dep-sha
                       "constraints-extras-0.3.0.2@sha256:bf6884be65958e9188ae3c9e5547abfd6d201df021bff8a4704c2c4fe1e1ae5b,1784")
                      (string->dep-sha
                       "dependent-sum-0.7.1.0@sha256:5599aa89637db434431b1dd3fa7c34bc3d565ee44f0519bfbc877be1927c2531,2068")
                      (string->dep-sha
                       "dependent-sum-template-0.1.0.3@sha256:0bbbacdfbd3abf2a15aaf0cf2c27e5bdd159b519441fec39e1e6f2f54424adde,1682")
                      (string->dep-sha
                       "hashing-0.1.0.1@sha256:98861f16791946cdf28e3c7a6ee9ac8b72d546d6e33c569c7087ef18253294e7,2816")
                      (string->dep-sha
                       "monoidal-containers-0.6.0.1@sha256:7d776942659eb4d70d8b8da5d734396374a6eda8b4622df9e61e26b24e9c8e40,2501"))))

(defparameter *morley-deps*
  (make-groups :comment "Morley Specific dependencies"
               :deps (list
                      *tezos-bake-monitor*)))


;; -----------------------------------
;; stack-yaml for the YAML generation
;; -----------------------------------

(defparameter *standard-library*
  (make-stack-yaml
   :extra-deps (list (make-groups :comment "General Dependencies"
                                  :deps (list *capability*)))
   :name "StandardLibrary"))

(defparameter *frontend*
  (make-stack-yaml
   ;; why is this one ahead again!?
   :resolver 17.9
   :packages (list *standard-library*)
   :extra-deps (list (make-groups :comment "General Dependencies"
                                  :deps (list *capability*)))
   :name "Frontend"))

(defparameter *core*
  (make-stack-yaml
   :name     "Core"
   :packages (list *standard-library*)
   :extra-deps (list (make-groups :comment "General Dependencies"
                                  :deps (list *capability* *extensible*))
                     *eac-solver*)))

(defparameter *Michelson*
  (make-stack-yaml
   :packages (list *standard-library*)
   :name "Michelson"))


;; -----------------------------------
;; Ouptut for YAML generation
;; -----------------------------------

(defun print-yaml (table)
  (format t (stack-yaml->string table)))

(defun generate-yaml-file (table out-file)
  (with-open-file (stream out-file
                          :direction         :output
                          :if-exists         :supersede
                          :if-does-not-exist :create)
    (format stream (stack-yaml->string table))))

(defun print-standard-library ()
  (print-yaml *standard-library*))

(defun generate-standard-library (out-file)
  (generate-yaml-file *standard-library* out-file))

(defun generate-translate-yaml ()
  (format t (format-packages *michelson*))
  (format t (group->string *morley-deps*)))

(defun main ()
  (generate-yaml-file *standard-library* "library/StandardLibrary/stack.yaml")
  (generate-yaml-file *frontend* "library/Frontend/stack.yaml")
  (generate-yaml-file *core* "library/Core/stack.yaml"))
