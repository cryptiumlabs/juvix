
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

(defparameter *tezos-morley*
  (make-dependency-git :name "https://gitlab.com/morley-framework/morley.git"
                       :commit "53961f48d0d3fb61051fceaa6c9ed6becb7511e5"
                       :subdirs (list "code/morley" "code/morley-prelude")))


(defparameter *capability*
  (string->dep-sha "capability-0.4.0.0@sha256:d86d85a1691ef0165c77c47ea72eac75c99d21fb82947efe8b2f758991cf1837,3345"))

(defparameter *extensible*
  (make-dependency-github :name "metastatedev/extensible-data"
                          :commit "d11dee6006169cb537e95af28c3541a24194dea8"))

(defparameter *tasty*
  (string->dep-sha "tasty-1.4.1@sha256:69e90e965543faf0fc2c8e486d6c1d8cf81fd108e2c4541234c41490f392f94f,2638"))

;; -----------------------------------
;; Groups for YAML generation
;; -----------------------------------

(defparameter *eac-solver*
  (make-groups :comment "For the EAC Solver"
               :deps (list
                      (make-dependency-github
                       :name "cwgoes/haskell-z3"
                       :commit "889597234bcdf5620c5a69d3405ab4d607ba4d71"))))

(defparameter *tasty-silver*
  (make-groups :comment "Testing with tasty silver"
               :deps (list
                      (make-dependency-github
                       :name "phile314/tasty-silver"
                       :commit "f1f90ac3113cd445e2a7ade43ebb29f0db38ab9b")
                      *tasty*)))

(defparameter *fmt-withdraw*
  (make-groups :comment "Fmt witherable"
               :deps (list
                      (string->dep-sha
                       "fmt-0.6.1.2@sha256:405a1bfc0ba0fd99f6eb1ee71f100045223f79204f961593012f28fd99cd1237,5319")
                      (string->dep-sha
                       "witherable-0.3.5@sha256:6590a15735b50ac14dcc138d4265ff1585d5f3e9d3047d5ebc5abf4cd5f50084,1476")
                      (string->dep-sha
                       "witherable-class-0@sha256:91f05518f9f4af5b02424f13ee7dcdab5d6618e01346aa2f388a72ff93e2e501,775"))))

(defparameter *morley-deps*
  (make-groups :comment "Morley Specific dependencies"
               :deps (list
                      *tezos-bake-monitor*
                      *tezos-morley*)))

(defparameter *morley-sub-deps*
  (make-groups
   :comment "Git depdencies caused by Morley specific dependencies"
   :deps (list
          (make-dependency-bare :name "base58-bytestring-0.1.0")
          (make-dependency-git :name "https://github.com/serokell/base-noprelude.git"
                               :commit "87df0899801dcdffd08ef7c3efd3c63e67e623c2")
          (make-dependency-bare :name "hex-text-0.1.0.0")
          (make-dependency-bare :name "show-type-0.1.1")
          (make-dependency-git :name "https://github.com/int-index/caps.git"
                               :commit "c5d61837eb358989b581ed82b1e79158c4823b1b")
          (string->dep-sha
           "named-0.3.0.1@sha256:2975d50c9c5d88095026ffc1303d2d9be52e5f588a8f8bcb7003a04b79f10a06,2312")
          (make-dependency-bare :name "cryptonite-0.27")
          (make-dependency-bare :name "uncaught-exception-0.1.0")
          (make-dependency-bare :name "tasty-hunit-compat-0.2.0.1")
          (string->dep-sha
           "with-utf8-1.0.2.2@sha256:42eed140390b3e93d9482b084d1d0150e8774667f39c33bd47e84815751fad09,3057"))))

(defparameter *morley-arithmetic-circuit-deps*
  (make-groups :comment "Shared Deps Between Arithmetic Circuits and Morley"
               :deps (list
                      *elliptic-curve*
                      *pairing*
                      *galois-field*)))

(defparameter *sub-morley-arithmetic-circuit-deps*
  (make-groups
   :comment "Sub dependencies of arithmetic-circuit git"
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


(defparameter *llvm-hs-deps*
  (make-groups :comment "LLVM-HS Library dependencies"
               :deps (list
                      (make-dependency-bare :name "llvm-hs-9.0.1")
                      (make-dependency-bare :name "llvm-hs-pure-9.0.0")
                      (string->dep-sha
                       "llvm-hs-pretty-0.6.2.0@sha256:4c600122965e8dff586bdca0044ec2b1896f2875c2da5ad89bbab9799c9697cd,1670"))))

(defparameter *llvm-hs-extra-deps*
  (make-groups :comment "LLVM-HS Library Extra dependencies"
               :deps (list
                      (string->dep-sha
                       "derive-storable-0.2.0.0@sha256:a5bb3fb8feb76e95c713f3a5e401f86b7f622dd598e747cad4324b33933f27e1,2422")
                      (string->dep-sha
                       "derive-storable-plugin-0.2.3.0@sha256:11adeef08d4595cfdfefa2432f6251ba5786ecc2bf0488d36b74e3b3e5ca9ba9,2817"))))


(defparameter *interaction-net-extra-deps*
  (make-groups :comment "For Interaction Nets"
               :deps (list
                      (make-dependency-github
                       :name "cryptiumlabs/jsonschema-gen"
                       :commit "0639cd166ec59a04d07a3a7d49bdf343e567000e"))))


;; -----------------------------------
;; stack-yaml for the YAML generation
;; -----------------------------------

(defun make-general-depencies (&rest deps)
  (make-groups :comment "General Dependencies" :deps deps))

(defparameter *standard-library*
  (make-stack-yaml
   :name "StandardLibrary"
   :extra-deps (list (make-general-depencies *capability*))))

(defparameter *frontend*
  (make-stack-yaml
   ;; why is this one ahead again!?
   :resolver   17.9
   :name       "Frontend"
   :packages   (list *standard-library*)
   :extra-deps (list (make-general-depencies *capability*))))

(defparameter *core*
  (make-stack-yaml
   :name       "Core"
   :packages   (list *standard-library*)
   :extra-deps (list (make-general-depencies *capability* *extensible*)
                     *eac-solver*)))

(defparameter *translate*
  (make-stack-yaml
   :name "Translate"
   :packages   (list *core* *frontend* *standard-library*)
   :extra-deps (list (make-general-depencies *capability* *extensible*)
                     *tasty-silver*
                     *eac-solver*)))

(defparameter *Pipeline*
  (make-stack-yaml
   :packages (list *standard-library*)
   ;; hack name, for sub dirs
   :name "Pipeline"
   :path-to-other "../"))

(defparameter *Michelson*
  (make-stack-yaml
   ;; hack name, for sub dirs
   :name "Backends/Michelson"
   :path-to-other "../../"
   :packages      (list *standard-library* *core* *pipeline*)
   :extra-deps    (list (make-general-depencies *capability* *extensible*)
                        *fmt-withdraw*
                        *eac-solver*
                        *morley-arithmetic-circuit-deps*
                        *morley-deps*
                        *morley-sub-deps*)))

(defparameter *interaction-net*
  (make-stack-yaml
   :name "InteractionNet"))

(defparameter *LLVM*
  (make-stack-yaml
   :resolver 17.9
   :name "Backends/LLVM"
   :path-to-other "../../"
   :packages (list *standard-library* *core* *interaction-net*)
   :extra-deps (list (make-general-depencies *capability* *extensible*)
                     *llvm-hs-deps*
                     *llvm-hs-extra-deps*
                     *eac-solver*
                     *interaction-net-extra-deps*)
   :extra "allow-newer: true"))


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
  (generate-yaml-file *frontend*         "library/Frontend/stack.yaml")
  (generate-yaml-file *core*             "library/Core/stack.yaml")
  (generate-yaml-file *translate*        "library/Translate/stack.yaml")
  (generate-yaml-file *Michelson*        "library/Backends/Michelson/stack.yaml")
  (generate-yaml-file *LLVM*             "library/Backends/LLVM/stack.yaml"))
