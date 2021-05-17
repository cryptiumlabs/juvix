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

;; -----------------------------------
;; Dependencies for YAML generation
;; -----------------------------------
(defparameter *galois-field*
  (make-dependency-git :name   "https://github.com/serokell/galois-field.git"
                       :commit "576ba98ec947370835a1f308895037c7aa7f8b71"))

(defparameter *galois-field-plonk*
  (make-dependency-github :name "adjoint-io/galois-field"
                          :commit "3b13705fe26ea1dc03e1a6d7dac4089085c5362d")
  "Plonk uses a special version of this library")

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

(defparameter *fmt*
  (string->dep-sha
   "fmt-0.6.1.2@sha256:405a1bfc0ba0fd99f6eb1ee71f100045223f79204f961593012f28fd99cd1237,5319"))

(defparameter *aeson-options*
  (string->dep-sha
   "aeson-options-0.1.0@sha256:2d0c25afbb2d038bd5b57de8d042e319ea1a5ec7d7b92810d8a0cf0777882b6a,1244"))

(defparameter *un-exceptionalio*
  (string->dep-sha
   "unexceptionalio-0.5.0@sha256:ad0b2d4d1f62a3e24cdb80360eea42ab3f0a0559af42aba19b5cf373378913ce,1682"))

(defparameter *base-no-prelude-standard*
  (string->dep-sha
   "base-noprelude-4.13.0.0@sha256:3cccbfda38e1422ca5cc436d58858ba51ff9114d2ed87915a6569be11e4e5a90,6842")
  "this is the standard version of base no prelude")

(defparameter *base-no-prelude-special*
  (make-dependency-git :name "https://github.com/serokell/base-noprelude.git"
                       :commit "87df0899801dcdffd08ef7c3efd3c63e67e623c2")
  "this is a special version of base no prelude we have to use with Michelson backend")

(defparameter *sr-extra*
  (make-dependency-github :name "seereason/sr-extra"
                          :commit "d5435dcb2ae5da5f9e0fb8e5a3c40f99937a046f"))

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


(defparameter *withdraw*
  (make-groups :comment "Witherable"
               :deps (list
                      (string->dep-sha
                       "witherable-0.3.5@sha256:6590a15735b50ac14dcc138d4265ff1585d5f3e9d3047d5ebc5abf4cd5f50084,1476")
                      (string->dep-sha
                       "witherable-class-0@sha256:91f05518f9f4af5b02424f13ee7dcdab5d6618e01346aa2f388a72ff93e2e501,775"))))

(defparameter *fmt-withdraw*
  (merge-group (make-groups :comment "Fmt witherable" :deps (list *fmt*))
               *withdraw*))

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
          (make-dependency-bare :name "hex-text-0.1.0.0")
          (make-dependency-bare :name "show-type-0.1.1")
          (string->dep-sha
           "named-0.3.0.1@sha256:2975d50c9c5d88095026ffc1303d2d9be52e5f588a8f8bcb7003a04b79f10a06,2312")
          (make-dependency-bare :name "cryptonite-0.27")
          (make-dependency-bare :name "uncaught-exception-0.1.0")
          (make-dependency-bare :name "tasty-hunit-compat-0.2.0.1")
          (string->dep-sha
           "with-utf8-1.0.2.2@sha256:42eed140390b3e93d9482b084d1d0150e8774667f39c33bd47e84815751fad09,3057")))
  "this is generic, and used in a few places")

(defparameter *morley-sub-deps-extra*
  (make-groups
   :comment "Git depdencies caused by Morley specific dependencies that are speicific to Michelson"
   :deps (list
          (make-dependency-git :name "https://github.com/int-index/caps.git"
                               :commit "c5d61837eb358989b581ed82b1e79158c4823b1b")
          *base-no-prelude-special*))
  "like *morley-sub-deps* but is an extra layer of dependency that is not used elsewhere")

(defparameter *morley-deps-testing*
  (make-dependency-git :name "https://gitlab.com/morley-framework/morley.git"
                       :commit "53961f48d0d3fb61051fceaa6c9ed6becb7511e5"
                       :subdirs (list "code/morley" "code/morley-prelude")))

(defparameter *morley-arithmetic-circuit-deps*
  (make-groups :comment "Shared Deps Between Arithmetic Circuits and Morley"
               :deps (list
                      *elliptic-curve*
                      *pairing*
                      *galois-field*)))

(defparameter *morley-arithmetic-circuit-deps-plonk*
  (make-groups :comment "Shared Deps Between Arithmetic Circuits and Morley For Plonk"
               :deps (list
                      *elliptic-curve*
                      *pairing*
                      *galois-field-plonk*)))

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
  (make-groups :comment "For Interaction Nets json-schema"
               :deps (list
                      (make-dependency-github
                       :name "cryptiumlabs/jsonschema-gen"
                       :commit "0639cd166ec59a04d07a3a7d49bdf343e567000e"))))

(defparameter *graph-visualizer*
  (make-groups
   :comment "Visualizing graphs"
   :deps (list
          (string->dep-sha "fgl-visualize-0.1.0.1@sha256:e682066053a6e75478a08fd6822dd0143a3b8ea23244bdb01dd389a266447c5e,995"))))

;; -----------------------------------
;; stack-yaml for the YAML generation
;; -----------------------------------

(defun big-dep-list (&key (plonk nil))
  "For the packages with lots of dependecies, these tend to be the
common ones to include"
  (list (make-general-depencies *capability*
                                *extensible*
                                *aeson-options*
                                *un-exceptionalio*
                                *sr-extra*)
        *llvm-hs-extra-deps*
        *withdraw*
        *graph-visualizer*
        *tasty-silver*
        *morley-sub-deps*
        (make-groups
         :comment "For special deps that are similar to Michelson but not quite the same"
         :deps (list *base-no-prelude-standard*))
        *interaction-net-extra-deps*
        ;; no morley plonk given as plonk wants a different one
        (if plonk
            *morley-arithmetic-circuit-deps-plonk*
            *morley-arithmetic-circuit-deps*)
        *sub-morley-arithmetic-circuit-deps*))

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
                        *morley-sub-deps*
                        *morley-sub-deps-extra*)))

(defparameter *interaction-net*
  (make-stack-yaml
   :name       "InteractionNet"
   :resolver   17.9
   :packages   (list *standard-library* *core*)
   :extra-deps (list (make-general-depencies *capability* *extensible*)
                     *interaction-net-extra-deps*
                     *graph-visualizer*
                     *eac-solver*)
   :extra "allow-newer: true"))

(defparameter *interaction-net-IR*
  (make-stack-yaml
   :name       "InteractionNetIR"
   :resolver   17.9
   :packages   (list *standard-library* *core*)
   :extra-deps (list (make-general-depencies *capability* *extensible*)
                     *eac-solver*)))

(defparameter *LLVM*
  (make-stack-yaml
   :name "Backends/LLVM"
   :resolver 17.9
   :path-to-other "../../"
   :packages (list *standard-library* *core* *interaction-net*)
   :extra-deps (list (make-general-depencies *capability* *extensible*)
                     *llvm-hs-deps*
                     *llvm-hs-extra-deps*
                     *eac-solver*
                     *interaction-net-extra-deps*)
   :extra "allow-newer: true"))

(defparameter *Pipeline*
  (make-stack-yaml
   :packages (list *standard-library*
                   *frontend*
                   *core*
                   *translate*
                   *michelson*
                   *plonk*)
   ;; hack name, for sub dirs
   :name "Pipeline"
   :extra-deps (big-dep-list)
   :extra "allow-newer: true"))

(defparameter *Easy-Pipeline*
  (make-stack-yaml
   :packages (list *standard-library*
                   *frontend*
                   *core*
                   *translate*
                   *michelson*
                   *pipeline*)
   ;; hack name, for sub dirs
   :name "EasyPipeline"
   :extra-deps (big-dep-list)
   :extra "allow-newer: true"))


(defparameter *plonk*
  (make-stack-yaml
   :name "Backends/Plonk"
   :path-to-other "../../"
   :packages (list *standard-library*
                   *frontend*
                   *core*
                   *pipeline*
                   *translate*
                   *michelson*)
   :extra-deps (big-dep-list :plonk t)
   :extra "allow-newer: true"))

(defparameter *juvix*
  (make-stack-yaml
   :name "Juvix"
   :packages (list *standard-library*
                   *frontend*
                   *core*
                   *pipeline*
                   *translate*
                   *michelson*
                   *easy-pipeline*
                   *plonk*)
   :path-to-other "./library/"
   :extra-deps
   (append (big-dep-list)
           (list (make-groups :comment "Michelson Morely Testing library"
                              :deps (list *morley-deps-testing*))))
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
  (generate-yaml-file *standard-library*   "library/StandardLibrary/stack.yaml")
  (generate-yaml-file *frontend*           "library/Frontend/stack.yaml")
  (generate-yaml-file *core*               "library/Core/stack.yaml")
  (generate-yaml-file *translate*          "library/Translate/stack.yaml")
  (generate-yaml-file *Michelson*          "library/Backends/Michelson/stack.yaml")
  (generate-yaml-file *LLVM*               "library/Backends/LLVM/stack.yaml")
  (generate-yaml-file *plonk*              "library/Backends/Plonk/stack.yaml")
  (generate-yaml-file *easy-pipeline*      "library/EasyPipeline/stack.yaml")
  (generate-yaml-file *Pipeline*           "library/Pipeline/stack.yaml")
  (generate-yaml-file *interaction-net*    "library/InteractionNet/stack.yaml")
  (generate-yaml-file *interaction-net-IR* "library/InteractionNetIR/stack.yaml")
  (generate-yaml-file *juvix*              "stack.yaml"))
