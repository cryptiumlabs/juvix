;;; Dependencies for YAML generation
;;; ----------------------------------------------------------------------

;; --------------------------------------
;; Crypto Style Dependencies
;; --------------------------------------

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

;; --------------------------------------
;; Tezos Style Dependencies
;; --------------------------------------

(defparameter *tezos-bake-monitor*
  (make-dependency-git :name "https://gitlab.com/obsidian.systems/tezos-bake-monitor-lib.git"
                       :commit "9356f64a6dfc5cf9b108ad84d1f89bcdc1f08174"
                       :subdirs (list "tezos-bake-monitor-lib")))

;; Why do we use such a specific version again
(defparameter *tezos-morley*
  (make-dependency-git :name "https://gitlab.com/morley-framework/morley.git"
                       :commit "53961f48d0d3fb61051fceaa6c9ed6becb7511e5"
                       :subdirs (list "code/morley" "code/morley-prelude")))

;; It seems we were directed to grab these when the system failed to load
(defparameter *morley*
  (string->dep-sha
   "morley-1.14.0@sha256:70a9fc646bae3a85967224c7c42b2e49155461d6124c487bbcc1d825111a189d,9682"))

(defparameter *morley-prelude*
  (string->dep-sha
   "morley-prelude-0.4.0@sha256:7234db1acac9a5554d01bdbf22d63b598c69b9fefaeace0fb6f765bf7bf738d4,2176"))


(defparameter *base-no-prelude-standard*
  (string->dep-sha
   "base-noprelude-4.13.0.0@sha256:3cccbfda38e1422ca5cc436d58858ba51ff9114d2ed87915a6569be11e4e5a90,6842")
  "this is the standard version of base no prelude")

(defparameter *base-no-prelude-special*
  (make-dependency-git :name "https://github.com/serokell/base-noprelude.git"
                       :commit "87df0899801dcdffd08ef7c3efd3c63e67e623c2")
  "this is a special version of base no prelude we have to use with Michelson backend")

;; --------------------------------------
;; Stadnard Library Style Dependencies
;; --------------------------------------

(defparameter *prettiest*
  (make-dependency-github :name "jyp/prettiest"
                          :commit "e5ce6cd6b4da71860c3d97da84bed4a827fa00ef"))


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

(defparameter *sr-extra*
  (make-dependency-github :name "seereason/sr-extra"
                          :commit "d5435dcb2ae5da5f9e0fb8e5a3c40f99937a046f"))

;;; ----------------------------------------------------------------------
;;; Groups for YAML generation
;;; ----------------------------------------------------------------------

;; --------------------------------------
;; Tezos Dependency Groups
;; --------------------------------------

(defparameter *morley-deps*
  (make-groups :comment "Morley Specific dependencies"
               :deps (list
                      *tezos-bake-monitor*)))

(defparameter *morley-sub-deps*
  (make-groups
   :comment "Git depdencies caused by Morley specific dependencies"
   :deps (list
          *morley*
          *morley-prelude*
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

;; --------------------------------------
;; Tezos ∧ Arithmetic Circuit dependcy Groups
;; --------------------------------------


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

;; --------------------------------------
;; LLVM Extra Depenecy groups
;; --------------------------------------

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


;; --------------------------------------
;; Interaction Net Groups Depencenies
;; --------------------------------------

(defparameter *interaction-net-extra-deps*
  (make-groups :comment "For Interaction Nets json-schema"
               :deps (list
                      (make-dependency-github
                       :name "cryptiumlabs/jsonschema-gen"
                       :commit "0639cd166ec59a04d07a3a7d49bdf343e567000e"))))

;; --------------------------------------
;; General Extra Groups
;; --------------------------------------

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


(defparameter *graph-visualizer*
  (make-groups
   :comment "Visualizing graphs"
   :deps (list
          (string->dep-sha "fgl-visualize-0.1.0.1@sha256:e682066053a6e75478a08fd6822dd0143a3b8ea23244bdb01dd389a266447c5e,995"))))

;; -----------------------------------
;; stack-yaml for the YAML helpers
;; -----------------------------------

;; TODO ∷ deprecate this when we have dependencies imply other
;; dependencies we should bring in
(defparameter *standard-library-extra-deps*
  (merge-group
   (make-groups
    :comment "Standard Library Extra Dependency"
    :deps nil)
   *tasty-silver*)
  "Extra dependencies for the standard library")


(defun make-general-dependencies (&rest deps)
  (make-groups :comment "General Dependencies" :deps deps))

(defun big-dep-list (&key (plonk nil))
  "For the packages with lots of dependecies, these tend to be the
common ones to include"
  (list (make-general-dependencies *capability*
                                *prettiest*
                                *extensible*
                                *aeson-options*
                                *un-exceptionalio*
                                *sr-extra*)
        *llvm-hs-extra-deps*
        *withdraw*
        *graph-visualizer*
        *standard-library-extra-deps*
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

;;; ----------------------------------------------------------------------
;;; stack-yaml for the YAML generation
;;; ----------------------------------------------------------------------

(defparameter *standard-library*
  (make-stack-yaml
   :name "StandardLibrary"
   :extra-deps (list (make-general-dependencies *capability* *prettiest*) *standard-library-extra-deps*)))

(defparameter *frontend*
  (make-stack-yaml
   ;; why is this one ahead again!?
   :resolver   17.9
   :name       "Frontend"
   :packages   (list *standard-library*)
   :extra-deps (list (make-general-dependencies *capability* *prettiest*) *standard-library-extra-deps*)))

(defparameter *core*
  (make-stack-yaml
   :name       "Core"
   :packages   (list *standard-library*)
   :extra-deps (list (make-general-dependencies *capability* *extensible* *prettiest*)
                      *standard-library-extra-deps*
                      *eac-solver*)))

(defparameter *translate*
  (make-stack-yaml
   :name "Translate"
   :packages   (list *core* *frontend* *standard-library*)
   :extra-deps (list (make-general-dependencies *capability* *extensible* *prettiest*)
                     *standard-library-extra-deps*
                     *eac-solver*)))

(defparameter *interaction-net*
  (make-stack-yaml
   :name       "InteractionNet"
   :resolver   17.9
   :packages   (list *standard-library* *core*)
   :extra-deps (list (make-general-dependencies *capability* *extensible* *prettiest*)
                     *standard-library-extra-deps*
                     *interaction-net-extra-deps*
                     *graph-visualizer*
                     *eac-solver*)
   :extra "allow-newer: true"))

(defparameter *interaction-net-IR*
  (make-stack-yaml
   :name       "InteractionNetIR"
   :resolver   17.9
   :packages   (list *standard-library* *core*)
   :extra-deps (list (make-general-dependencies *capability* *extensible* *prettiest*)
                     *eac-solver*)))

(defparameter *LLVM*
  (make-stack-yaml
   :name "Backends/LLVM"
   :resolver 17.9
   :path-to-other "../../"
   :packages (list *standard-library* *core* *interaction-net*)
   :extra-deps (list (make-general-dependencies *capability* *extensible* *prettiest*)
                     *llvm-hs-deps*
                     *llvm-hs-extra-deps*
                     *eac-solver*
                     *standard-library-extra-deps*
                     *interaction-net-extra-deps*)
   :extra "allow-newer: true"))

;; Define these before pipeline due to mutual recursion
(defparameter *Pipeline*
  (make-stack-yaml
   :packages (list *standard-library*
                   *frontend*
                   *core*
                   *translate*)
   ;; hack name, for sub dirs
   :name "Pipeline"
   :extra-deps (big-dep-list)
   :extra "allow-newer: true"))

(defparameter *Michelson*
  (make-stack-yaml
   ;; hack name, for sub dirs
   :name "Backends/Michelson"
   :path-to-other "../../"
   :packages      (list *standard-library* *core* *pipeline*
                        ;; this is needed due to pipeline additions
                        ;; have left it unable to build. I think due to cyclic dependencies
                        *translate*
                        *frontend*)
   :extra-deps    (list (make-general-dependencies *capability* *extensible* *prettiest*)
                        *fmt-withdraw*
                        *eac-solver*
                        *morley-arithmetic-circuit-deps*
                        *morley-deps*
                        *morley-sub-deps*
                        *morley-sub-deps-extra*
                        *graph-visualizer*
                        *standard-library-extra-deps*)))

(defparameter *plonk*
  (make-stack-yaml
   :name "Backends/Plonk"
   :path-to-other "../../"
   :packages (list *standard-library*
                   *frontend*
                   *core*
                   *pipeline*
                   *translate*)
   :extra-deps (big-dep-list :plonk t)
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
   (big-dep-list)
   :extra "allow-newer: true"))

