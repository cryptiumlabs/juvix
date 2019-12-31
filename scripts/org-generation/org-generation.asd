(asdf:defsystem :org-generation
  :depends-on (:fset :uiop)
  :version "0.0.1"
  :description ""
  :author "Mariari"
  :pathname "src/"
  :components
  ((:file "type-signature")
   (:file "maybe")
   (:file "types"   :depends-on ("maybe" "type-signature"))
   (:file "utility" :depends-on ("type-signature"))
   (:file "haskell" :depends-on ("maybe" "types"))
   (:file "context" :depends-on ("haskell" "types"))
   (:file "lisp"    :depends-on ("types" "maybe" "type-signature"))
   (:file "code-generation" :depends-on ("context" "utility" "types" "type-signature" "maybe")))
  :in-order-to ((asdf:test-op (asdf:test-op :org-generation/test))))

(asdf:defsystem :org-generation/test
  :depends-on (:org-generation :fiveam)
  :description "testing org-generation"
  :pathname "test/"
  :components ((:file "run-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :org-test :run-tests)))
