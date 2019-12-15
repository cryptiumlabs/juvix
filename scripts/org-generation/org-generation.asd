(asdf:defsystem :org-generation
  :depends-on (:fset :uiop)
  :version "0.0.1"
  :description ""
  :author "Mariari"
  :pathname "src/"
  :components
  ((:file "type-signature")
   (:file "maybe")
   (:file "types" :depends-on ("maybe"))
   (:file "code-generation" :depends-on ("types" "type-signature" "maybe")))
  :in-order-to ((asdf:test-op (asdf:test-op :org-generation/test))))

(asdf:defsystem :org-generation/test
  :depends-on (:org-generation :fiveam)
  :description "testing org-generation"
  :pathname "test/"
  :components ((:file "run-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :org-test :run-tests)))
