(asdf:defsystem :yaml-generator
  :version "0.1.0.0"
  :description "Provides a Yaml generator"
  :author "Mariari and The Juvix Team"
  :license "GPL3"
  :pathname "src/"
  :components
  ((:file "stack-yaml")
   (:file "config" :depends-on ("stack-yaml"))
   (:file "main"   :depends-on ("config"))))

;; We do this to make the system load for the script
;; (asdf:load-system :yaml-generator)

(defun main ()
  (asdf:load-system :yaml-generator)
  (main))
