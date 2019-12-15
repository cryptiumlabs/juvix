;; configuration file for code generation


(haskell
  :name Juvix)

(lisp
 ;; this makes it use the asd file to check imports instead of loads
 :asdf #p"org-generation.asd")
