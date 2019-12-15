(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load "./org-generation/org-generation.asd")

(asdf:load-system :org-generation)

(org-generation/code-generation:generate-org-file #p"../src/" #p"../doc/Code/Juvix.org")
(org-generation/code-generation:generate-org-file #p"../app/" #p"../doc/Code/App.org")
(org-generation/code-generation:generate-org-file #p"../test/" #p"../doc/Code/Test.org")
