
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load "./generate.lisp")

(code-generation:generate-org-file #p"../src/" #p"../doc/Code/Juvix.org")
(code-generation:generate-org-file #p"../app/" #p"../doc/Code/App.org")
(code-generation:generate-org-file #p"../test/" #p"../doc/Code/Test.org")
