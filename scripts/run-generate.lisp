
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load "./generate.lisp")

(code-generation:generate-org-file "../src/" #p"../doc/Code/Juvix.org")
(code-generation:generate-org-file "../app/" #p"../doc/Code/App.org")
(code-generation:generate-org-file "../test/" #p"../doc/Code/Test.org")
