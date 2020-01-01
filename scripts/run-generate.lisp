(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; dependency for new systems without fset
(quicklisp:quickload "fset" :silent t)

(load "./org-generation/org-generation.asd")

(asdf:load-system :org-generation)

(let ((config (org-generation/code-generation:gen-config #p"./org-generation/language-config.lisp")))

  (org-generation/code-generation:generate-org-file config
                                                    #p"../src/"
                                                    #p"../doc/Code/Juvix.org")
  (org-generation/code-generation:generate-org-file config
                                                    #p"../app/"
                                                    #p"../doc/Code/App.org")
  (org-generation/code-generation:generate-org-file config
                                                    #p"../test/"
                                                    #p"../doc/Code/Test.org")

  (org-generation/code-generation:generate-org-file config
                                                    #p"./org-generation/"
                                                    #p"../doc/Code/org-generation.org"))
