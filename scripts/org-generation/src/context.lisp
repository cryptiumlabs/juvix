;;; - This module servers as the abstraction from the first class
;;;   module design of this application
;;;   + This module does this by simply wrapping all module calls in
;;;     proper function calls that dispatch to the inner module table
;;;     that is formed
;;; * Module Hash Table
;;;

(defpackage #:org-generation/context
  (:nicknames #:og/context)
  (:use #:cl
        #:org-generation/types
        #:org-generation/maybe
        #:org-generation/type-signature)
  (:export #:valid-extensions
           #:read-config
           #:alias-map-to-language-import
           #:import-org-aliases
           #:module-comments))

(in-package :org-generation/context)

;; -----------------------------------------------------------------------------
;; stored module type with initialize
;; -----------------------------------------------------------------------------

(defstruct language-context
  (package (error "need a proper package") :type package)
  context)

;; -----------------------------------------------------------------------------
;; Main functions
;; -----------------------------------------------------------------------------
(sig valid-extensions (-> fset:map (or fset:set t)))
(defun valid-extensions (config)
  (fset:reduce (lambda (acc key val)
                 (declare (ignore val))
                 (fset:with acc key))
               config
               :initial-value (fset:empty-set)))

(sig read-config (-> pathname fset:map))
(defun read-config (file)
  (initalize-map (parse-list-to-map (parse file))))


(sig alias-map-to-language-import (-> fset:map fset:map (or fset:map t)))
(defun alias-map-to-language-import (config alias-map)
  "takes an alias map, of path-name's to org-alias, and changes the keys to be
the language specific import"
  (fset:image (lambda (key value)
                (let* ((extension (pathname-type key))
                       (context   (fset:lookup config extension)))
                  (values
                   (if context
                       (uiop:symbol-call (language-context-package context)
                                         'convert-path
                                         key
                                         (language-context-context context))
                       key)
                   value)))
              alias-map))

(sig import-org-aliases (-> fset:map file-info fset:map list (or list t)))
(defun import-org-aliases (config file-info conflict-map lines)
  "calls the import-generation function on the language context, the conflict-map
that has the language import, and the lines of the file"
  (let ((context (fset:lookup config (pathname-type (file-info-path file-info)))))
    (uiop:symbol-call (language-context-package context)
                      'import-generation
                      (language-context-context context)
                      conflict-map
                      (extend-file-info file-info lines))))

(defun module-comments (config file-info lines level)
  (let ((context (fset:lookup config (pathname-type (file-info-path file-info)))))
    (remove-if #'uiop:emptyp
               (mapcar (lambda (x) (og/utility:update-headline-level x level))
                       (uiop:symbol-call (language-context-package context)
                                         'module-comments
                                         (extend-file-info file-info lines))))))
;; -----------------------------------------------------------------------------
;; Config Generation
;; -----------------------------------------------------------------------------

(sig initalize-map (-> fset:map fset:map))
(defun initalize-map (config)
  "Takes the configuration and generates the map of extensions to the valid package
with a fully initialized context"
  (reduce (lambda (map symbol)
            (let ((package (find-package (language-name symbol))))
              (if package
                  (fset:with map
                             (lookup-value package '*extension*)
                             (make-language-context
                              :package package
                              :context (uiop:symbol-call
                                        package
                                        'initialize
                                        (fset:lookup config symbol))))
                  map)))
          (enabled-languages config)
          :initial-value (fset:empty-map)))

(sig language-name (-> symbol symbol))
(defun language-name (symbol)
  "creates the package name if such a module even exists!"
  (intern (concatenate 'string "ORG-GENERATION/" (symbol-name symbol))))

(sig enabled-languages (-> fset:map (or list t)))
(defun enabled-languages (config)
  "looksups the enabled module list in the config map"
  (fset:lookup config :enabled))

;; -----------------------------------------------------------------------------
;; Reading the config file and converting it to a map
;; -----------------------------------------------------------------------------

(sig parse-list-to-map (-> list fset:map))
(defun parse-list-to-map (xs)
  (reduce (lambda (map item)
            (if (listp item)
                (fset:with map (car item) (cdr item))
                map))
          xs
          :initial-value (fset:empty-map)))

;; this is a bit dangerous, as we are taking in raw sexps
(sig parse (-> pathname (or t list)))
(defun parse (file)
  "parses the configuration file"
  (uiop:read-file-forms file))

;; -----------------------------------------------------------------------------
;; Calling semantics
;; -----------------------------------------------------------------------------


(sig lookup-value (-> package symbol t))
(defun lookup-value (package symbol)
  (symbol-value (uiop:find-symbol* symbol package)))

;; tests

;; (time (initalize-map (parse-list-to-map
;;                       (parse #p"./org-generation/language-config.lisp"))))

;; (time (read-config #p"./org-generation/language-config.lisp"))
