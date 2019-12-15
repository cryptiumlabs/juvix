(defpackage #:org-generation/context
  (:nicknames #:og/context)
  (:use #:cl
        #:org-generation/types
        #:org-generation/maybe
        #:org-generation/type-signature))

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
                              :context (funcall (lookup package 'initialize)
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
  (fset:lookup config 'enabled))

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
  (symbol-value (find-symbol (symbol-name symbol)
                             package)))

(sig lookup (-> package symbol symbol))
(defun lookup (package symbol)
  (find-symbol (symbol-name symbol)
               package))


;; tests

;; (time (initalize-map (parse-list-to-map
;;                       (parse #p"./org-generation/language-config.lisp"))))

;; (time (read-config #p"./org-generation/language-config.lisp"))
