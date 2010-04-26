(setq enable-local-eval    nil) ;; ignore eval: in file variables.
(setq byte-compile-verbose t) ;;   to get function name as compiling...

(setq root "/usr")
(setq root "/opt")

(add-to-list 'load-path (concat root "/share/emacs/site-lisp"))
(add-to-list 'load-path (concat root "/local/share/emacs/site-lisp"))
(load (concat root "/local/share/emacs/site-lisp/site-start") nil t)


(when (file-exists-p (concat root "/share/emacs/site-lisp/cedet/eieio/eieio.el"))
  (add-to-list 'load-path (concat root "/share/emacs/site-lisp/cedet/eieio/eieio.el")))

(add-to-list 'load-path (concat root "/local/share/emacs/site-lisp/eshell-2.4.2"))
(add-to-list 'load-path (concat root "/local/share/emacs/site-lisp/w3-4.0pre.47"))
(add-to-list 'load-path (concat root "/local/share/emacs/site-lisp/vm-7.07"))
(add-to-list 'load-path (concat root "/local/share/emacs/site-lisp/emacs-cl"))

(require 'cl)
(load "load-cl")

;; (add-to-list 'load-path "/home/pascal/src/common/common-lisp" )
(add-to-list 'load-path (concat (getenv "HOME") "/src/public/emacs" ))


;;; (defadvice byte-compile-file-form (before verbose-bcff (form) activate)
;;;   (cond
;;;    ((eq 'defun (car form)) 
;;;     (message "Compiling function %s..." (cadr form)))
;;;    ((eq 'defmacro (car form))
;;;     (message "Compiling macro %s..." (cadr form)))
;;;    ))

;;;; .emacs                           --                     --          ;;;;
