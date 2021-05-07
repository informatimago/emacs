;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;; emacs <-> common-lisp RPC with slime/swank

;;; In emacs, we can execute Common Lisp expressions:

(require 'slime)

(setf slime-enable-evaluate-in-emacs t)

(defun eval-in-cl (cl-expression-string process-result-values)
  (slime-eval-async
   `(swank:eval-and-grab-output ,(format "(cl:multiple-value-list %s)"
                                        cl-expression-string))
   (lexical-let  ((here (current-buffer)) ; shall we use a marker?
                  (process-result-values process-result-values))
     (lambda (result-values)
       (set-buffer here)
       (funcall process-result-values result-values))))
  nil)

;; (eval-in-cl "(values 1 * (expt 2 20) (package-name *package*))"
;;             (lambda (result) (insert (format ";; %s\n" (second result)))))
;;
;; (eval-in-cl "(progn #+sbcl (values 1 0 :sbcl) #+ccl (values 2 :ccl) #-(or sbcl ccl) 3)"
;;             (lambda (result) (insert (format ";; %s\n" (second result)))))
;;
;; (eval-in-cl "+"
;;             (lambda (result) (insert (format ";; %s\n" (second result)))))


;;;; THE END ;;;;
