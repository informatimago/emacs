;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;; emacs <-> common-lisp RPC with slime/swank

;;; In emacs, we can execute Common Lisp expressions:

(require 'slime)

(slime)

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

(eval-in-cl "(values 1 * (expt 2 20) (package-name *package*))"
            (lambda (result) (insert (format ";; %s\n" (second result)))))
;; (1 (1 0 :sbcl) 1048576 "COMMON-LISP-USER")


(eval-in-cl "(progn #+sbcl (values 1 0 :sbcl) #+ccl (values 2 :ccl) #-(or sbcl ccl) 3)"
            (lambda (result) (insert (format ";; %s\n" (second result)))))
;; (1 0 :sbcl)



;;; In Common Lisp, we can execute emacs lisp expressions:

(defparameter *emacs-readtable* (copy-readtable))
(setf (readtable-case *emacs-readtable*) :preserve)
(set-syntax-from-char #\> #\) *emacs-readtable*)
(set-dispatch-macro-character
 #\# #\<
 (lambda (stream subchar dispchar)
   `(emacs-unreadable ,@(read-delimited-list #\> stream t)))
 *emacs-readtable*)

;; Probably more readtable patching would be in order.
;;
;; We could define CLOS proxies for emacs objects for a more seamless
;; integration. swank::eval-in-emacs process the CL form to make it
;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; to emacs lisp forms returning the corresponding emacs object.

(defun eval-in-emacs (form &optional nowait)
  (let ((result (SWANK::EVAL-IN-EMACS `(format "%S" ,form) nowait))
        (*readtable* *emacs-readtable*))
    (with-input-from-string (in result)
      (let ((result (read in nil in)))
        result))))


(eval-in-emacs `(progn
                  (switch-to-buffer (buffer-named "*scratch*"))
                  (goto-char (point-max))
                  (insert ,(format nil "~%Hello~%"))
                  (list 42 (current-buffer))))

;; Switch to the *scratch* buffer,
;; goto the last position, and
;; inserts \nHello\n
;; then returns:
;; (42 (EMACS-UNREADABLE |buffer| |*scratch*|))

