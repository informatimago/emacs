;;;; -*- mode:emacs-lisp;coding:utf-8 -*-

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

#-(and)
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
