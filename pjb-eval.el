;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-eval.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Provides an eval-last-expression command that can work for Common Lisp,
;;;;    emacs lisp and scheme.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-10-18 <PJB> Extracted from pjb-erc.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(require 'cl)

(defun eval-last-expression--multi-line-values-p ()
  (case major-mode
    ((emacs-lisp-mode lisp-mode slime-repl-mode scheme-mode)
     t)
    (otherwise
     nil)))

(defun cl-eval-last-expression ()
  (interactive)
  (let ((current-prefix-arg t)
        (expression (slime-last-expression)))
    (push-mark)
    (slime-eval-async `(swank:eval-and-grab-output-and-error ,expression)
      (lambda (result)
        (destructuring-bind (output values) result
          (if (eval-last-expression--multi-line-values-p)
              (progn
                (when (plusp (length output))
                  (insert "\n;; prints:\n"
                          (replace-regexp-in-string "\n" ";; " output t t)))
                (if (plusp (length values))
                  (insert "\n;; --> "
                          (replace-regexp-in-string "\n" " ;\n;;     " values t t)
                          "\n")
                  (insert "\n;; No value\n")))
            (progn
              (insert (if (zerop (length output)) " #|" " #| ")
                      output)
              (when (plusp (length values))
                (insert
                 " --> "
                 (replace-regexp-in-string "\n" " ; " values t t)))
              (insert " |# "))))))))

(defun el-eval-last-expression ()
  (interactive)
  (let ((pt (point))
        (current-prefix-arg t))
    (set-mark pt)
    (eval-last-sexp t)
    (save-excursion
      (goto-char pt)
      (if (eval-last-expression--multi-line-values-p)
          (insert "\n;; --> ")
        (insert " --> ")))))

(defun scheme-eval-last-expression ()
  (interactive)
  (let ((pt (point))
        (current-prefix-arg t))
    (set-mark pt)
    (lisp-eval-last-sexp t)
    (save-excursion
      (goto-char pt)
      (if (eval-last-expression--multi-line-values-p)
          (insert "\n;; --> ")
        (insert " --> ")))))

(defun eval-last-expression ()
  (interactive)
  (case major-mode
    ((emacs-lisp-mode)
     (el-eval-last-expression))
    ((lisp-mode slime-repl-mode)
     (cl-eval-last-expression))
    ((scheme-mode)
     (scheme-eval-last-expression))
    ((erc-mode)
     (loop with current-channel = (buffer-name)
        for (channels . eval-function)
        in '((("#emacs" "#emacsfr"  "#haskell-emacs" "irc.libera.chat:6697")
              . el-eval-last-expression)
             (("#scheme")
              . scheme-eval-last-expression)
             (t
              . cl-eval-last-expression))
        when (or (eql channels t)
                 (member* current-channel channels :test (function string=)))
        do (funcall eval-function)))
    (t
     (eval-last-sexp t))))

(global-set-key (kbd "C-x C-e") 'eval-last-expression)

(provide 'pjb-eval)
;;;; THE END ;;;;
