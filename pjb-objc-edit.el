;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-objc-edit.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A few utilities to help editing Objective-C++ code with
;;;;    strange style rules.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2012-11-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2012 - 2012
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
(require 'cc-mode)
(require 'semantic)


(defparameter *operators*
  '((infix  . ("+" "-" "*" "/" "^" "&" "|" "&&" "||" "=" "==" ":" "?"))
    (suffix . ("++" "--" "*"))
    (prefix . ("++" "--" "-" "+" "&" "*" "!"))))


(defparameter *no-space*
  '("++" "--" ":"))

(defparameter *pre-space*
  '("+" "-" "*" "/" "^" "&" "|" "&&" "||" "=" "==" "?"
    "!"))

(defparameter *post-space*
  '("+" "-" "*" "/" "^" "&" "|" "&&" "||" "=" "==" "?"
    ","))

(defparameter *post-newline-indent*
  '(";"))


(defun pjb-objc-edit-insert-special-character (n)
  (interactive "P")
  (cond
    ((listp n)    (self-insert-command n))
    ((integerp n) (self-insert-command n))
    (t

     (let ((before (char-before)))
       (cond
	 ((member before '(32 9 10 13 nil))
	  (insert (format "%c" last-command-char)))
	 ((alphanumericp before)
	  (insert (format " %c " last-command-char)))
	 )
       )

     
     )))


(defun pjb-objc-edit-forward-sexp (&optional argument)
  (interactive "P")
  (if (and argument (minusp argument))
      (pjb-objc-edit-backward-sexp (- argument))
      (progn
        (forward-sexp)
        (backward-sexp)
        (if (looking-at "@\\(interface\\|implementation\\|protocol *[^(]\\)\\>")
            (loop repeat (or argument 1)
               do (re-search-forward "^\\s-*@end\\>" nil t))
            (forward-sexp argument)))))


(defun pjb-objc-edit-backward-sexp (&optional argument)
  (interactive "P")
  (if (and argument (minusp argument))
      (pjb-objc-edit-forward-sexp (- argument))
      (let ((from (point)))
        (backward-sexp)
        (if (looking-at "@end\\>")
            (loop repeat (or argument 1)
               do (re-search-backward "@\\(interface\\|implementation\\|protocol *[^(]\\)\\>" nil t)
               finally (goto-char (match-beginning 0)))
            (unless (or (null argument) (= 1 argument))
              (goto-char from)
              (backward-sexp argument))))))




(defun pjb-objc-kill-ring-save-selector ()
  (interactive)
  (cond
    ((looking-at "\\(\\s-\\|\n\\)*\\[")
     (let ((selector  (pjb-objc-selector-name (pjb-objc-message-send-selector  (pjb-objc-parser--parse-message-send)))))
       (kill-new selector)
       (message "Kill-ring'ed %S" selector)))
    ((looking-at "\\(\\s-\\|\n\\)*[-+]")
     (let ((selector  (pjb-objc-selector-name (pjb-objc-method-signature-selector (pjb-objc-parser--parse-method-signature)))))
       (kill-new selector)
       (message "Kill-ring'ed %S" selector)))
    (t
     (up-list)
     (backward-sexp)
     (pjb-objc-kill-ring-save-selector))))


(defun pjb-objc-edit-add-font-lock-keywords ()
  (interactive)
  ;; Try with overlays, not compose-region!
  ;; (font-lock-add-keywords
  ;;  nil
  ;;  '(("^ *# *pragma +mark +- *$"
  ;;     (0 (progn (compose-region (match-beginning 0) (match-end 0)
  ;;                               "/\\-"
  ;;                               'decompose-region)
  ;;               nil)))
  ;;    ("^ *# *pragma +mark +\\([^-].*\\) *$"
  ;;     (0 (progn (compose-region (match-beginning 0) (match-end 0)
  ;;                               "=!"
  ;;                               'decompose-region)
  ;;               nil)))))
  )


(defun pjb-objc-edit-meat ()
  (interactive)
  (local-set-key (kbd "C-M-f") 'pjb-objc-edit-forward-sexp)
  (local-set-key (kbd "C-M-b") 'pjb-objc-edit-backward-sexp)
  (local-set-key (kbd "C-c C-o C-f") 'pjb-objc-edit-forward-sexp)
  (local-set-key (kbd "C-c C-o C-b") 'pjb-objc-edit-backward-sexp)
  (local-set-key (kbd "C-c C-o s")   'pjb-objc-kill-ring-save-selector)
  (local-set-key (kbd "C-c C-o c")   'pjb-objc-ide-find-superclass-file)
  (local-set-key (kbd "C-c C-o u")   'pjb-objc-ide-beginning-of-class)
  (pjb-objc-edit-add-font-lock-keywords))



(defun pjb-objc-edit-convert-snail-to-camel (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "_\\(.\\)" end t)
    (let ((ch (match-string 1)))
      (replace-match (string-upcase ch) t t))))

(provide 'pjb-objc-edit)
;;;; THE END ;;;;
