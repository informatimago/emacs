;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-describe.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    describe
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2023-08-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2023 - 2023
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


(defun key-sequence-p (object)
  nil)

(defun input-method-p (object)
  (and (stringp object)
       (assoc object input-method-alist)
       t))

(defvar *description-map*
  '((string key-sequence-p describe-bindings)
    (char-table t describe-buffer-case-table)
    (buffer t describe-categories)
    ;; (integer t describe-char)

    (symbol charsetp        describe-character-set)
    (symbol coding-system-p describe-coding-system)
    ;; (symbol t describe-copying)
    ;; (symbol t describe-distribution)
    ;; (symbol t describe-gnu-project)
    (symbol coding-system-p describe-current-coding-system)
    (symbol coding-system-p describe-current-coding-system-briefly)
    (char-table t describe-current-display-table)
    (symbol facep describe-face)
    (symbol fontp describe-font)
    (symbol fontp  describe-fontset)
    ((or symbol function) functionp describe-function)
    (string input-method-p describe-input-method)
    (string t describe-key-briefly)
    (string t describe-language-environment)

    (symbol t describe-minor-mode)
    (symbol t describe-minor-mode-from-indicator)
    (symbol t describe-minor-mode-from-symbol)
    (symbol t describe-mode)
    (symbol t describe-mode-local-bindings)
    (symbol t describe-mode-local-bindings-in-mode)
    (symbol t describe-no-warranty)
    (symbol t describe-package)
    (symbol t describe-prefix-bindings)
    (symbol t describe-project)
    (symbol t describe-syntax)
    (symbol t describe-text-properties)
    (symbol t describe-theme)
    (symbol t describe-variable)))

(defun describe (what)
  (interactive "xWhat: ")
  (save-excursion
   (get-buffer-create "*describe*")
   (loop for (type predicate describe-function)
           in *description-map*
         when (and (typep what type)
                   (if (eq predicate 't)
                       t
                       (funcall predicate what)))
           do (ignore-errors
               (insert "\n"
                       (save-excursion
                        (funcall describe-function what)
                        (with-current-buffer "*Help*"
                          (buffer-substring (point-min) (point-max))))
                       "\n"))))
  (switch-to-buffer (get-buffer-create "*describe*")))


(provide 'pjb-describe)
