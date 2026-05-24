;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-valgrind.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A simple derived mode for browsing valgrind log files, with
;;;;    syntax highlighting for the PID prefix, suppression blocks and
;;;;    stack-frame lines.
;;;;
;;;;    By default the mode is activated on files ending in `.valgrind'
;;;;    or `.helgrind'.
;;;;
;;;;USAGE
;;;;
;;;;    (require 'pjb-valgrind)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-05-24 <PJB> Extracted from per-employer emacs rc files.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2026
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

(require 'compile)

(defvar pjb-valgrind-font-lock-keywords
  '(("^{[^}]*}"
     (0 font-lock-preprocessor-face))
    ("^\\(==[0-9]*==\\) \\([^ \n].*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-warning-face))
    ("^\\(==[0-9]*==\\)   \\([^ \n].*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-comment-face))
    ("^\\(==[0-9]*==\\)    [a-z]+ \\(0x[0-9A-F]*\\): \\([^( \n]*\\)(\\(.*\\)) (\\([^:]+:[0-9]+\\))$"
     (1 font-lock-variable-name-face)
     (2 font-lock-constant-face)
     (3 font-lock-function-name-face)
     (4 font-lock-type-face)
     (5 font-lock-comment-face)))
  "Font-lock keywords for `pjb-valgrind-mode'.")

;;;###autoload
(define-derived-mode pjb-valgrind-mode compilation-mode "Valgrind"
  "Major mode for reading valgrind log files."
  (setq-local font-lock-keywords nil)
  (font-lock-add-keywords nil pjb-valgrind-font-lock-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(val\\|hel\\)grind\\'" . pjb-valgrind-mode))

(provide 'pjb-valgrind)
;;;; THE END ;;;;
