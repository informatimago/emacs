;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-nxml.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Editing helpers for `nxml-mode'.  Note: this module is about
;;;;    EDITING XML buffers; `pjb-xml.el' is about manipulating the
;;;;    parsed XML data structures.
;;;;
;;;;USAGE
;;;;
;;;;    (require 'pjb-nxml)
;;;;
;;;;    ;; Also installs a hideshow entry for `nxml-mode' that uses
;;;;    ;; `pjb-nxml-forward-element' to step over balanced elements.
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

(require 'nxml-mode)
(require 'hideshow)
(require 'outline)

(defun pjb-nxml-forward-element ()
  "Move forward over one balanced XML element or comment.
Skips comments (`<!-- ... -->') as opaque blocks, and refuses to
move across outline section headings (as defined by `outline-regexp')."
  (let ((nxml-sexp-element-flag (not (looking-at "<!--"))))
    (unless (looking-at outline-regexp)
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))

;;; Install the hideshow entry for `nxml-mode' (and, for convenience,
;;; one for `lua-mode' which uses similar block-delimiter rules).
;;; Existing entries for these modes are replaced.

(with-eval-after-load 'hideshow
  (setq hs-special-modes-alist
        (append
         '((lua-mode
            "\\([[{(]\\|\\<\\(do\\|\\(?:functio\\|the\\)n\\)\\>\\)"
            "\\([]})]\\|\\<\\(end\\)\\>\\)"
            nil
            lua-forward-sexp
            nil)
           (nxml-mode
            "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
            ""
            "<!--" ;; won't work on its own; uses syntax table
            (lambda (_arg) (pjb-nxml-forward-element))
            nil))
         (cl-remove-if (lambda (entry)
                         (memq (car entry) '(lua-mode nxml-mode)))
                       hs-special-modes-alist))))

(provide 'pjb-nxml)
;;;; THE END ;;;;
