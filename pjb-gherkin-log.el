;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-gherkin-log.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A simple read-only mode for browsing the OUTPUT (logs) of
;;;;    Gherkin / Cucumber BDD runs, with color coding for `error',
;;;;    `simple', `command' and `build' lines.  For editing the
;;;;    `.feature' source itself, use `feature-mode' instead.
;;;;
;;;;USAGE
;;;;
;;;;    (require 'pjb-gherkin-log)
;;;;
;;;;    ;; Optionally bind to a project-specific log naming convention,
;;;;    ;; e.g. tickets that start with FOO-:
;;;;    (setq pjb-gherkin-log-auto-mode-regexp "/FOO-[^/]*\\.log\\'")
;;;;    (pjb-gherkin-log-register-auto-mode)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-05-24 <PJB> Extracted from per-employer emacs rc files;
;;;;                     the ticket-prefix is now a defcustom and the
;;;;                     default does not register any auto-mode entry.
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

(require 'view)

(defgroup pjb-gherkin-log nil
  "Highlighting of Gherkin / Cucumber log files."
  :group 'tools
  :prefix "pjb-gherkin-log-")

(defcustom pjb-gherkin-log-auto-mode-regexp nil
  "File-name regexp on which to auto-activate `pjb-gherkin-log-mode'.
When nil (the default) no entry is added to `auto-mode-alist'.
Set this to a project-specific pattern (e.g. \"/FOO-[^/]*\\\\.log\\\\'\")
and call `pjb-gherkin-log-register-auto-mode' to enable automatic
activation."
  :type '(choice (const :tag "Disabled" nil) regexp)
  :group 'pjb-gherkin-log)

(defface pjb-gherkin-log-error
  '((default (:foreground "red")))
  "Face for error lines in Gherkin logs."
  :group 'pjb-gherkin-log)

(defface pjb-gherkin-log-simple
  '((default (:foreground "yellow")))
  "Face for simple lines in Gherkin logs."
  :group 'pjb-gherkin-log)

(defface pjb-gherkin-log-command
  '((default (:foreground "blue")))
  "Face for command lines in Gherkin logs."
  :group 'pjb-gherkin-log)

(defface pjb-gherkin-log-build
  '((default (:foreground "grey")))
  "Face for build lines in Gherkin logs."
  :group 'pjb-gherkin-log)

(defvar pjb-gherkin-log-font-lock-keywords
  '(("^\\(error .*\\)"                (1 'pjb-gherkin-log-error))
    ("^\\(build .*error on line.*\\)" (1 'pjb-gherkin-log-error))
    ("^\\(.*\\.[chm]:[0-9]+: error:.*\\)" (1 'pjb-gherkin-log-error))
    ("^\\(simple .*\\)"               (1 'pjb-gherkin-log-simple))
    ("^\\(command .*\\)"              (1 'pjb-gherkin-log-command))
    ("^\\(build .*\\)"                (1 'pjb-gherkin-log-build)))
  "Font-lock keywords for `pjb-gherkin-log-mode'.")

;;;###autoload
(define-derived-mode pjb-gherkin-log-mode view-mode "Gherkin-Log"
  "Major mode for browsing Gherkin / Cucumber run logs."
  (toggle-truncate-lines 1)
  (setq-local font-lock-maximum-size 10000000)
  (setq-local font-lock-keywords nil)
  (font-lock-add-keywords nil pjb-gherkin-log-font-lock-keywords)
  (font-lock-flush)
  (font-lock-ensure))

;;;###autoload
(defun pjb-gherkin-log-register-auto-mode ()
  "Add `pjb-gherkin-log-auto-mode-regexp' to `auto-mode-alist'.
No-op when the variable is nil."
  (interactive)
  (when pjb-gherkin-log-auto-mode-regexp
    (add-to-list 'auto-mode-alist
                 (cons pjb-gherkin-log-auto-mode-regexp
                       'pjb-gherkin-log-mode))))

(provide 'pjb-gherkin-log)
;;;; THE END ;;;;
