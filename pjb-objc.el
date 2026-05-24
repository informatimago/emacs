;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-objc.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Refactoring helpers for Objective-C source.  Currently:
;;;;
;;;;      `pjb-objc-c-comment-block-to-line'
;;;;          Turn a `/* ... */' block comment into a sequence of
;;;;          `// ...' line comments.
;;;;
;;;;      `pjb-objc-signatures-to-methods-with-log'
;;;;          Expand a region of bare method signatures into method
;;;;          bodies that wrap their original doc comment and add an
;;;;          `NSLog(...)' entry-trace guarded by `#ifdef DEBUG'.
;;;;          Uses `pjb-objc-parser--parse-method-signature'.
;;;;
;;;;USAGE
;;;;
;;;;    (require 'pjb-objc)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-05-24 <PJB> Extracted from per-employer emacs rc files.
;;;;BUGS
;;;;
;;;;    `pjb-objc-signatures-to-methods-with-log' depends on a
;;;;    `pjb-objc-parser' module providing
;;;;    `pjb-objc-parser--parse-method-signature' and the accessors
;;;;    `pjb-objc-method-signature-parameters',
;;;;    `pjb-objc-method-signature-selector',
;;;;    `pjb-objc-selector-components' and
;;;;    `pjb-objc-parameter-name'.  That parser is not yet bundled
;;;;    with this file.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2026
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

(require 'cl-lib)

(defun pjb-objc--looking-at-c-comment ()
  "Return non-nil if point is at whitespace followed by a C comment."
  (looking-at "[ \n\t]*/[*/]"))

(defun pjb-objc-c-comment-block-to-line (comment)
  "Convert COMMENT, a string containing a `/* ... */' block comment,
into a sequence of `// ...' line comments.  Strings that are not
recognized as block comments are returned unchanged."
  (if (and (>= (length comment) 4)
           (string= "/*" (substring comment 0 2))
           (string= "*/" (substring comment (- (length comment) 2))))
      (concat "// "
              (mapconcat #'identity
                         (split-string (substring comment 2 (- (length comment) 2)) "\n")
                         "\n// "))
      comment))

(defvar pjb-objc-default-method-with-log-tag ""
  "Default tag string prepended to the NSLog message in
`pjb-objc-signatures-to-methods-with-log'.")

(defun pjb-objc-signatures-to-methods-with-log (start _end &optional tag)
  "In the region START.._END, expand each Objective-C method signature
into a method body that logs its entry (guarded by `#ifdef DEBUG').
A preceding C block comment is converted to line comments and kept
inside the method body.  TAG defaults to
`pjb-objc-default-method-with-log-tag'.  The end of the region is
ignored: expansion stops at the first unparsable signature."
  (interactive "r")
  (goto-char start)
  (let ((tag (or tag pjb-objc-default-method-with-log-tag)))
    (cl-loop
     (let (comment signature)
       (if (pjb-objc--looking-at-c-comment)
           (progn
             (forward-comment 1)
             (let ((endc (point)))
               (forward-comment -1)
               (let ((startc (point)))
                 (goto-char endc)
                 (unless (setf signature
                               (ignore-errors
                                 (pjb-objc-parser--parse-method-signature)))
                   (cl-return))
                 (setf comment
                       (pjb-objc-c-comment-block-to-line
                        (prog1 (buffer-substring startc endc)
                          (delete-region startc endc)))))))
         (unless (setf signature
                       (ignore-errors
                         (pjb-objc-parser--parse-method-signature)))
           (cl-return)))
       (when (looking-at " *;")
         (delete-region (match-beginning 0) (match-end 0)))
       (insert "{\n")
       (when comment (insert comment "\n"))
       (insert "#ifdef DEBUG\n")
       (insert (format "NSLog(@\"%s" tag))
       (cl-loop with parameters = (pjb-objc-method-signature-parameters signature)
                for component in (pjb-objc-selector-components
                                  (pjb-objc-method-signature-selector signature))
                for parameter = (pop parameters)
                do (insert (prin1-to-string component))
                   (when parameter (insert " %@ "))
                when parameter
                  collect (pjb-objc-parameter-name parameter) into arguments
                finally (insert "\"")
                        (when arguments
                          (insert ","
                                  (mapconcat #'prin1-to-string arguments ","))))
       (insert ");\n")
       (insert "#endif\n")
       (insert "}\n")
       (let ((block-end (point)))
         (backward-sexp)
         (indent-region (point) block-end)
         (forward-sexp))))))

(provide 'pjb-objc)
;;;; THE END ;;;;
