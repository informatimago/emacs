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
(require 'semantics)


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


(defun poe-insert-special-character (n)
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




(provide 'pjb-objc-edit)
;;;; THE END ;;;;
