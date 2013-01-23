;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-objc-parser.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Partial Parsers for Objective-C code.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2013-01-21 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2013 - 2013
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


(defstruct (pjb-objc-parameter
             (:constructor pjb-objc-parameter))
  name
  type)


(defstruct (pjb-objc-method-signature
             (:constructor pjb-objc-method-signature))
  object
  result-type
  selector
  parameters)


(defstruct (pjb-objc-selector
             (:constructor pjb-objc-selector (components)))
  components)



(defun pjb-objc-parser--eat-sexp ()
  (prog1 (progn
           (forward-sexp)
           (backward-sexp)
           (sexp-at-point))
    (forward-sexp)))


(defun pjb-objc-parser--eat-looked ()
  (goto-char (match-end 0)))


(defun pjb-objc-parser--parse-parameter ()
  (let ((type (pjb-objc-parser--eat-sexp))
        (parameter (pjb-objc-parser--eat-sexp)))
    (pjb-objc-parameter :name parameter :type type)))


(defun pjb-objc-parser--parse-method-signature ()
  (unless (looking-at "\\(\\s-\\|\n\\)*[-+]")
    (error "Expected token (-|+) not found."))
  (let ((method-object  (prog1
                            (if (looking-at "\\(\\s-\\|\n\\)*-")
                                :instance
                                :class)
                          (pjb-objc-parser--eat-looked)))
        (result-type (pjb-objc-parser--eat-sexp))
        selector arguments)
    (let ((item (pjb-objc-parser--eat-sexp)))
      (if (looking-at ":")
          (loop
             do
               (pjb-objc-parser--eat-looked)
               (push (intern (format "%s:" item)) selector)
               (push (pjb-objc-parser--parse-parameter) arguments)
               (cond
                 ((looking-at "\\(\\s-\\|\n\\)*:")
                  (setf item ""))
                 ((looking-at "\\(\\s-\\|\n\\)*,")
                  (pjb-objc-parser--eat-looked)
                  (if (looking-at "\\(\\s-\\|\n\\)*\\.\\.\\.")
                      (progn
                        (pjb-objc-parser--eat-looked)
                        (push (pjb-objc-parameter :name '...) arguments)
                        (return))
                      (error "Found invalid token")))
                 ((looking-at "\\(\\s-\\|\n\\)*\\(;\\|{\\|//\\|/\\*\\)")
                  (return))
                 (t
                  (setf item (pjb-objc-parser--eat-sexp))
                  (unless (looking-at ":")
                    (error "Found invalid syntax")))))
          (push item selector)))
    (pjb-objc-method-signature
     :object method-object
     :result-type result-type
     :selector (pjb-objc-selector (reverse selector))
     :parameters (reverse arguments))))



(provide 'pjb-objc-parser)

