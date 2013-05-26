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



(defstruct (pjb-objc-selector
             (:constructor pjb-objc-selector (components)))
  components)


(defun pjb-objc-selector-name (selector)
  (mapconcat (function symbol-name) (pjb-objc-selector-components selector) ""))



(defstruct pjb-objc-position
  start
  end)


(defstruct (pjb-objc-parameter
             (:include pjb-objc-position)
             (:constructor pjb-objc-parameter))
  name
  type)


(defstruct (pjb-objc-method-signature
             (:include pjb-objc-position)
             (:constructor pjb-objc-method-signature))
  object
  result-type
  selector
  parameters)


(defstruct (pjb-objc-expression
             (:include pjb-objc-position)
             (:constructor pjb-objc-expression))
  operator
  arguments)


(defstruct (pjb-objc-message-send
             (:include pjb-objc-position)
             (:constructor pjb-objc-message-send))
  receiver
  selector
  arguments)






(defun pjb-objc-parser--eat-sexp ()
  "PRIVATE: Return the next Sexp, and move the point past it."
  (c-forward-comments)
  (prog1 (progn
           (forward-sexp)
           (backward-sexp)
           (sexp-at-point))
    (forward-sexp)))


(defun pjb-objc-parser--eat-looked ()
  "PRIVATE: Move the point to (match-end 0)."
  (goto-char (match-end 0)))


(defun pjb-objc-parser--parse-parameter ()
  "PRIVATE: parse a method parameter:
parameter ::= '(' type ')' identifier .
"
  (c-forward-comments)
  (let ((start     (point))
        (type      (pjb-objc-parser--eat-sexp))
        (parameter (progn (c-forward-comments)
                          (pjb-objc-parser--eat-sexp)))
        (end       (point)))
    (pjb-objc-parameter :start start :end end :name parameter :type type)))


(defun pjb-objc-parser--looking-at-semicolon ()
  (looking-at "\\(\\s-\\|\n\\)*;"))

(defun pjb-objc-parser--looking-at-block ()
  (looking-at "\\(\\s-\\|\n\\)*{"))

(defun pjb-objc-parser--looking-at-comment ()
  (looking-at "\\(\\s-\\|\n\\)*\\(//\\|/\\*\\)"))


(defun pjb-objc-parser--parse-method-signature ()
  "Parse a method signature:
signature ::= '+'|'-'  '(' type ')' signature-selector .
signature-selector ::= colon-less-selector
                     | selector-part-colon parameter
                          { opt-selector-part-colon parameter }
                          [ opt-selector-part-colon '...' ] .
colon-less-selector ::= identifier .
selector-part-colon ::= identifier ':' .
opt-selector-part-colon ::= [ identifier ] ':' .
"
  (c-forward-comments)
  (unless (looking-at "\\(\\s-\\|\n\\)*[-+]")
    (error "Expected token (-|+) not found."))
  (let ((start (point))
        (method-object  (prog1
                            (if (looking-at "\\(\\s-\\|\n\\)*-")
                                :instance
                                :class)
                          (pjb-objc-parser--eat-looked)))
        (result-type (progn (c-forward-comments)
                            (pjb-objc-parser--eat-sexp)))
        (selector    '())
        (parameters  '()))
    (let ((item (progn (c-forward-comments)
                       (pjb-objc-parser--eat-sexp))))
      (if (looking-at ":")
          (loop
             do
               (pjb-objc-parser--eat-looked)
               (push (intern (format "%s:" item)) selector)
               (push (pjb-objc-parser--parse-parameter) parameters)
               (c-forward-comments)
               (cond
                 ((looking-at "\\(\\s-\\|\n\\)*:")
                  (setf item ""))
                 ((looking-at "\\(\\s-\\|\n\\)*,")
                  (pjb-objc-parser--eat-looked)
                  (if (looking-at "\\(\\s-\\|\n\\)*\\.\\.\\.")
                      (progn
                        (pjb-objc-parser--eat-looked)
                        (push (pjb-objc-parameter :name '...) parameters)
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
     :start start
     :end (point)
     :object method-object
     :result-type result-type
     :selector (pjb-objc-selector (reverse selector))
     :parameters (reverse parameters))))




(defun pjb-objc-parser--parse-expression ()
  (c-forward-comments)
  (let ((start      (point))
        (expression (pjb-objc-parser--eat-sexp)))
    (if (eql '@ expression)
        (let ((operator '@)
              (expression (pjb-objc-parser--eat-sexp)))
          (pjb-objc-expression :start start :end (point) :operator operator  :arguments (list expression))) 
        (pjb-objc-expression :start start :end (point) :operator 'identity :arguments (list expression)))))


(defun pjb-objc-parser--parse-message-send ()
  "Parse a message send:
message-send ::= '[' receiver message ']' .
receiver ::= expression .
message ::= colon-less-selector
          | selector-part-colon argument
            { opt-selector-part-colon argument }
            [ opt-selector-part-colon argument { ',' argument } ] .
argument ::= expression .
colon-less-selector ::= identifier .
selector-part-colon ::= identifier ':' .
opt-selector-part-colon ::= [ identifier ] ':' .
"
  (c-forward-comments)
  (unless (looking-at "\\(\\s-\\|\n\\)*\\[")
    (error "Expected token '[' not found."))
  (let ((start     (point))
        (receiver  (progn (pjb-objc-parser--eat-looked)
                          (pjb-objc-parser--parse-expression)))
        (selector  '())
        (arguments '()))
    (let ((item (progn (c-forward-comments)
                       (pjb-objc-parser--eat-sexp))))
      (if (looking-at ":")
          (loop
             do
               (pjb-objc-parser--eat-looked)
               (push (intern (format "%s:" item)) selector)
               (push (pjb-objc-parser--parse-expression) arguments)
               (c-forward-comments)
               (cond
                 ((looking-at "\\(\\s-\\|\n\\)*:")
                  (setf item ""))
                 ((looking-at "\\(\\s-\\|\n\\)*,")
                  (pjb-objc-parser--eat-looked)
                  (push (pjb-objc-parser--parse-expression) arguments))
                 ((looking-at "\\(\\s-\\|\n\\)*]")
                  (pjb-objc-parser--eat-looked)
                  (return))
                 (t
                  (setf item (pjb-objc-parser--eat-sexp))
                  (unless (looking-at ":")
                    (error "Found invalid syntax")))))
          (push item selector)))
    (pjb-objc-message-send
     :start start
     :end (point)
     :receiver receiver
     :selector (pjb-objc-selector (reverse selector))
     :arguments (reverse arguments))))



(provide 'pjb-objc-parser)

