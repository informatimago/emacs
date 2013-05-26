;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-xcode.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads a Xcode DVT theme plist and set the font-lock face colors from it.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2013-04-12 <PJB> Created.
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
(require 'pjb-xml)
(require 'color)



(defun plist-object-to-lisp (pobject)
  (when (atom pobject)
    (error "Unexpected plist object %S" object))
  (case (car pobject)
    ((dict)    (loop
                  for (key value) on (cddr pobject) by (function cddr)
                  collect (cons (plist-object-to-lisp key)
                                (plist-object-to-lisp value))))
    ((key)     (intern (format ":%s" (third pobject))))
    ((string)  (third pobject))
    (otherwise (error "Unsupported plist object type %s" (car pobject)))))

(defun plist-to-lisp (plist)
  (assert (and (listp plist) (eq 'plist (first plist))))
  (let ((version (cdr (assoc 'version (second plist)))))
   (unless (string= "1.0" version)
     (error "Unsupported plist version %S" version)))
  (plist-object-to-lisp (third plist)))


(defun parse-color (color)
  "`color' contains the RGBA values as floating point numbers between 0 and 1. Return them in a list."
  (first (read-from-string (format "(%s)" color))))

(assert (equalp (parse-color "0.338151 0.602653 1 1") '(0.338151 0.602653 1 1)))

(defvar *map-xcode-syntax-to-faces*
  '((font-lock-builtin-face                . :xcode.syntax.identifier.function.system)
    (font-lock-comment-delimiter-face      . :xcode.syntax.comment)
    (font-lock-comment-face                . :xcode.syntax.comment)
    (font-lock-constant-face               . :xcode.syntax.identifier.constant)
    (font-lock-doc-face                    . :xcode.syntax.comment.doc)
    (font-lock-function-name-face          . :xcode.syntax.identifier.function)
    (font-lock-keyword-face                . :xcode.syntax.keyword)
    (font-lock-preprocessor-face           . :xcode.syntax.preprocessor)
    (font-lock-string-face                 . :xcode.syntax.string)
    (font-lock-type-face                   . :xcode.syntax.identifier.type)
    (font-lock-variable-name-face          . :xcode.syntax.identifier.variable)
    (default                               . :xcode.syntax.plain)))

(defun load-faces-from-xcode-dvtcolortheme (path)
  (let* ((theme        (plist-to-lisp  (xml-remove-blank-elements (first (xml-parse-file path)))))
         (xcode-colors (assoc :DVTSourceTextSyntaxColors theme)))
    (loop
       for (face . key) in *map-xcode-syntax-to-faces*
       for xcode-color = (cdr (assoc key xcode-colors))
       when xcode-color
       do (let ((color (apply (function color-rgb-to-hex) (subseq (parse-color xcode-color) 0 3))))
            (set-face-foreground face color)))))

(provide 'pjb-xcode)




