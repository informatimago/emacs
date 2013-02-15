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

(defparameter pjb-objc-edit--*c++-operators*
  '((1 :left-to-right                     ;  highest
     (2 :infix  "::"                    "Scope resolution (C++ only)"))
    (2 :left-to-right
     (1 :suffix "++"                    "Suffix increment")
     (1 :suffix "--"                    "Suffix decrement")
     (2 :infix  "()"                    "Function call")
     (2 :infix  "[]"                    "Array subscripting")
     (2 :infix  "."                     "Element selection by reference")
     (2 :infix  "->"                    "Element selection through pointer")
     (1 :prefix "typeid()"              "Run-time type information (C++ only) (see typeid)")
     (2 :prefix "const_cast"            "Type cast (C++ only) (see const cast)")
     (2 :prefix "dynamic_cast"          "Type cast (C++ only) (see dynamic_cast)")
     (2 :prefix "reinterpret_cast"      "Type cast (C++ only) (see reinterpret cast)")
     (2 :prefix "static_cast"           "Type cast (C++ only) (see static cast)"))
    (3 :right-to-left
     (1 :prefix "++"                    "Prefix increment" )
     (1 :prefix "--"                    "Prefix decrement")
     (1 :prefix "+"                     "Unary plus")
     (1 :prefix "-"                     "Unary minus")
     (1 :prefix "!"                     "Logical NOT")
     (1 :prefix "~"                     "Bitwise NOT")
     (2 :infix  "(type)"                "Type cast")
     (1 :prefix "*"                     "Indirection (dereference)")
     (1 :prefix "&"                     "Address-of")
     (1 :prefix "sizeof"                "Size-of")
     (1 :prefix "new"                   "Dynamic memory allocation (C++ only)")
     (2 :infix  "new[]"                 "Dynamic memory allocation (C++ only)")
     (1 :prefix "delete, delete[]"      "Dynamic memory deallocation (C++ only)"))
    (4 :left-to-right
     (2 :infix  ".*"                    "Pointer to member (C++ only)")
     (2 :infix  "->*"                   "Pointer to member (C++ only)"))
    (5 :left-to-right
     (2 :infix  "*"                     "Multiplication")
     (2 :infix  "/"                     "Division")
     (2 :infix  "%"                     "Modulo (remainder)"))
    (6 :left-to-right
     (2 :infix  "+"                     "Addition")
     (2 :infix  "-"                     "Subtraction"))
    (7 :left-to-right
     (2 :infix  "<<"                    "Bitwise left shift")
     (2 :infix  ">>"                    "Bitwise right shift"))
    (8 :left-to-right
     (2 :infix  "<"                     "Less than")
     (2 :infix  "<="                    "Less than or equal to")
     (2 :infix  ">"                     "Greater than")
     (2 :infix  ">="                    "Greater than or equal to"))
    (9 :left-to-right
     (2 :infix  "=="                    "Equal to")
     (2 :infix  "!="                    "Not equal to"))
    (10 :left-to-right
     (2 :infix  "&"                    "Bitwise AND"))
    (11 :left-to-right
     (2 :infix  "^"                    "Bitwise XOR (exclusive or)"))
    (12 :left-to-right
     (2 :infix  "|"                    "Bitwise OR (inclusive or)"))
    (13 :left-to-right
     (2 :infix  "&&"                   "Logical AND"))
    (14 :left-to-right
     (2 :infix  "||"                   "Logical OR"))
    (15 :right-to-left
     (3 :infix "?:"                    "Ternary conditional (see ?:)" )
     (2 :infix  "="                    "Direct assignment")
     (2 :infix  "+="                   "Assignment by sum")
     (2 :infix  "-="                   "Assignment by difference")
     (2 :infix  "*="                   "Assignment by product")
     (2 :infix  "/="                   "Assignment by quotient")
     (2 :infix  "%="                   "Assignment by remainder")
     (2 :infix  "<<="                  "Assignment by bitwise left shift")
     (2 :infix  ">>="                  "Assignment by bitwise right shift")
     (2 :infix  "&="                   "Assignment by bitwise AND")
     (2 :infix  "^="                   "Assignment by bitwise XOR")
     (2 :infix  "|="                   "Assignment by bitwise OR"))
    (16 :right-to-left
     (1 :prefix "throw"                "Throw operator (exceptions throwing, C++ only)"))
    (17 :left-to-right
     (2 :infix  ","                    "Comma")))
  "A list of operators grouped by priority level, highest priority first.
Each group is a list (level associativity . operators).
Each operator is a list: (arity position operator description).
associativity is (member :left-to-right :right-to-left)
position is (member :prefix :infix :suffix)
")


(defun pjb-objc-edit--special-character-operators ()
  "Return a list of operators made of special characters only."
  (loop
     with ops = '()
     for (level associativity . operators) in pjb-objc-edit--*c++-operators*
     do (loop for (arity position operator description) in operators
           do (when (notany (function alphanumericp) operator)
                (pushnew (list operator position) ops :test (function equalp))))
     finally (return (sort* ops (function >) :key (lambda (x) (length (first x)))))))


;; (pjb-objc-edit--special-character-operators)
;; 
;; ((">>=" :infix) ("<<=" :infix) ("->*" :infix) ("|=" :infix) ("^=" :infix)
;;  ("&=" :infix) ("%=" :infix) ("/=" :infix) ("*=" :infix) ("-=" :infix)
;;  ("+=" :infix) ("?:" :infix) ("||" :infix) ("&&" :infix) ("!=" :infix)
;;  ("==" :infix) (">=" :infix) ("<=" :infix) (">>" :infix) ("<<" :infix)
;;  (".*" :infix) ("--" :prefix) ("++" :prefix) ("->" :infix) ("[]" :infix)
;;  ("()" :infix) ("--" :suffix) ("++" :suffix) ("::" :infix) ("," :infix)
;;  ("=" :infix) ("|" :infix) ("^" :infix) ("&" :infix) (">" :infix)
;;  ("<" :infix) ("-" :infix) ("+" :infix) ("%" :infix) ("/" :infix)
;;  ("*" :infix) ("&" :prefix) ("*" :prefix) ("~" :prefix) ("!" :prefix)
;;  ("-" :prefix) ("+" :prefix) ("." :infix))

;; (">>=" "<<=" "->*" "|=" "^=" "&=" "%=" "/=" "*=" "-=" "+=" "?:" "||"
;;  "&&" "!=" "==" ">=" "<=" ">>" "<<" ".*" "->" "[]" "()" "--" "++" "::"
;;  "," "=" "|" "^" ">" "<" "%" "/" "&" "*" "~" "!" "-" "+" ".")



(defparameter *spaces-around* '(">>=" "<<=" "->*" "|=" "^=" "&=" "%="
                                "/=" "*=" "-=" "+=" "||" "&&" "!="
                                "==" ">=" "<=" ">>" "<<" "=" "|" "^"
                                "&" ">" "<" "-" "+" "%" "/" "*"))
(defparameter *space-after*  '(","))
(defparameter *space-before* '())

(defparameter *newline-around* '("{" "}"))
(defparameter *newline-after*  '(";"))
(defparameter *newline-before* '())

(defun pjb-objc-edit-insert-spaces-around-operators (start end)
  (interactive "r")
  (with-marker (end end)
    (goto-char start)
    (let ((re (let* ((opchars (remove-duplicates (mapconcat (function identity) *spaces-around* "")))
                     (re (mapconcat (function regexp-quote) *spaces-around* "\\|")))
                (format "\\([^ %s]\\(%s\\)[^ %s]\\)\\|\\([^ %s]\\(%s\\) \\)\\|\\( \\(%s\\)[^ %s]\\)"
                        opchars re opchars
                        opchars re
                        re opchars))))
      (while (re-search-forward re end t nil)
        (cond
          ((match-beginning 1)
           (goto-char (match-end 2))
           (insert " ")
           (goto-char (match-beginning 2))
           (insert " ")
           (goto-char (match-end 1)))
          ((match-beginning 3)
           (goto-char (match-beginning 4))
           (insert " ")
           (goto-char (match-end 3)))
          ((match-beginning 5)
           (goto-char (match-end 6))
           (insert " ")))))
    (goto-char start)
    (let ((re (format " *\\(%s\\) *" (mapconcat (function regexp-quote) *space-after* "\\|"))))
      (while (re-search-forward re end t nil)
        (replace-match (format "%s " (match-string 1)))))))





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

(defun pjb-objc-edit-convert-camel-to-snail (start end)
  (interactive "r")
  (goto-char start)
  (let ((case-fold-search nil))
   (while (re-search-forward "\\([a-z]\\)\\([A-Z]\\)" end t)
     (let ((ch1 (match-string 1))
           (ch2 (match-string 2)))
       (replace-match (format "%s_%s" ch1 (string-downcase ch2)) t t)))))


(provide 'pjb-objc-edit)
;;;; THE END ;;;;
