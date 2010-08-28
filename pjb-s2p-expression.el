;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-s2p-expression.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    199?-??-?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2001
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;******************************************************************************
(require 'pjb-utilities)
(require 'pjb-strings)
(require 'pjb-cl)
(require 'eieio)
(provide 'pjb-s2p-expression)


(defclass AEToken ()
  (
   (ttype
    :initform 'eos
    :accessor ttype
    :initarg  :ttype
    :documentation 
    "Type of token: 'integer, 'float, 'identifier, 'string, or 'special.")
   (value 
    :initform nil
    :accessor value
    :initarg  :value
    :documentation 
    "Value of the token (a number, a string or a symbol).")
   (string
    :initform nil
    :accessor get-string
    :initarg  :string
    :documentation
    "String expression of the token as found in the source.")
   )
  (:documentation "
A AEToken.
"))



(defclass AEScanner ()
  (
   (source 
    :initform ""
    :accessor source
    :initarg  :source
    :documentation "Source string for the scanner.")
   
   (token   
    :initform nil
    :accessor token
    :documentation "Scanned token.")
   
   (slength  
    :initform 0
    :accessor slength
    :documentation "Length of the source string.")
   
   (curr-pos 
    :initform 0
    :accessor curr-pos
    :documentation "Index of the current curr-pos.")
   )
  (:documentation "
A AEScanner.
Given a source string, it will parse it a return a flow of tokens.

The grammar parsed is:

   ['+'|'-'] digit { digit } [ '.' { digit } ] ['e'|'E'] ['+'|'-'] digit { digit }
             converted with string-to-number:
                 integerp --> 'integer
                 floatp   --> 'float
                 
   letter { letter | digit | '_' | '$' } 
             converted with intern
                          --> 'identifier
                 
   single-or-double-quoted-string 
             no conversion
                          --> 'string

   any-other-char 
             converted with intern
                          --> 'special

   spaces are skipped. (9, 10, 11, 12, 13, 32)
"))


(defmethod initWithString ((self AEScanner) source-string)
  "DO:     Initialize the scanner with the given SOURCE-STRING.
NOTE:   scan-next-token must be sent to retrieve the first token.
RETURN: self."
  (if (not (stringp source-string))
      (error "The SOURCE-STRING argument must be a string."))
  (setf (slot-value self 'source)      source-string)
  (setf (slot-value self 'slength)     (length source-string))
  (setf (slot-value self 'curr-pos)    0)
  (setf (slot-value self 'token)       nil)
  self)




(defmethod eos ((self AEScanner))
  "RETURN: Whether the end of source has been reached. (eq token 'eos)"
  (<= (slength self) (curr-pos self)))


(defmethod curr-char ((self AEScanner))
  "PRE:    p=(curr-pos self), e=(eos self)
POST:   p=(curr-pos self).
RETURN: (not e) => The character at curr-pos p in source,
        e       => nil."
  (if (eos self)
      nil
    (aref (source self) (curr-pos self))))


(defmethod advance-character ((self AEScanner))
  "PRE:    p=(curr-pos self), e=(eos self)
POST:   p+1=(curr-pos self).
RETURN: (not e) => The character at curr-pos p+1 in source,
        e       => nil."
    (if (eos self)
        nil
      (setf (slot-value self 'curr-pos) (1+ (curr-pos self)))
      (if (eos self)
          nil
        (aref (source self) (curr-pos self)))))
                


(defun is-space (c)
  "RETURN: Whether C is a space."
  (member c '(9 10 11 12 13 32)))


(defmethod skip-spaces ((self AEScanner))
  "DO:    Skip spaces.
PRE:    p=(curr-pos self)
POST:   p<=(curr-pos self),
        for all i in [p,(- (curr-pos self) 1)], (aref (source self) i) is a space,
        (not (eos self)) => (aref (source self) (curr-pos self)) is not a space.
RETURN: self"
  (while (and (not (eos self)) 
              (is-space (aref (source self) (curr-pos self))))
    (setf (slot-value self 'curr-pos) (1+ (curr-pos self))))
  self)


(defmethod scan-string ((self AEScanner))
  "DO:     Scan a string.
PRE:    terminator=(curr-char self)
POST:   (or (eos self) 
            (and (equal (aref (self source) (- (self curr-pos) 1)) terminator)
                 terminator does not occur unescaped in 
                 (get-string (token self))
RETURN: (token self)"
  (let ((terminator (curr-char self))
        (tokchr     (advance-character self))
        (tokstr     nil)
        (tokval     nil))

    
    (setq tokstr (cons terminator tokstr))
    (while (not (or (eos self) (equal terminator tokchr)))
      (setq tokstr (cons tokchr tokstr))
      (if (equal ?\\ tokchr) ;; we must skip tokchr
          (progn
            (setq tokchr (advance-character self))
            (setq tokstr (cons tokchr tokstr))))
      (setq tokval (cons tokchr tokval))
      (setq tokchr (advance-character self)))
    (if (not (equal terminator tokchr))
        (error "Unterminated string %S." tokstr))
    (setq tokstr (concat (reverse (cons tokchr tokstr))))
    (advance-character self)
    (setq tokval (concat (reverse tokval)))
    (new-token self 'string tokval tokstr)))


(defmethod scan-identifier ((self AEScanner))
  "DO:     Scan an identifier.
PRE:    (is-letter (curr-char self))
POST:   (or (eos self) 
            (and curr-pos points at the first char after the identifier,
                 token is the scanned identifier )
RETURN: (token self)"
  (let (
        (tokchr     (curr-char self))
        (tokstr     nil)
        (tokval     nil))

    (while (and (not (eos self))
                (or (is-digit tokchr)
                    (is-letter tokchr)
                    (member tokchr '( ?_ ?$ ))))
      (setq tokstr (cons tokchr tokstr))
      (setq tokchr (advance-character self)))
    (setq tokstr (concat (reverse tokstr)))
    (new-token self 'identifier (intern tokstr) tokstr)))



(defmethod scan-number ((self AEScanner))
  "DO:     Scan a number.
PRE:    (is-digit (curr-char self))
POST:   (or (eos self) 
            (and curr-pos points at the first char after the number,
                 token is the scanned number )
RETURN: (token self)"
  (let (
        (tokchr     (curr-char self))
        (tokstr     nil)
        (tokval))
    
    (while (and (not (eos self)) (is-digit tokchr))
      (setq tokstr (cons tokchr tokstr))
      (setq tokchr (advance-character self)))
    
    (if (equal tokchr ?.)
        (progn
          (setq tokstr (cons tokchr tokstr))
          (setq tokchr (advance-character self))

          (while (and (not (eos self)) (is-digit tokchr))
            (setq tokstr (cons tokchr tokstr))
            (setq tokchr (advance-character self)))))

    (if (member tokchr '( ?e ?E ))
        (progn
          (setq tokstr (cons tokchr tokstr))
          (setq tokchr (advance-character self))
          (if (member tokchr '( ?+ ?- ))
              (progn
                (setq tokstr (cons tokchr tokstr))
                (setq tokchr (advance-character self))))
          (if (not (is-digit tokchr))
              (error "Syntax error: a digit was expected after '%s'."
                     (concat (reverse tokstr))))
          (while (and (not (eos self)) (is-digit tokchr))
            (setq tokstr (cons tokchr tokstr))
            (setq tokchr (advance-character self)))))

    (setq tokstr (concat (reverse tokstr)))
    (setq tokval (string-to-number tokstr))
    (new-token self (if (integerp tokval) 'integer 'float) tokval tokstr)))


(defmethod scan-special ((self AEScanner))
  "DO:     Scan a special character or special character sequence.
        The following sequences are agregated: <= <> >= == != /= 
PRE:    (not (or (is-letter (curr-char self)) 
                 (is-number (curr-char self))
                 (member (curr-char self) '( ?\" ?' ))))
POST:   (or (eos self) 
            (and curr-pos points at the first char after the special,
                 token is the scanned special )
RETURN: (token self)"
  (let (
        (tokchr     (curr-char self))
        (tokstr     nil)
        (tokval     nil))

    (cond
     ((eq tokchr ?< )
      (setq tokstr (cons tokchr tokstr)
            tokchr (advance-character self))
      (if (member tokchr '( ?= ?> ))
          (setq tokstr (cons tokchr tokstr)
                tokchr (advance-character self))))
     ((member tokchr '( ?> ?= ?! ?/ ))
      (setq tokstr (cons tokchr tokstr)
            tokchr (advance-character self))
      (if (eq tokchr ?=)
          (setq tokstr (cons tokchr tokstr)
                tokchr (advance-character self))))
     (t
      (setq tokstr (cons tokchr tokstr)
            tokchr (advance-character self))))
     (setq tokstr (concat (reverse tokstr)))
     (new-token self 'special (intern tokstr) tokstr)))




(defmethod new-token ((self AEScanner) ttype value string)
  "RETURN: the new token made from ttype, value and string.
POST:   (token self) is the new token."
  (let ((token (make-instance AEToken string
                              :ttype  ttype 
                              :value  value 
                              :string string)))
    (setf (slot-value self 'token) token)
    token))


(defmethod next-token ((self AEScanner))
  "RETURN: (token self)"
  (skip-spaces self)
  (if (eos self)
      (new-token self 'eos nil "")
    (let ((cc (curr-char self)))
      (cond
       ((or (equal ?\" cc) (equal ?' cc))     (scan-string     self))
       ((is-digit cc)                         (scan-number     self))
       ((is-letter cc)                        (scan-identifier self))
       (t                                     (scan-special    self))))
    ))


(defclass AEParser ()
  (
   (scanner 
    :initform (lambda () (make-instance AEScanner))
    :accessor scanner
    :documentation "The scanner.")
   (sexp
    :initform 0
    :accessor sexp
    :documentation 
    "The lisp expression, ready to be evaluated with (eval).")
   )
  (:documentation "
The Parser, and compilator of simple arithmetic expressions.

compare : expr | simple cmp-op expr .
cmp-op : < | > | <= | >= | = | == | <> | != | /= .
expr : term { + term | - term } .
term : fact { * fact | / fact | ^ fact } .
fact : - simple | + simple | simple .
simple : identifier | number | ( expr ) | { expr } | [ expr ] .
"))
   

(defmethod initWithString ((self AEParser) source-string)
  "DO:     Initialize the parser with the source string."
  (initWithString (scanner self) source-string)
  self)

(defmethod parse-simple ((self AEParser))
  "DO:     simple : identifier | number | ( expr ) | { expr } | [ expr ] .
RETURN: a lisp expression : identifier, number  or expr." 
  (let ((op) (expr))
      (setq op (token (scanner self)))
      (cond

       ((eq (value op) (intern "("))
        (next-token (scanner self))
        (setq expr (parse-expr self))
        (if (eq (value (token (scanner self))) (intern ")"))
            (progn
              (next-token (scanner self))
              expr)
          (error "Parenthesis mismatch at: '%s'."
                 (substring (source (scanner self)) 
                            (1- (curr-pos (scanner self)))))))

       ((eq (value op) (intern "{"))
        (next-token (scanner self))
        (setq expr (parse-expr self))
        (if (eq (value (token (scanner self))) (intern "}"))
            (progn
              (next-token (scanner self))
              expr)
          (error "Parenthesis mismatch at: '%s'."
                 (substring (source (scanner self)) 
                            (1- (curr-pos (scanner self)))))))

       ((eq (value op) (intern "["))
        (next-token (scanner self))
        (setq expr (parse-expr self))
        (if (eq (value (token (scanner self))) (intern "]"))
            (progn
              (next-token (scanner self))
              expr)
          (error "Parenthesis mismatch at: '%s'."
                 (substring (source (scanner self)) 
                            (1- (curr-pos (scanner self)))))))

       ((member (ttype op) '(identifier integer float))
        (next-token (scanner self))
        (value op))

       (t
        (error "Unexpected token at: '%s'."
                 (substring (source (scanner self))
                            (1- (curr-pos (scanner self)))))))))



(defmethod parse-fact ((self AEParser))
  "DO:     fact : - simple | + simple | simple .
RETURN: a lisp expression : (op simple) or simple." 
  (let ((op))
      (setq op (token (scanner self)))
      (if (member (value op) '(+ -))
          (progn
            (next-token (scanner self))
            (list (value op) (parse-simple self)))
        (parse-simple self))))


(defmethod parse-term ((self AEParser))
  "DO:     term : fact { * fact | / fact | ^ fact } .
RETURN: a lisp expression : (op term fact) or fact."
  (let ((fact) (op))
    (setq fact (parse-fact self))
    (setq op (token (scanner self)))
    (while (member (value op) '(* / ^))
      (next-token (scanner self))
      (setq fact (list (value op) 
                       (if (eq (value op) '/) (list 'float fact) fact) 
                       (parse-fact self)))
      (setq op (token (scanner self))))
    fact))



(defun not-equal (a b) (not (equal a b)))

(defun lisp-compare-op (op-val)
  (cond 
   ((member op-val '( = == ))     'equal)
   ((member op-val '( != <> /= )) 'not-equal)
   (t                             op-val)
   ))


(defmethod parse-compare ((self AEParser))
  "DO:     compare : expr | expr cmp-op expr .
        cmp-op : < | > | <= | >= | = | == | <> | != | /= .
RETURN: a lisp expression : (op term fact) or fact."
  (let (compare op)
    (setq compare (parse-expr self))
    (setq op (token (scanner self)))
    (if (member (value op) '( <  >  <=  >=  =  ==  <>  !=  /= ))
        (progn
          (next-token (scanner self))
          (setq compare (list (lisp-compare-op (value op))
                              compare 
                              (parse-expr self)))))
    compare))



(defmethod parse-expr ((self AEParser))
  "DO:     expr : term { + term | - term } .
RETURN: a lisp expression : (+ expr term), (- expr term) or term."
  (let ((term) (op))
    (setq term (parse-term self))
    (setq op (token (scanner self)))
    (while (member (value op) '(+ -))
      (next-token (scanner self))
      (setq term (list (value op) term (parse-term self)))
      (setq op (token (scanner self))))
    term))


(defmethod parse-nothing-more ((self AEParser))
  "DO:     Report an error when (not (eos (scanner self))).
RETURN: self."
  (if (not (eos (scanner self))) 
      (error "Exceeding characters: '...%s'." 
             (substring (source (scanner self)) 
                        (1- (curr-pos (scanner self))))))
  self)


(defmethod compile-arithmetic-expr ((self AEParser))
  "DO:     Parse and compile the expression.
RETURN: self."
  (next-token (scanner self))
  (if (eos (scanner self))
      (setf (slot-value self 'sexp) 0)
    (setf (slot-value self 'sexp) (parse-compare self))
    (parse-nothing-more self))
  self)


(defun s2p-calculette-to-lisp ()
  "See calculette."
  (interactive)
  (s2p-calculette t))

(defun eval-infix-arithmetic (infix-expression-string)
  "Evaluate the arithmetic expression INFIX-EXPRESSION-STRING."
  (let ((parser (make-instance AEParser))
        sexp)
    (initWithString parser infix-expression-string)
    (compile-arithmetic-expr parser)
    ;; (if displayLisp (printf "\n%S" (sexp parser)))
    (eval (sexp parser))))

 
(defun s2p-calculette (&optional displayLisp)
  "Evaluate the arithmetic expression found from the beginning of the line 
up to the point, and insert the result at the point.

Identificators are interpreted as lisp symbols 
               and are expected to have number value.

The accepted operators are :  
        unary:   + -
        binary:  + - * / ^  < > <= >= = == != <>
        parenthesis:  () {} []
"
  (interactive "P")
  (let* ((last-point  (point))
         (from-point  (progn (beginning-of-line) (point)))
         (source      (buffer-substring from-point last-point))
         (parser      (make-instance AEParser))
         (sexp))
    (goto-char last-point)
    (initWithString parser source)
    (compile-arithmetic-expr parser)
    (if displayLisp (printf "\n%S" (sexp parser)))
    (printf "\n %S\n" (eval (sexp parser)))))



;    (while (not (equal 'eos (ttype token)))
;      (printf (if (equal 'special (ttype token))
;                  "%-10S %c\n"
;                "%-10S %S\n") (ttype token) (value token))
;      (setq token (next-token scanner)))
;    (printf "%-10S %S\n" (ttype token) (value token))))





;;;; pjb-expression.el                -- 2002-12-27 15:14:46 -- pascal   ;;;;
