;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-i2p-expression.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This packages exports functions to convert infix expressions
;;;;    to prefix s-expressions,
;;;;    and to simplify and evaluate these s-expressions.
;;;;
;;;;    i2p-calculette, i2p-evaluate, i2p-eval, i2p-simplify, i2p-expression.
;;;;
;;;;    SEE ALSO: pjb-expression.el which implement a calculette, evaluate and
;;;;              parse from a string instead of from a parsed i-expr.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2002-12-27 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
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
(require 'pjb-cl)
(provide 'pjb-i2p-expression)


;;----------------------------------------------------------------------
;;   expression     ::= numexpression | boolcomparison .
;;
;;   disjonction    ::= conjonction [ or disjonction ]
;;   conjonction    ::= proposition [ and conjonction ]
;;   proposition    ::= 'not' proposition | numcomparison | boolsimple
;;   boolsimple     ::= fun-name ( expression { , expression } )
;;                    | boolconstant | variable | (disjonction )
;;   numcomparison  ::= numexpression [ numcomp-op numexpression ]
;;   boolcomparison ::= boolexpression [ boolcom-op boolexpression ]
;;
;;   numexpression  ::= numexpression [ termop term ]
;;   term           ::= term [ factop factor ]
;;   factor         ::= factor [ infix-op infix ]
;;   infix          ::= prefix-op infix | suffix
;;   suffix         ::= suffix suffix-op | simple
;;   simple         ::= fun-name ( expression { , expression } )
;;                    | numconstant | variable | ( numexpression )
;;
;;   fun-name must be a fbound symbol.
;;   variable must be bound or have the property :operator :unknown
;;   constants are strings, numbers or arrays
;;   (numerical operators could be defined for arrays too)
;;
;;   plist:
;;     :operator  :term
;;     :operator  :factor
;;     :operator  :infix
;;     :operator  :suffix
;;     :operator  :prefix
;;     :operator  :function
;;     :operator  :numcomp
;;     :operator  :boolcomp
;;
;;   termop         ::= + | - | ...
;;   factop         ::= * | / | ...
;;   prefix-op      ::= - | ...
;;   suffix-op      ::= ! | ...
;;   infix-op       ::= ^ | ...
;;   numcomp-op     ::= < | <= | = | /= | > | >=
;;   boolcomp-op    ::= <=> | <== | ==> | xor
;;----------------------------------------------------------------------


(eval-when (compile load eval)
  (put 'i2p-error 'error-conditions '(i2p-error error)))


(defun i2p-calculette-to-lisp ()
  "See i2p-calculette."
  (interactive)
  (i2p-calculette t))

(defun i2p-calculette (&optional displayLisp)
  (interactive "P")
  (let* ((from-point  (progn (beginning-of-line) (point)))
         (last-point  (progn (end-of-line)       (point)))
         (source  (concat "( " (buffer-substring from-point last-point) " )"))
         i-expr pos)
    (let ((rfs (read-from-string source)))
      (setq i-expr (car rfs) pos (cdr rfs)))
    (if (/= pos (length source))
        (signal 'scan-error (list "Unbalanced parentheses"
                                  (- (+ from-point pos) 2))))
    (goto-char last-point)
    (insert (i2p-evaluate i-expr displayLisp))))



(defun i2p-evaluate (i-expr &optional displayLisp)
  (condition-case error
      (multiple-value-bind (s-expr rest) (i2p-expression i-expr)
        (if rest
            (signal 'i2p-error (list (format "Remaining %S" rest) rest)))
        (setq s-expr (i2p-simplify s-expr))
        (concatenate 'string
          (if displayLisp (format "\n%S" s-expr) "")
          (multiple-value-bind (res ue) (i2p-eval s-expr)
            (format "\n %S\n" res))))))



(defun i2p-eval (s-expr)
  "
DO:      evaluates as much as possible of s-expr.
RETURN:  a value or a partially evalued s-expr,
         whether s-expr is completely evalued
"
  (if (consp s-expr)
      (multiple-value-bind (arguments all-evaluated)
          (do ((ue-args (cdr s-expr) (cdr ue-args))
               (e-args)
               (e-all t))
              ((null ue-args) (values (nreverse e-args) e-all))
            (multiple-value-bind (arg evaluated) (i2p-eval (car ue-args))
              (push arg e-args)
              (unless evaluated (setq e-all nil))))
        (if all-evaluated
            (condition-case error
                (values (eval (cons (car s-expr) arguments)) t)
              (error (values (cons (car s-expr) arguments) nil)))
          (values (cons (car s-expr) arguments) nil)))
    (condition-case error
        (values (eval s-expr) t)
      (error (values s-expr nil)))))




(defun more-than-one-element (list)
  "
RETURN: (< 1 (length list))
"
  (and (consp list)
       (consp (cdr list))))



;;; (show (more-than-one-element nil)
;;;       (more-than-one-element :not-a-list)
;;;       (more-than-one-element '(a))
;;;       (more-than-one-element '(a b))
;;;       (more-than-one-element '(a b c)) )


(defun i2p-simplify (sexp)
  "
RETURN: A simplified sexp.
NOTE:   Implemented rules:
        - collapsing { < = >, and, or, +, *, neg }
        - left-collapsing { -, / }
        - neg(neg(a)) = a
        - neg(0) = 0
        - a+0 = 0+a = a
        - a-0 = a
        - a-a = 0
        - a*0 = 0*a = 0
        - a*1 = 1*a = a
        - t or a  =  a or t  =  t
        - nil and a  =  a and nil  =  nil
        - t and t  =  t
        - nil or nil  =  nil
        - t xor t  =  nil xor nil  =  nil
        - t xor nil  =  nil xor t  =  t
        Not implemented yet:
        - 0-a = -a
        - a/1 = a
        - a^1 = a
        - a^0 = 0 if a/ = 0
        - 0^b = 0 if b/ = 0
        Could be implemented too:
        - a-b-c = a-(b+c)
        - a/b/c = a/(b*c)
"
  ;;  (setq sexp '(neg (neg 2)) operator (car sexp) subsexps (cdr sexp))
  (block :simplifying
    (if (consp sexp)
        (let ((operator (car sexp))
              (subsexps (cdr sexp)))
          ;; phase 0: simplifying subexpressions
          (setq subsexps (mapcar (function i2p-simplify) subsexps))
          ;; phase 1: collapsing
          (case operator
            ((<=> and or + *)
             (setq subsexps
                   (mapcan (lambda (sub)
                             (if (consp sub)
                                 (if (eq operator (car sub))
                                     (cdr sub) (list sub)) (list sub)))
                           subsexps)) )
            ((- /)
             (setq subsexps
                   (if (and (consp (car subsexps))
                            (eq operator (caar subsexps)))
                       (append (cdar subsexps) (cdr subsexps))
                     subsexps)) )
            (neg
             (when (and (consp (car subsexps))
                        (eq operator (caar subsexps)))
               (setq sexp (cadar subsexps))
               (return-from :simplifying)) )
            ) ;;case
          ;; phase 2: neutral or absorbing elements
          (case operator
            (+
             (setq subsexps (mapcan (lambda (sub)
                                      (if (and (numberp sub) (= 0 sub))
                                          nil (list sub)))
                                    subsexps))
             (setq sexp (if subsexps
                            (if (more-than-one-element subsexps)
                                (cons operator subsexps)
                              (car subsexps)) 0)) )
            (-
             (setq subsexps (cons (car subsexps)
                                  (mapcan (lambda (sub)
                                            (if (and (numberp sub) (= 0 sub))
                                                nil (list sub)))
                                          (cdr subsexps))))
             (setq sexp (if (more-than-one-element subsexps)
                            (if (more-than-one-element (cdr subsexps))
                                (cons operator subsexps)
                              ;; TODO: (not (equal 0 0.0))
                              (if (equal (car subsexps) (cadr subsexps))
                                  0 (cons operator subsexps)))
                          (car subsexps) )) )
            (neg
             (if (and (numberp (car subsexps)) (= 0 (car subsexps)))
                 (setq sexp 0)))
            (*
             (if (find 0 subsexps :test (lambda (a b) (and (numberp a)
                                                           (numberp b)
                                                           (= a b))))
                 (setq sexp 0)
               (progn
                 (setq subsexps (mapcan (lambda (sub)
                                          (if (and (numberp sub) (= 1 sub))
                                              nil (list sub)))
                                        subsexps))
                 (setq sexp (if subsexps
                                (if (more-than-one-element subsexps)
                                    (cons operator subsexps)
                                  (car subsexps)) 1)))) )
            (/
             (setq subsexps (cons (car subsexps)
                                  (mapcan (lambda (sub)
                                            (if (and (numberp sub) (= 1 sub))
                                                nil (list sub)))
                                          (cdr subsexps))))
             (setq sexp (if (more-than-one-element subsexps)
                            (cons operator subsexps) (car subsexps))) )
            (=
             ;; TODO: (not (equal 0 0.0))
             (setq sexp
                   (if (and (= 2 (length subsexps))
                            (equal (car subsexps) (cadr subsexps)))
                       t
                     (cons operator subsexps))) )
            (and
             (setq sexp
                   (cond
                    ((every (lambda (x) (eq x t)) subsexps)   t)
                    ((null (cdr subsexps))                    (car subsexps))
                    ((position nil subsexps
                               :test (lambda (a b) (eq (not a) (not b))))  nil)
                    (t (cons operator subsexps)))) )
            (or
             (setq sexp
                   (cond
                    ((null subsexps)                           nil)
                    ((some (lambda (x) (eq x t)) subsexps)     t)
                    ((null (cdr subsexps))                     (car subsexps))
                    ((position t subsexps :test (function eq)) t)
                    (t (cons operator subsexps)))) )
            (xor
             (setq sexp
                   (let ((a (nth 0 subsexps))
                         (b (nth 1 subsexps)))
                     (cond
                      ((or (and (eq t   a) (eq t   b))
                           (and (eq nil a) (eq nil b))) nil)
                      ((or (and (eq nil a) (eq t   b))
                           (and (eq t   a) (eq nil b))) t)
                      (t (cons operator subsexps))))) )
            (t (setq sexp (cons operator subsexps)))
            ) ;;case
          ))) ;;symplifying
  sexp)



(defmacro <=> (&optional first-bool &rest other-bools)
  "
RETURN:  Whether all arguments are nil or all are not nil,
         but evaluating them  only as necessary
         (stops ealuating them as soon as one is not equivalent to the first).
"
  `(do ((first (not (eval ,first-bool)))
        (rest  ',other-bools (cdr rest)))
       ((or (null rest)
            (not (eq first (not (eval (car rest)))))) (null rest))))



;;;   (mapcar (lambda (e) (printf "%3s   ~S\n" (eval e) e))
;;;           '((<=>)
;;;             (<=> nil) (<=> t)
;;;             (<=> nil nil) (<=> nil t) (<=> t nil)  (<=> t t)
;;;             (<=> nil nil nil) (<=> nil t nil) (<=> t nil nil)  (<=> t t nil)
;;;             (<=> nil nil t) (<=> nil t t) (<=> t nil t)  (<=> t t t)))
;;; (show (macroexpand (quote (<=> nil nil t))))


(defun ==> (p q) "RETURN:  p ==> q" (or (not p) q))
(defun <== (q p) "RETURN:  q <=> p" (or (not p) q))
(defun xor (p q) "RETURN:  p xor q" (not (eq (not p) (not q))))
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(defalias '! 'fact)
(defalias 'neg '-)

;;; (show (xor nil nil) (xor nil t) (xor t nil) (xor t t) (xor 1 2) (xor nil 2))


(defmacro i2p-try-both (sexp-1 sexp-2)
  `(let ((pexp1 nil) (ires1 nil) (erro1 nil) (rlen1 most-positive-fixnum)
         (pexp2 nil) (ires2 nil) (erro2 nil) (rlen2 most-positive-fixnum))
     ;; try both
     (condition-case error
         (multiple-value-bind (s-expr i-rest) ,sexp-1
           (setq pexp1 s-expr ires1 i-rest rlen1 (length i-rest)))
       (i2p-error (setq erro1 (car error) rlen1 (length (cadr error))))
       (error (setq erro1 error)) )
     (if (or erro1 ires1) ;; if sexp-1 ate all without error,
         ;; then there's no need to try the other
         (progn
           (condition-case error
               (multiple-value-bind (s-expr i-rest) ,sexp-2
                 (setq pexp2 s-expr ires2 i-rest rlen2 (length i-rest)))
             (i2p-error (setq erro2 (car error) rlen2 (length (cadr error))))
             (error (setq erro2 error)) )
;;;      (mapc (lambda (s)
;;;              (show s (eval s)))
;;;            '(pexp1 ires1 erro1 rlen1 pexp2 ires2  erro2 rlen2))
           (if erro1
               (cond
                ((not erro2)     (values pexp2 ires2))
                ((< rlen1 rlen2) (signal (car erro1) (cdr erro1)))
                (t               (signal (car erro2) (cdr erro2))))
             (cond
              (erro2             (values pexp1 ires1))
              ((< rlen1 rlen2)   (values pexp1 ires1))
              (t                 (values pexp2 ires2)))))
       (values pexp1 ires1))))



(defun i2p-expression (expression)
  (i2p-try-both (i2p-numexpression expression)
                (i2p-boolcomparison expression) ))



(defun i2p-disjonction (disjonction)
  (multiple-value-bind (s-conjonction i-rest) (i2p-conjonction disjonction)
    (cond
     ((null i-rest)               (values s-conjonction i-rest))
     ((not (eq 'or (car i-rest))) (values s-conjonction i-rest))
     ((< (length i-rest) 2)
      (signal :i2p-error
              (list (format "Missing disjonction after %S" (car i-rest))
                    i-rest)) )
     (t (multiple-value-bind (s-disjonction ii-rest)
            (i2p-disjonction (cdr i-rest))
          (values (list (car i-rest) s-conjonction s-disjonction) ii-rest))))))



(defun i2p-conjonction (conjonction)
  (multiple-value-bind (s-proposition i-rest) (i2p-proposition conjonction)
    (cond
     ((null i-rest)                (values s-proposition i-rest))
     ((not (eq 'and (car i-rest))) (values s-proposition i-rest))
     ((< (length i-rest) 2)
      (signal :i2p-error
              (list (format "Missing conjonction after %S" (car i-rest))
                    i-rest)) )
     (t (multiple-value-bind (s-conjonction ii-rest)
            (i2p-conjonction (cdr i-rest))
          (values (list (car i-rest) s-proposition s-conjonction) ii-rest))))))



(defun i2p-proposition (proposition)
  (if (eq 'not (car proposition))
      (multiple-value-bind (s-proposition i-rest)
          (i2p-proposition (cdr proposition))
        (values (list (car proposition) s-proposition) i-rest))

    (i2p-try-both (i2p-numcomparison proposition)
                  (i2p-boolsimple proposition) )))





(defun i2p-boolcomparison (comparison)
  (multiple-value-bind (s-expr-1 i-rest) (i2p-disjonction comparison)
    (if (and i-rest (i2p-boolcompop-p (car i-rest)))
        (multiple-value-bind (s-expr-2 ii-rest) (i2p-disjonction (cdr i-rest))
          (values (list (car i-rest) s-expr-1 s-expr-2) ii-rest))
      (values s-expr-1 i-rest))))



(defun i2p-numcomparison (comparison)
  (multiple-value-bind (s-expr-1 i-rest) (i2p-numexpression comparison)
    (if (and i-rest (i2p-numcompop-p (car i-rest)))
        (multiple-value-bind (s-expr-2 ii-rest) (i2p-numexpression (cdr i-rest))
          (values (list (car i-rest) s-expr-1 s-expr-2) ii-rest))
      (values s-expr-1 i-rest))))



(defun i2p-boolsimple (simple)
  (let ((first (car simple)))
    (cond
     ((consp first)
      (multiple-value-bind (i-expr i-rest) (i2p-disjonction first)
        (if i-rest
            (signal :i2p-error
                    (list (format "Unexpected rest: %S" i-rest) i-rest)) )
        (values i-expr (cdr simple))))
     ((i2p-function-p first)
      ;;(and (not (i2p-anyop-p first)) (consp (cadr simple))))
      (values (cons first (i2p-argument-list (cadr simple))) (cddr simple)) )
     ((i2p-anyop-p first)
      (signal :i2p-error
              (list (format  "Syntax error from %S" simple) simple)) )
     (t
      (values first (cdr simple))))))



(defun i2p-numexpression (expression)
  (multiple-value-bind (s-term i-rest) (i2p-term expression)
    (cond
     ((null i-rest)                        (values s-term i-rest))
     ((not (i2p-termop-p (car i-rest)))    (values s-term i-rest))
     ((< (length i-rest) 2)
      (signal :i2p-error
              (list (format "Missing expression after %S" (car i-rest))
                    i-rest)) )
     (t (multiple-value-bind (s-expr ii-rest) (i2p-numexpression (cdr i-rest))
          (values (list (car i-rest) s-term s-expr) ii-rest))))))



(defun i2p-term (term)
  (multiple-value-bind (s-factor i-rest) (i2p-factor term)
    (cond
     ((null i-rest)                        (values s-factor i-rest))
     ((not (i2p-factorop-p (car i-rest)))  (values s-factor i-rest))
     ((< (length i-rest) 2)
      (signal :i2p-error
              (list (format "Missing term after %S" (car i-rest))
                    i-rest)) )
     (t (multiple-value-bind (s-term ii-rest) (i2p-term (cdr i-rest))
          (values (list (car i-rest) s-factor s-term) ii-rest))))))



(defun i2p-factor (factor)
  (multiple-value-bind (s-infix i-rest) (i2p-infix factor)
    (cond
     ((null i-rest)                        (values s-infix i-rest))
     ((not (i2p-infixop-p (car i-rest)))   (values s-infix i-rest))
     ((< (length i-rest) 2)
      (signal :i2p-error
              (list (format "Missing factor after %S" (car i-rest))
                    i-rest)) )
     (t (multiple-value-bind (s-factor ii-rest) (i2p-factor (cdr i-rest))
          (values (list (car i-rest) s-infix s-factor) ii-rest))))))



(defun i2p-infix (infix)
  (if (i2p-prefixop-p (car infix))
      (multiple-value-bind (s-infix i-rest) (i2p-infix (cdr infix))
        (values (list (if (eq '- (car infix)) 'neg (car infix))
                      s-infix) i-rest))
    (i2p-suffix infix)))



(defun i2p-suffix (suffix)
  (multiple-value-bind (s-suffix i-rest) (i2p-simple suffix)
    (do ()
        ( (or (null i-rest) (not (i2p-suffixop-p (car i-rest))))
          (values s-suffix i-rest) )
      (setq s-suffix (list (car i-rest) s-suffix)
            i-rest   (cdr i-rest)))))



(defun i2p-argument-list (arguments)
  (do ((s-arguments nil)
       (rest arguments) )
      ((null rest) (nreverse s-arguments))
    (multiple-value-bind (s-arg i-rest) (i2p-expression rest)
      (push s-arg s-arguments)
      (if i-rest
          (if (or (eq '\, (car i-rest)) (eq ': (car i-rest)))
              (progn
                (if (null (cdr i-rest))
                    (signal :i2p-error
                            (list (format "Missing argument after %S"
                                          (car i-rest)) i-rest)))
                (setq rest (cdr i-rest)))
            (signal :i2p-error
                    (list (format "Expected a coma insteand of %S"
                                  (car i-rest)) i-rest)))
        (setq rest i-rest)))))



(defun i2p-simple (simple)
  (let ((first (car simple)))
    (cond
     ((consp first)
      (multiple-value-bind (i-expr i-rest) (i2p-numexpression first)
        (if i-rest
            (signal :i2p-error
                    (list (format "Unexpected rest: %S" i-rest) i-rest)) )
        (values i-expr (cdr simple))))
     ((i2p-function-p first)
      ;;    (and (not (i2p-anyop-p first)) (consp (cadr simple))))
      (values (cons first (i2p-argument-list (cadr simple))) (cddr simple)) )
     ((i2p-anyop-p first)
      (signal :i2p-error
              (list (format "Syntax error from %S" simple) simple)) )
     (t
      (values first (cdr simple))))))




(defun i2p-numcompop-p (operator)
  (or (member* operator '(< <= = /= >= >) :test (function eq))
      (and (symbolp operator) (eq (get operator :operator) :numcomp))))



(defun i2p-boolcompop-p (operator)
  (or (member* operator '(<== <=> xor ==>) :test (function eq))
      (and (symbolp operator) (eq (get operator :operator) :boolcomp))))



(defun i2p-termop-p (operator)
  (or (eq operator '+)
      (eq operator '-)
      (and (symbolp operator) (eq (get operator :operator) :term))))



(defun i2p-factorop-p (operator)
  (or (eq operator '*)
      (eq operator '/)
      (and (symbolp operator) (eq (get operator :operator) :factor))))



(defun i2p-prefixop-p (operator)
  (or (eq operator '-)
      (and (symbolp operator) (eq (get operator :operator) :prefix))))



(defun i2p-infixop-p (operator)
  (or (eq operator '^)
      (and (symbolp operator) (eq (get operator :operator) :infix))))



(defun i2p-suffixop-p (operator)
  (or (eq operator '!)
      (and (symbolp operator) (eq (get operator :operator) :suffix))))



(defun i2p-function-p (operator)
  (and (symbolp operator)
       (or (fboundp operator)
           (eq (get operator :operator) :function))
       (not (or
             (i2p-termop-p operator)
             (i2p-factorop-p operator)
             (i2p-prefixop-p operator)
             (i2p-infixop-p operator)
             (i2p-suffixop-p operator)))))



(defun i2p-conjonctionop-p (operator)
  (eq operator 'and))


(defun i2p-disjonctionop-p (operator)
  (eq operator 'or))


(defun i2p-boolprefixop-p (operator)
  (eq operator 'not))


(defun i2p-anyop-p (operator)
  (find t '(
            i2p-numcompop-p i2p-boolcompop-p i2p-termop-p i2p-factorop-p
            i2p-prefixop-p i2p-infixop-p i2p-suffixop-p i2p-function-p
            i2p-conjonctionop-p i2p-disjonctionop-p i2p-boolprefixop-p )
        :test (function eq)
        :key  (lambda (x)
                (or (eq x t) (funcall x operator)) )))



;;;; pjb-i2p-expression.el            -- 2003-04-01 23:20:01 -- pascal   ;;;;
