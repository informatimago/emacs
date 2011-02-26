;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-pmatch.el
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Sexp Pattern Matcher.
;;;;
;;;;
;;;;    
;;;;    Pattern Matcher
;;;;    ===============
;;;;    
;;;;    This is a simple-minded `s-expression` pattern matcher.
;;;;    
;;;;    The main function is `match`, which takes a pattern and a target
;;;;    s-expression.  It returns a list of bindings, or a list starting with
;;;;    the keyword :failed if matching could not complete.
;;;;    
;;;;    It does a job similar to regular expressions and `string-match`, but
;;;;    works on lists (and sublists) of atoms instead of strings (vectors of
;;;;    characters).
;;;;    
;;;;    
;;;;    
;;;;    Simple patterns
;;;;    ---------------
;;;;    
;;;;    Simple patterns are normal sexp, without any of the special symbols
;;;;    defined below. ::
;;;;    
;;;;        (match 'an-atom
;;;;               'an-atom)
;;;;        --> nil
;;;;    
;;;;        (match 'an-atom-pattern
;;;;               'a-different-atom-expression)
;;;;        --> (:failed (:different an-atom-pattern a-different-atom-expression))
;;;;    
;;;;    
;;;;        (match '(a simple literal pattern)
;;;;               '(a simple literal pattern))
;;;;        --> nil
;;;;    
;;;;        (match '(a simple literal pattern)
;;;;               '(a simple literal expression))
;;;;        --> (:failed (:different pattern expression))
;;;;    
;;;;    
;;;;    The pattern and expression may of course contain sub-expressions: ::
;;;;    
;;;;        (match '(I should buy (10 apples) (2 kg of oranges) (a pleasant book))
;;;;               '(I should buy (10 apples) (2 kg of oranges) (a pleasant book)))
;;;;        --> nil
;;;;    
;;;;        (match '(I should buy (10 apples) (2 kg of oranges) (a pleasant book))
;;;;               '(I should buy (10 apples) (3 kg of oranges) (a pleasant book)))
;;;;        --> (:failed (:different 2 3))
;;;;    
;;;;    
;;;;    So far, it is not much different from equalp.  But the pattern may
;;;;    contain in place of any element, one of the following special symbols: ::
;;;;    
;;;;        !av        expects a symbol (variable).
;;;;        !ac        expects a constant (any non-symbol atom).
;;;;        !ax        expects anything (one sexp).
;;;;    
;;;;    or a list starting with one of the following special symbols: ::
;;;;    
;;;;        (!v n)     expects a symbol (variable)       and bind it.
;;;;        (!c n)     expects a constant (atom)         and bind it.
;;;;        (!x n)     expects anything (one sexp)       and bind it.
;;;;        (!n n ...) expects anything (several items)  and bind them.
;;;;        (!+ ...)   expects anything (one or more times).  
;;;;        (!* ...)   expects anything (zero or more times). 
;;;;        (!! ...)   expects anything (zero or one time).
;;;;    
;;;;    Anonymous tokens
;;;;    ----------------
;;;;    
;;;;    `!av` matches any symbol, and rejects lists or other non-symbol atoms: ::
;;;;    
;;;;        (match '(!av eats an !av)
;;;;               '(John eats an apple))
;;;;        --> nil
;;;;    
;;;;        (match '(!av eats an !av)
;;;;               '(John eats a banana))
;;;;        --> (:failed (:different an a))
;;;;    
;;;;        (match '(!av eats an !av)
;;;;               '((a big elephant) eats an apple))
;;;;        --> (:failed (:not-variable ((a big elephant) eats an apple)))
;;;;    
;;;;        (match '(!av eats an !av)
;;;;               '(42 eats an apple))
;;;;        --> (:failed (:not-variable (42 eats an apple)))
;;;;    
;;;;    
;;;;    `!ac` matches any non-symbol atoms, and rejects lists or symbols: ::
;;;;    
;;;;        (match '(I ate !ac kg of apples)
;;;;               '(I ate 42 kg of apples))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate !ac kg of apples)
;;;;               '(I ate (* 21 2) kg of apples))
;;;;        --> (:failed (:not-constant ((* 21 2) kg of apples)))
;;;;    
;;;;        (match '(I ate !ac kg of apples)
;;;;               '(I ate two kg of apples))
;;;;        --> (:failed (:not-constant (two kg of apples)))
;;;;    
;;;;    `!ax` matches one s-sexp, whatever it is: ::
;;;;    
;;;;        (match '(I ate !ax kg of apples)
;;;;               '(I ate 42 kg of apples))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate !ax kg of apples)
;;;;               '(I ate (* 21 2) kg of apples))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate !ax kg of apples)
;;;;               '(I ate two kg of apples))
;;;;        --> nil
;;;;    
;;;;    Repeatition and option tokens
;;;;    -----------------------------
;;;;    
;;;;    - `!+` allows the matching of a subsequence of patterns one or more times.
;;;;    - `!*` allows the matching of a subsequence of patterns zero or more times.
;;;;    - `!!` allows the matching of a subsequence of patterns zero or one time.
;;;;    
;;;;    
;;;;        (match '(I ate (!+ and ate))
;;;;               '(I ate))
;;;;        --> (:failed (:missing-repeat ((!+ and ate))))
;;;;    
;;;;        (match '(I ate (!+ and ate))
;;;;               '(I ate and ate))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate (!+ and ate))
;;;;               '(I ate and ate and ate))
;;;;        --> nil
;;;;    
;;;;    
;;;;    
;;;;        (match '(I ate (!* and ate))
;;;;               '(I ate))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate (!* and ate))
;;;;               '(I ate and ate))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate (!* and ate))
;;;;               '(I ate and ate and ate))
;;;;        --> nil
;;;;    
;;;;    
;;;;    
;;;;        (match '(I ate (!! and ate))
;;;;               '(I ate))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate (!! and ate))
;;;;               '(I ate and ate))
;;;;        --> nil
;;;;    
;;;;        (match '(I ate (!! and ate))
;;;;               '(I ate and ate and ate))
;;;;        --> (:failed (:different nil (and ate and ate)))
;;;;    
;;;;    
;;;;    Of course, the sub-patterns can be anything: ::
;;;;    
;;;;        (match '(I ate (!* and !av))
;;;;               '(I ate and ran and sat))
;;;;         --> nil
;;;;    
;;;;        (match '(I ate (!* and !av))
;;;;               '(I ate and ran or sat))
;;;;        --> (:failed (:different nil (and ran or sat)))
;;;;    
;;;;        (match '(I ate (!* and !av))
;;;;               '(I ate and (also ran) and sat))
;;;;        --> (:failed (:different nil (and (also ran) and sat)))
;;;;    
;;;;    
;;;;    Binding tokens
;;;;    --------------
;;;;    
;;;;    Now is the interesting section.  The sub-expressions matched by the
;;;;    pattern may be remembered and given a name.  That association of a
;;;;    name and a sub-expression is called a binding.  They're stored in a
;;;;    cons cell with the name in the car, and the sub-expression in the cdr.
;;;;    
;;;;    `(!v name) binds a symbol to the `name`: ::
;;;;    
;;;;        (match '((!v name) eats an (!v fruit))
;;;;               '(John eats an apple))
;;;;        --> ((fruit . apple) (name . John))
;;;;    
;;;;        (match '((!v name) eats an (!v fruit))
;;;;               '(John eats a banana))
;;;;        --> (:failed (:different an a) (name . John))
;;;;    
;;;;        (match '((!v name) eats an (!v fruit))
;;;;               '(John eats an 8))
;;;;        --> (:failed (:not-variable (8)) (name . John))
;;;;    
;;;;    Notice that in case of mismatch, the bindings found so far are found
;;;;    after the :failed and reason list in the result.
;;;;    
;;;;    
;;;;    `(!c name) is used to bind non-symbol atoms: ::
;;;;    
;;;;        (match '(I ate (!c amount) kg of apples)
;;;;               '(I ate 42 kg of apples))
;;;;        --> ((amount . 42))
;;;;    
;;;;        (match '(I ate (!c amount) kg of apples)
;;;;               '(I ate (* 21 2) kg of apples))
;;;;        --> (:failed (:not-constant ((* 21 2) kg of apples)))
;;;;    
;;;;        (match '(I ate (!c amount))
;;;;               '(I ate two kg of apples))
;;;;        --> (:failed (:not-constant (two kg of apples)))
;;;;    
;;;;    
;;;;    and `(!x name) is used to bind any sexp: ::
;;;;    
;;;;        (match '(I ate (!x amount) kg of apples)
;;;;               '(I ate 42 kg of apples))
;;;;        --> ((amount . 42))
;;;;    
;;;;        (match '(I ate (!x amount) kg of apples)
;;;;               '(I ate (* 21 2) kg of apples))
;;;;        --> ((amount * 21 2))
;;;;    
;;;;        (match '(I ate (!x amount) kg of apples)
;;;;               '(I ate two kg of apples))
;;;;        --> ((amount . two))
;;;;    
;;;;    
;;;;    Finally, `(!n name ...)` can be used to bind a sequence of
;;;;    sub-patterns: ::
;;;;    
;;;;        (match '(I ate (!n amount-with-unit !ac !av) of apples)
;;;;               '(I ate 42 kg of apples))
;;;;        --> ((amount-with-unit 42 kg))
;;;;    
;;;;    
;;;;    Now, one important feature, is that if a name is used twice in the
;;;;    pattern, each time it matches a sub-expression, these sub-expressions
;;;;    must be equal. ::
;;;;    
;;;;        (match '(I ate (!n amount-with-unit !ac !av) of apples
;;;;                   and (!n amount-with-unit !ac !av) of bananas)
;;;;               '(I ate 42 kg of apples and 42 kg of bananas))
;;;;        --> ((amount-with-unit 42 kg))
;;;;    
;;;;        (match '(I ate (!n amount-with-unit !ac !av) of apples
;;;;                   and (!n amount-with-unit !ac !av) of bananas)
;;;;               '(I ate 42 kg of apples and 3 kg of bananas))
;;;;        --> (:failed (:different of 42))
;;;;                     ;; granted, the reason sublist could need some work
;;;;    
;;;;    This is particularly important when used with repeatitions:
;;;;    
;;;;        (match '(I ate (!c amount) (!v fruit) (!* and (!c amount) (!v fruit)))
;;;;               '(I ate 3 apples and 3 apples))
;;;;        --> ((fruit . apples) (amount . 3))
;;;;    
;;;;        (match '(I ate (!c amount) (!v fruit) (!* and (!c amount) (!v fruit)))
;;;;               '(I ate 3 apples and 5 apples))
;;;;        --> (:failed (:different nil (and 5 apples)) (fruit . apples) (amount . 3))
;;;;    
;;;;        (match '(I ate (!c amount) (!v fruit) (!* and (!c amount) (!v fruit)))
;;;;               '(I ate 3 apples and 3 bananas))
;;;;        --> (:failed (:different nil (and 3 bananas)) (fruit . apples) (amount . 3))
;;;;    
;;;;        (match '(I ate (!* (!c amount) (!v fruit) and) nothing.)
;;;;               '(I ate 3 apples and 3 apples and nothing.))
;;;;        --> ((fruit . apples) (amount . 3))
;;;;    
;;;;        (match '(I ate (!* (!c amount) (!v fruit) and) nothing.)
;;;;               '(I ate 3 apples and 3 bananas and nothing.))
;;;;        --> (:failed (:different nothing\. 3))
;;;;    
;;;;    To be able to match such repeatitions, you will have to use anonymous
;;;;    tokens, and give a name to the whole list: ::
;;;;    
;;;;        (match '(I ate (!n list (!* !ac !av and)) nothing.)
;;;;               '(I ate 3 apples and 3 bananas and nothing.))
;;;;        --> ((list 3 apples and 3 bananas and))
;;;;    
;;;;    
;;;;
;;;;    
;;;;    Using bindings to build new s-expressions
;;;;    -----------------------------------------
;;;;    
;;;;    The function `instanciate` takes a list of bindings as returned by a
;;;;    successful `match`, and insert them in a template.  A template is a
;;;;    s-expressions, which may contain sub-expressions of the following
;;;;    forms:
;;;;    
;;;;    - (:! variable)
;;;;    - (:@ variable)
;;;;    - (:! expression)
;;;;    
;;;;    `(:! variable)` is substituted by the value of the variable in the bindings.
;;;;    
;;;;    `(:@ variable)` is substituted by the value of the variable in the
;;;;    bindings, it must be a list, and it is slit in-line like ,@ in backquote.
;;;;    
;;;;    `(:! expression)`, when expression is not a variable in the given
;;;;    bindings, is evaluated with eval, after having been processed for :!
;;;;    and :@ in it.
;;;;    
;;;;    ::
;;;;    
;;;;        (let ((things (if (zerop (random 2))
;;;;                          'fruits
;;;;                          'calories)))
;;;;          (instanciate (match '((!c apples) apples and (!c oranges) oranges)
;;;;                              '(42 apples and 3 oranges))
;;;;                       '((:! apples) apples and (:! oranges) oranges
;;;;                         are (:! (+ (:! apples) (:! oranges))) (:! things))))
;;;;        --> (42 apples and 3 oranges are 45 fruits) ; or
;;;;        --> (42 apples and 3 oranges are 45 calories)
;;;;    
;;;;    See also the following section.
;;;;    
;;;;    
;;;;    `match-case`
;;;;    -----------
;;;;    
;;;;    Another way to use the bindings, is using the `match-case` macro.  It
;;;;    takes a s-expression, and a list of clauses which have a pattern and a
;;;;    body.  If the pattern matches the s-expression, then it binds the
;;;;    variables as lisp variables, and evaluates the body in a lexical
;;;;    context where these lisp variables have the values of the matched
;;;;    sub-expressions.
;;;;    
;;;;    ::
;;;;    
;;;;        (let ((expr '(add 42 to 21)))
;;;;          (match-case expr
;;;;                      ((add       (!x a) to   (!x b)) `(+ ,a ,b))
;;;;                      ((multiply  (!x a) with (!x b)) `(* ,a ,b))
;;;;                      ((substract (!x a) from (!x a)) 0)
;;;;                      (otherwise                      :error)))
;;;;        --> (+ 42 21)
;;;;    
;;;;        (let ((expr '(multiply 42 with (* 3 7))))
;;;;          (match-case expr
;;;;                      ((add       (!x a) to   (!x b)) `(+ ,a ,b))
;;;;                      ((multiply  (!x a) with (!x b)) `(* ,a ,b))
;;;;                      ((substract (!x a) from (!x a)) 0)
;;;;                      (otherwise                      :error)))
;;;;        --> (* 42 (* 3 7))
;;;;    
;;;;    This can be used to build new s-expressions with backquote and splice,
;;;;    or to do any other processing required.
;;;;    
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-07 <PJB> Converted from Common Lisp.  We use ! instead of ?
;;;;                     since ? is used in emacs for characters :-(
;;;;    2003-12-17 <PJB> Created.
;;;;BUGS
;;;;    Pattern matcher and instantiation doesn't work with arrays/matrices,
;;;;    structures...
;;;;
;;;;    The reason sublists would need some work to make them more
;;;;    useful in some cases.
;;;;
;;;;    The alternative !/ is not implemented yet.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************
(require 'cl)


(defun* make-match-state (&key dict)
  "Make a match state from a dictionary.
Current implementation mrerely return the dictionary."
  dict)

(defun* match-state-dict (ms)
  "Return the dictionary of the match state ms."
  ms)

(defun* match-state-failed-p (ms)
  "
RETURN: Whether the match failed."
  (eql (first (match-state-dict ms)) :failed))

(defun* match-state-fail     (ms raison)
  "PRIVATE
Return a new match-state with a dictionary containing the associations
of the ms p-list, and with an entry at :failed for reason."
  (make-match-state :dict  (list* :failed raison (match-state-dict ms))))

(defun* match-state-retry    (ms)
  "PRIVATE
Return a match-state like ms without the :failed entry." 
  (if (match-state-failed-p ms)
      (make-match-state :dict (cddr (match-state-dict ms)))
      ms))


(defun* match-dict-map (ms function)
  "
DO:     Calls FUNCTION (lambda (symbol value) ...) with all successive bindings,
        (unless matching state is failed).
RETURN: The list of results of the FUNCTION.
"
  (lexical-let ()
    (unless (match-state-failed-p ms)
      (mapcar (lambda (binding) (funcall function (first binding) (second binding)))
              (match-state-dict ms)))))


(defun* match-dict-add-binding (ms pat exp)
  "PRIVATE
Returns a dictionnary with an added binding,
or ms if the binding is already present or ms was failed,
or a failed ms if the binading was present but different."
  (unless (match-state-failed-p ms)
   (lexical-let* ((var (second (car pat)))
                  (val (car exp))
                  (ass (assoc var (match-state-dict ms))))
     (cond
       ((null ass)                      ; a new binding:
        (make-match-state :dict (acons var val (match-state-dict ms))))
       ((equalp (cdr ass) val)          ; already there, same
        ms)
       (t                               ; already there, different
        (match-state-fail ms `(:mismatching-binding ,ass ,val)))))))


(defmacro defpattern (name pattern)
  "Define a predicate named name that indicates whether an expression
is a pattern expression."
  (if (listp pattern)
      `(defun* ,name (pat) ;; ((!n ...)...)
         ,(format "Test whether the argument is a ((%s ...) ...) pattern." (first pattern))
         (and (listp pat) (listp (car pat)) (symbolp (caar pat))
              (eql ',(car pattern) (caar pat))))
      `(defun* ,name (pat) ;; (!ac ...)
         ,(format "Test whether the argument is a (%s ...) pattern." pattern)
         (and (listp pat) (symbolp (car pat))
              (eql ',pattern (car pat))))))


(defpattern pat-anonymous-variable-p   !av)
(defpattern pat-anonymous-constant-p   !ac)
(defpattern pat-anonymous-expression-p !ax)
(defpattern pat-named-p                (!n))
(defpattern pat-variable-p             (!v))
(defpattern pat-constant-p             (!c))
(defpattern pat-expression-p           (!x))
(defpattern pat-optional-p             (!!))
(defpattern pat-repeat-p               (!+))
(defpattern pat-optional-repeat-p      (!*))
(defpattern pat-alternative-p          (!/)) ; not implemented yet.
(defpattern pat-squeleton-eval-p       :!)


(defun* exp-variable-p (exp)
  "PRIVATE"
  (and (consp exp)
       (symbolp   (car exp))))

(defun* exp-constant-p (exp)
  "PRIVATE"
  (and (consp exp)
       (atom (car exp))
       (not (symbolp (car exp)))))


;; (!n n !v) == (!v n)

;; (match '((!+ a b c)) '(a b c a b c))


;; pattern ::= term | ( pattern-seq ) .
;; pattern-seq ::= | pattern pattern-seq .
;; pattern-lst ::= | pattern pattern-lst .
;;
;; term      ::= !av | !ac | !ax                    -- anonymous terms
;;             | (!v name) | (!c name) | (!x name)  -- named terms
;;             | (!n name pattern-seq)              -- named sequence
;;             | (!! pattern-seq )                  -- optional sequence
;;             | (!+ pattern-seq )                  -- repeat sequence
;;             | (!* pattern-seq )                  -- optional repeat sequence
;;             | (!/ pattern-lst )                  -- alternative
;;             | atom | compound .
;;
;; name     ::= symbol .
;;
;; atom     ::= symbol | string | character | number .
;; compound ::= list | array | structure .
;; list     ::= ( pattern-seq ) .
;; array    ::= #A( pattern-seq )
;;            | #1A( pattern-seq )
;;            | #2A( (pattern-seq)... )
;;            | #3A( ((pattern-seq)...)... )
;;            | ... .

(defun* generate-all-follows (exp)
  "
RETURN:     A list of possible follows from shortest to longuest.
EXAMPLE:    (generate-all-follows '(a b c d))
            --> (((d c b a) ())
                 ((c b a) (d))
                 ((b a) (c d))
                 ((a) (b c d))
                 (() (a b c d)))
"
  (do ((rest  exp (cdr rest)) ;; what should match after
       (list  '())            ;; what we match (reversed),
       ;;             we reverse only in the final result [F].
       (frame '()))
      ((null rest)
       (push (list list rest) frame)
       frame)
    (push (list list rest) frame)
    (push (car rest) list)))


(defun* match (pat exp &optional (ms (make-match-state)))
  "
DO:        A pattern matcher accepting the following syntax:
             !av        expects a symbol (variable).
             !ac        expects a constant (non symbol atom).
             !ax        expects anything (one item).
             (!v n)     expects a symbol (variable)     and bind it.
             (!c n)     expects a constant (non symbol atom)  and bind it.
             (!x n)     expects anything (one item)     and bind it.
             (!n n ...) expects anything (several item) and bind them.
             (!+ ...)   expects anything (one or more times).  AOB
             (!* ...)   expects anything (zero or more times). AOB
             (!! ...)   expects anything (zero or one time).
             ...        expects exactly ... (can be a sublist).
           AOB = All occurences bind.
RETURN:    A match-state structure.
SEE ALSO:  match-state-failed-p to check if the matching failed.
           match-state-dict     to get the binding dictionary.
"
  ;; The pattern and the expression may be atoms or lists,
  ;; but usually we process (car pat) and (car exp), to be able
  ;; to match several items (!+ and !*).
  (cond
    ((match-state-failed-p ms) ms)

    ((pat-anonymous-variable-p pat)
     (if (exp-variable-p exp)
         (match (cdr pat) (cdr exp) ms)
         (match-state-fail ms `(:not-variable ,exp))))

    ((pat-variable-p pat)
     (if (exp-variable-p exp)
         (match (cdr pat) (cdr exp) (match-dict-add-binding ms pat exp))
         (match-state-fail ms `(:not-variable ,exp))))

    ((pat-anonymous-constant-p pat)
     (if (exp-constant-p exp)
         (match (cdr pat) (cdr exp) ms)
         (match-state-fail ms `(:not-constant ,exp))))

    ((pat-constant-p pat)
     (if (exp-constant-p exp)
         (match (cdr pat) (cdr exp) (match-dict-add-binding ms pat exp))
         (match-state-fail ms `(:not-constant ,exp))))

    ((pat-anonymous-expression-p pat)
     (if (null exp)
         (match-state-fail ms `(:missing-expression))
         (match (cdr pat) (cdr exp) ms)))
    
    ((pat-expression-p pat)
     (if (null exp)
         (match-state-fail ms `(:missing-expression))
         (match (cdr pat) (cdr exp) (match-dict-add-binding ms pat exp)) ))

    ((pat-named-p pat)
     (loop
        for (list rest) in (generate-all-follows exp)
        for soe = (match (cdr pat) rest ms)
        for nms = (if (match-state-failed-p soe)
                      soe
                      (lexical-let* ((list (reverse list))
                                     (nms (match (cddar pat) list soe)))
                        (if (match-state-failed-p nms)
                            nms
                            (match-dict-add-binding nms pat (list list)))))
        while (match-state-failed-p nms)
        finally (return nms)))
    
    ((and (pat-repeat-p pat) (null exp))
     (match-state-fail ms `(:missing-repeat ,pat)))
    
    ((or (pat-repeat-p pat) (pat-optional-repeat-p pat) (pat-optional-p pat))
     (loop
        for (list rest) in (generate-all-follows exp)
        for soe = (match (cdr pat) rest ms)
        for nms = (if (match-state-failed-p soe)
                      soe
                      (cond
                        ((pat-repeat-p pat)
                         ;; at least one (...2... already matches)
                         ;; ((!+ ...1...) ...2...)
                         ;; --> (...1... (!* ...1...) ...2...)
                         (match (append (cdar pat) (list (cons '!* (cdar pat))))
                                (reverse list) soe))
                        ((pat-optional-repeat-p pat)
                         ;; zero or more (...2... already matches)
                         ;; ((!* ...1...) ...2...)
                         ;; --> (...1... (!* ...1...) ...2...)
                         ;; --> (...2...)
                         (lexical-let ((nms (match (append (cdar pat) (list (car pat)))
                                                   (reverse list) soe)))
                           (if (match-state-failed-p nms)
                               (match nil list soe)
                               nms)))
                        ((pat-optional-p pat)
                         ;; zero or one (...2... already matches)
                         ;; ((!! ...1...) ...2...)
                         ;; --> (...1... ...2...)
                         ;; --> (...2...)
                         (lexical-let ((nms (match  (cdar pat) (reverse list) soe)))
                           (if (match-state-failed-p nms)
                               (match  nil list soe)
                               nms)))))
        while (match-state-failed-p nms)
        finally (return nms)))
    
    ((atom pat)
     (if (equal pat exp)
         ms
         (match-state-fail ms `(:different ,pat ,exp ))))
    
    ((atom exp)
     (match-state-fail ms `(:unexpected-atom ,exp)))
    
    (t ;; both cars are sublists.
     (match (cdr pat) (cdr exp) (match (car pat) (car exp) ms)))))


(defun* evaluate (instance)
  "PRIVATE"
  (cond
    ((atom instance)               instance)
    ((and (atom (car instance)) (eql :! (car instance)))
     (eval (evaluate (second instance))))
    (t (mapcar (function evaluate) instance))))


(defun* dict-value (dict name)
  "PRIVATE"
  (cdr (assoc name dict)))


(defun* dict-boundp (dict name)
  "PRIVATE"
  (and (or (symbolp name) (stringp name))
       (assoc name dict)))


(defun* subst-bindings (expr dict)
  "PRIVATE"
  (lexical-let ()
   (cond
     ((atom expr) (list expr))
     ((and (atom (first expr)) (eql :!  (first expr)))
      (if (and (atom (second expr))
               (dict-boundp dict (second expr)))
          (list (dict-value dict (second expr)))
          (list (mapcan (lambda (subexpr) (subst-bindings subexpr dict)) expr))))
     ((and (atom (first expr)) (eql :@ (first expr)))
      (copy-seq (dict-value dict (second expr))))
     (t (list (mapcan (lambda (subexpr) (subst-bindings subexpr dict))
                      expr))))))


(defun* instanciate (ms skeleton)
  "PRIVATE
PRE:   (not (match-state-failed-p ms))
DO:    Instanciate the skeleton, substituting all occurence of (:! var)
       with the value bound to var in the binding dictionary of MS,
       Occurences of (:@ var) are split in line like ,@ in backquotes.
       Then all remaining (:! form) are evaluated (with eval) from the
       deepest first.
"
  (assert (not (match-state-failed-p ms)))
  (evaluate (first (subst-bindings skeleton (match-state-dict ms)))))



(defun* collect-variables (pat)
  "
PAT:       A symbolic expression with the following syntax:
             (!v v)  expects a symbol (variable).
             (!c c)  expects a constant (non symbol atom).
             (!x x)  expects anything (one item).
             (!+ l)  expects anything (one or more items).
             (!* l)  expects anything (zero or more items).
             other   expects exactly other (can be a sublist).
RETURN:    A list of the symbol used in the various (!. sym) items, 
           in no particular order, but with duplicates deleted.
"
  (delete-duplicates
   (cond
     ((atom pat)
      nil)
     ((and (atom (car pat))
           (member* (car pat) '(!v !c !x !+ !*) :test (function eql)))
      (list (cadr pat)))
     (t
      (nconc (collect-variables (car pat)) (collect-variables (cdr pat)))))))


(defmacro match-case (sexp &rest clauses)
  "
SEXP:    A symbolic expression, evaluated.
CLAUSES: A list of (pattern &body body)
         The pattern must be litteral. 
         Lexical variable names are extracted from it, and body is executed
         in a lexical environment where these names are bound to the matched
         sub-expression of SEXP.
DO:      Execute the body of the clause whose pattern matches the SEXP,
         or whose pattern is a symbol string-equal to OTHERWISE.
EXAMPLE: (match-case expr
            ((add       (!x a) to   (!x b)) `(+ ,a ,b))
            ((multiply  (!x a) with (!x b)) `(* ,a ,b))
            ((substract (!x a) from (!x a)) 0)
            (otherwise                      :error))
"
  (with-gensyms (ex ms dc)
    `(lexical-let ((,ex ,sexp) (,ms) (,dc))
       (cond
         ,@(mapcar
            (lambda (clause)
              (lexical-let ((pat (car clause)) (body (cdr clause)))
                (if (and (symbolp pat) (eql 'otherwise pat))
                    `(t ,@body)
                    `((progn (setf ,ms (match ',pat ,ex))
                             (not (match-state-failed-p ,ms)))
                      (setf ,dc (match-state-dict ,ms))
                      (lexical-let ( ,@(mapcar
                                        (lambda (name) `(,name (cdr (assoc ',name ,dc)))) 
                                        (collect-variables pat)) )
                        ,@body))))) clauses)))))







(defun* run-tests (tests &key verbose)
  (loop
     for (expression expected-result &optional error) in tests
     do (let* ((got-error nil)
               (result (handler-case (eval expression) 
                         (error (err) (setf got-error err)))))
          (when verbose
            (message (format "test:   %S" expression))
            (message (format "expect: %S" expected-result))
            (message (format "got:    %S" expected-result)))
          (if got-error
              (if error
                  t
                  (error got-error))
              (assert (equalp result expected-result)
                      (expression result expected-result)
                      "Expression %S gave result %S instead of expected %S"
                      expression result expected-result))
          (when verbose
            (message (format "success!" expected-result))))
     finally (return :success)))


(defun test/pmatch/!v ()
  (run-tests '(((match '(begin (!n e (!! (!v a))) end)
                 '(begin x end))
                ((e x) (a . x)))

               ((match '(begin (!n e (!! (!v a))) end)
                 '(begin end))
                ((e)))

               ((match '(begin (!! (!v a)) end)
                 '(begin end))
                ())

               ((match '(begin (!! (!v a)) end)
                 '(begin x end))
                ((a . x)))

               ((match '(begin (!! (!v a)) end)
                 '(begin 42 end))
                (:failed (:different end 42))))))

(defun test/pmatch/!* ()
  (run-tests '(((match '(begin (!n e (!* !av)) end)
                 '(begin x y z end))
                ((e x y z)))

               ((match '(begin (!n e (!* (!v a))) end)
                 '(begin x x x end))
                ((e x x x) (a . x)))

               ((match '(begin (!n e (!* (!v a))) end)
                 '(begin x y z end))
                (:failed (:different end x)))

               ((match '(begin (!n e (!* (!v a))) end)
                 '(begin x end))
                ((e x) (a . x)))

               ((match '(begin (!n e (!* (!v a))) end)
                 '(begin end))
                ((e))))))

(defun test/match-case ()
  (run-tests '(((let ((expr '(add (multiply 2 with x) to 1)))
                  (match-case expr
                              ((add       (!x a) to   (!x b)) `(+ ,a ,b))
                              ((multiply  (!x a) with (!x b)) `(* ,a ,b))
                              ((substract (!x a) from (!x a)) 0)
                              (otherwise                      :error)))
                (+ (multiply 2 with x) 1)))))

(defun test/pmatch-various ()
  (run-tests  '(((match '(test (!v v) (!c c) (!x x) (!+ !av) (!* !ac))
                  '(test var 42 whatever v1 v2 v3 1 2 3 4 5))
                 ((x . whatever) (c . 42) (v . var))))))


(defun test/examples ()
  (run-tests '(((match '(!av eats an !av)
                 '(John eats an apple))
                nil)

               ((match '(!av eats an !av)
                 '(John eats a banana))
                (:failed (:different an a)))

               ((match '(!av eats an !av)
                 '((a big elephant) eats an apple))
                (:failed (:not-variable ((a big elephant) eats an apple))))

               ((match '(!av eats an !av)
                 '(42 eats an apple))
                (:failed (:not-variable (42 eats an apple))))

               ((match '(I ate !ac kg of apples)
                 '(I ate 42 kg of apples))
                nil)

               ((match '(I ate !ac kg of apples)
                 '(I ate (* 21 2) kg of apples))
                (:failed (:not-constant ((* 21 2) kg of apples))))

               ((match '(I ate !ac kg of apples)
                 '(I ate two kg of apples))
                (:failed (:not-constant (two kg of apples))))

               ((match '(I ate !ax kg of apples)
                 '(I ate 42 kg of apples))
                nil)

               ((match '(I ate !ax kg of apples)
                 '(I ate (* 21 2) kg of apples))
                nil)

               ((match '(I ate !ax kg of apples)
                 '(I ate two kg of apples))
                nil)

               ((match '(I ate (!+ and ate))
                 '(I ate))
                (:failed (:missing-repeat ((!+ and ate)))))

               ((match '(I ate (!+ and ate))
                 '(I ate and ate))
                nil)

               ((match '(I ate (!+ and ate))
                 '(I ate and ate and ate))
                nil)

               ((match '(I ate (!* and ate))
                 '(I ate))
                nil)

               ((match '(I ate (!* and ate))
                 '(I ate and ate))
                nil)

               ((match '(I ate (!* and ate))
                 '(I ate and ate and ate))
                nil)

               ((match '(I ate (!! and ate))
                 '(I ate))
                nil)

               ((match '(I ate (!! and ate))
                 '(I ate and ate))
                nil)

               ((match '(I ate (!! and ate))
                 '(I ate and ate and ate))
                (:failed (:different nil (and ate and ate))))

               ((match '(I ate (!* and !av))
                 '(I ate and ran and sat))
                nil)

               ((match '(I ate (!* and !av))
                 '(I ate and ran or sat))
                (:failed (:different nil (and ran or sat))))

               ((match '(I ate (!* and !av))
                 '(I ate and (also ran) and sat))
                (:failed (:different nil (and (also ran) and sat))))

               ((match '((!v name) eats an (!v fruit))
                 '(John eats an apple))
                ((fruit . apple) (name . John)))

               ((match '((!v name) eats an (!v fruit))
                 '(John eats a banana))
                (:failed (:different an a) (name . John)))

               ((match '((!v name) eats an (!v fruit))
                 '(John eats an 8))
                (:failed (:not-variable (8)) (name . John)))

               ((match '(I ate (!c amount) kg of apples)
                 '(I ate 42 kg of apples))
                ((amount . 42)))

               ((match '(I ate (!c amount) kg of apples)
                 '(I ate (* 21 2) kg of apples))
                (:failed (:not-constant ((* 21 2) kg of apples))))

               ((match '(I ate (!c amount))
                 '(I ate two kg of apples))
                (:failed (:not-constant (two kg of apples))))

               ((match '(I ate (!x amount) kg of apples)
                 '(I ate 42 kg of apples))
                ((amount . 42)))

               ((match '(I ate (!x amount) kg of apples)
                 '(I ate (* 21 2) kg of apples))
                ((amount * 21 2)))

               ((match '(I ate (!x amount) kg of apples)
                 '(I ate two kg of apples))
                ((amount . two)))

               ((match '(I ate (!n amount-with-unit !ac !av) of apples)
                 '(I ate 42 kg of apples))
                ((amount-with-unit 42 kg)))

               ((match '(I ate (!n amount-with-unit !ac !av) of apples
                         and (!n amount-with-unit !ac !av) of bananas)
                 '(I ate 42 kg of apples and 42 kg of bananas))
                ((amount-with-unit 42 kg)))

               ((match '(I ate (!n amount-with-unit !ac !av) of apples
                         and (!n amount-with-unit !ac !av) of bananas)
                 '(I ate 42 kg of apples and 3 kg of bananas))
                (:failed (:different of 42)))

               ((match '(I ate (!c amount) (!v fruit) (!* and (!c amount) (!v fruit)))
                 '(I ate 3 apples and 3 apples))
                ((fruit . apples) (amount . 3)))

               ((match '(I ate (!c amount) (!v fruit) (!* and (!c amount) (!v fruit)))
                 '(I ate 3 apples and 5 apples))
                (:failed (:different nil (and 5 apples)) (fruit . apples) (amount . 3)))

               ((match '(I ate (!c amount) (!v fruit) (!* and (!c amount) (!v fruit)))
                 '(I ate 3 apples and 3 bananas))
                (:failed (:different nil (and 3 bananas)) (fruit . apples) (amount . 3)))

               ((match '(I ate (!* (!c amount) (!v fruit) and) nothing.)
                 '(I ate 3 apples and 3 apples and nothing.))
                ((fruit . apples) (amount . 3)))

               ((match '(I ate (!* (!c amount) (!v fruit) and) nothing.)
                 '(I ate 3 apples and 3 bananas and nothing.))
                (:failed (:different nothing\. 3)))

               ((match '(I ate (!n list (!* !ac !av and)) nothing.)
                 '(I ate 3 apples and 3 bananas and nothing.))
                ((list 3 apples and 3 bananas and)))

               ((let ((things 'fruits))
                  (instanciate (match '((!c apples) apples and (!c oranges) oranges)
                                      '(42 apples and 3 oranges))
                               '((:! apples) apples and (:! oranges) oranges
                                 are (:! (+ (:! apples) (:! oranges))) (:! things))))
                (42 apples and 3 oranges are 45 fruits))

               ((let ((expr '(add 42 to 21)))
                  (match-case expr
                              ((add       (!x a) to   (!x b)) `(+ ,a ,b))
                              ((multiply  (!x a) with (!x b)) `(* ,a ,b))
                              ((substract (!x a) from (!x a)) 0)
                              (otherwise                      :error)))
                (+ 42 21))

               ((let ((expr '(multiply 42 with (* 3 7))))
                  (match-case expr
                              ((add       (!x a) to   (!x b)) `(+ ,a ,b))
                              ((multiply  (!x a) with (!x b)) `(* ,a ,b))
                              ((substract (!x a) from (!x a)) 0)
                              (otherwise                      :error)))
                (* 42 (* 3 7)))

               ((let ((expr '(multiply 42 and (* 3 7))))
                  (match-case expr
                              ((add       (!x a) to   (!x b)) `(+ ,a ,b))
                              ((multiply  (!x a) with (!x b)) `(* ,a ,b))
                              ((substract (!x a) from (!x a)) 0)
                              (otherwise                      :error)))
                :error))
             :verbose nil))


(assert (every (lambda (x) (eq :success x))
               (list (test/pmatch/!v)
                     (test/pmatch/!*)
                     (test/match-case)
                     (test/pmatch-various)
                     (test/examples))))

(provide 'pjb-pmatch)
;;;; THE END ;;;;
