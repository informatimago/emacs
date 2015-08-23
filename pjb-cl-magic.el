;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;****************************************************************************
;;FILE:               pjb-cl-magic.el
;;LANGUAGE:           emacs lisp
;;SYSTEM:             POSIX
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    Automagically insert parenthesis while typing Common-Lisp code.
;;
;;    There are 338 functions, macros or special operators in COMMON-LISP
;;    that have a fixed number of arguments, therefore for which
;;    parenthesis can be deduced automatically.
;;
;;    When a space is typed, the current sexps is analysed, and parenthesis
;;    are added before the point accordingly to what is known of the 
;;    Common-Lisp syntax. 
;;    In addition, the expected argument is indicated in the mini-buffer. 
;;
;;    Usage with slime:  (byte-compile-file "pjb-cl-magic.el")
;;                       (load-file "pjb-cl-magic.elc")
;;                       (setf *pjb-cl-magic-case*   (function identity-region)
;;                             *pjb-cl-magic-indent* nil)
;;
;;    To use with another common-lisp mode, add to your common-lisp mode hook:
;;                       (local-set-key  " " 'pjb-cl-magic-space)
;;                
;;    This is a proof-of-concept.  There remains quite a number of
;;    missing features (see BUGS below). 
;;
;;    More sophisticated or complete handling would need to parse the
;;    whole file or the whole project sources, (or worse, introspect
;;    an inferior-lisp where the already typed in code would have
;;    been REPLed, like in Slime). 
;;    This would be impractical (having to patch (or hook) a lot of
;;    emacs input, or too slow (here, we have to parse again the current
;;    sexps every time a space is typed).
;;    
;;    My conclusion is that it would be better to have a synthesizer 
;;    editor, (which could be implemented in emacs or hemlock).
;;    
;;AUTHORS
;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2004-11-01 <PJB> Commented.
;;    2004-10-18 <PJB> Created.
;;BUGS
;;    - the names of this file and its entry point ("magic") are weak.
;;
;;    - It only knows about symbols from the COMMON-LISP package.
;;
;;    - It does not handle nicely open strings, comments (#|...|#) or 
;;      or even inserting spaces inside a string (you'd have to type C-q SPC).
;;
;;    - It infinite loops when  a space is typed inside a |xxx symbol...
;;
;;    - It does not insert destructured parenthesis for macros:
;;      typing:
;;           with-open-file SPC
;;      should give:
;;        in buffer:      (with-open-file (
;;        in mini-buffer: WITH-OPEN-FILE:  STREAM
;;
;;    - Some macro or special operators have a syntax that is not reflected
;;      in their lambda-list. For example, typing:
;;           cond null x SPC
;;      should give:
;;           (COND ((NULL x)
;;      instead of:
;;           (COND (NULL x)
;;
;;    - Help is botched when there are keywords, the indexing is wrong.
;;
;;    - No provision for :ALLOW-OTHER-KEYS T argument,
;;      only for :ALLOW-OTHER-KEYS parameter.
;;
;;    - No analysis of lexical bindings, so typing:
;;          defun x (list) (car list
;;      gives:
;;          (DEFUN x (list) (CAR (LIST 
;;      instead of:
;;          (DEFUN x (list) (CAR list)
;;      (but of course, the former could be what is wanted...)
;;
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal Bourguignon 2004 - 2004
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************
(provide 'pjb-cl-magic)
(eval-when-compile (require 'pjb-cl))   ; for the macros...
(require 'pjb-cl)
(require 'pjb-sources)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WARNING:     Case is significant in emacs lisp!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;----------------------------------------------------------------------
;; User customization:
;;----------------------------------------------------------------------


(defvar *readtable-case* (function string-upcase)
  "*A function used to change the case of the symbols accordingly
 to the (READTABLE-CASE *READTABLE*):
    :UPCASE   --> STRING-UPCASE
    :DOWNCASE --> STRING-DOWNCASE
    :PRESERVE --> IDENTITY
    :INVERT   --> STRING-INVERTCASE
 It is used in the lookup of the symbols in the tables.")


(defvar *pjb-cl-magic-case*  (function upcase-region)
  "*The case change applied to the function symbol in the source.
     :UPCASE   --> upcase-region
     :DOWNCASE --> downcase-region
     :PRESERVE --> identity-region
     :INVERT   --> invertcase-region
               --> capitalize-region")


(defvar *pjb-cl-magic-indent* t
  "*Whether lisp-indent-line should be called when a SPC is typed.")


;;----------------------------------------------------------------------
;; Debugging:
;;----------------------------------------------------------------------

(eval-when-compile
  (defconst *debugging-situations* '(:invariant :processing :trace-ossn) "
:invariant   check invariant (slow)
:processing  trace processing.
:trace-ossn  dump ossn while checking invariant.
"))


(defmacro when-debugging (situation &rest body)
  (when (intersection situation *debugging-situations*)
    `(progn ,@body)))


;;----------------------------------------------------------------------

(defun* string-invertcase (string)
  (map-into (make-string* (length string))
            (lambda (ch)
              (cond
                ((upper-case-p ch) (char-downcase ch))
                ((lower-case-p ch) (char-upcase   ch))
                (t                 ch))) string))


(defun* identity-region (beg end) (interactive))


(defun* invertcase-region (beg end)
  (interactive "r")
  (let ((inverted (string-invertcase (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (insert inverted)))

;;----------------------------------------------------------------------

(defstruct lambda-list
  "Cached data about a lambda-list. 
The purpose is to be able to determine rapidely what to do when a SPC is typed
after a COMMON-LISP symbol.
- symbol, kind and lambda-list are the raw data coming from *raw-lambda-lists*
  they're not used.
- cnt-mandatory:      the number of mandatory arguments.
- cnt-optional:       the number of mandatory arguments.
- rest-p:             whether &REST or &BODY parameters are present.
- allow-other-keys-p: whether &ALLOW-OTHER-KEYS is present.
- keys:               list of keyword names.
- help:               an a-list used to generate the help. 
                      Use the lambda-list-get-help to get the help string!
- actions:            a vector of action functions. Use the lambda-list-action
                      function to get the right action!
"
  (symbol             nil :type symbol)
  (kind               nil :type symbol)
  (lambda-list             nil :type list)
  (cnt-mandatory      0   :type integer)
  (cnt-optional       0   :type integer)
  (rest-p             nil :type boolean)
  (allow-other-keys-p nil :type boolean)
  (keys               nil :type list)
  (help               '() :type list)
  (actions            []  :type vector))



(defparameter *cl-lambda-lists* nil
  "A hash table mapping COMMON-LISP symbols to lambda-list structures.
This table is built from the *raw-lambda-lists* list 
SEE: load-lambda-lists.")


(defun* get-lambda-list-of (symbol)
  (gethash (funcall *readtable-case* (cl:string symbol)) *cl-lambda-lists*))


(defun* lambda-list-compute-actions (lambda-list)
  "
DO:      Update (lambda-list-actions lambda-list) with a vector containing the
         functions to apply when an argument of the corresponding index
         is entered.
NOTE:    Call lambda-list-action to get the correct action!
"
  ;; (print (lambda-list-symbol lambda-list))
  (let ((actions (make-array (list (+ (lambda-list-cnt-mandatory lambda-list)
                                      (lambda-list-cnt-optional lambda-list)
                                      (* 2 (length (lambda-list-keys lambda-list)))
                                      1))
                             :element-type 'function
                             :initial-element (function lambda-list-close)))
        (base 0))
    (dotimes (i (+ (lambda-list-cnt-mandatory lambda-list)
                   (lambda-list-cnt-optional lambda-list)))
      (setf (aref actions (+ base i)) (function lambda-list-accept)))
    (incf base (+ (lambda-list-cnt-mandatory lambda-list)
                  (lambda-list-cnt-optional lambda-list)))
    (dotimes (i (length (lambda-list-keys lambda-list)))
      (setf (aref actions (+ base (* 2 i)))
            (function lambda-list-check-current-keyword)
            (aref actions (+ base (* 2 i) 1))
            (function lambda-list-check-previous-keyword)))
    (incf base (* 2 (length (lambda-list-keys lambda-list))))
    (setf (aref actions base)  (if (lambda-list-rest-p lambda-list)
                                   (function lambda-list-accept)
                                   (function lambda-list-close)))
    (setf (lambda-list-actions lambda-list) actions)))


;; sexp            length   action
;; (char             1      [0]=accept 
;; (char arg         2      [0]=accept [1]=accept 
;; (char arg arg     3      [0]=accept [1]=accept [2]=close  


(defun* lambda-list-action (lambda-list sexp-length)
  (let ((max  (1- (length (lambda-list-actions lambda-list)))))
    (cond ((and (<= 0 sexp-length) (< sexp-length max))
           (aref  (lambda-list-actions lambda-list) (1- sexp-length)))
          ((lambda-list-allow-other-keys-p lambda-list)
           (when-debugging (:processing)
             (message "slen=%d max=%d odd=%S" 
                      sexp-length max (oddp (- sexp-length max))))
           (if (oddp (- sexp-length max))
               (function lambda-list-check-current-keyword)
               (function lambda-list-check-previous-keyword)))
          (t
           (aref (lambda-list-actions lambda-list) max)))))


(defparameter +cl-lambda-list-keywords+
  '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &BODY &WHOLE &ENVIRONMENT))


(defparameter +cl-lambda-list-kinds+
  '(:ordinary :macro :method-combination :boa :generic :specialized
    :defsetf :deftype :destructuring :modify-macro))


(defun* split-lambda-list-on-keywords (lambda-list lambda-list-kind)
  "
lambda-list-kind:  (member +cl-lambda-list-kinds+)
"
  (let ((sing-result '())
        (env (position '&ENVIRONMENT lambda-list)))
    (when env
      (push (list '&ENVIRONMENT (elt lambda-list (1+ env))) sing-result)
      (setf lambda-list (remove-if (lambda (x) t) lambda-list :start env :end (+ env 2))))
    (when (eq '&WHOLE (first lambda-list))
      (push (subseq lambda-list 0 2) sing-result)
      (setf lambda-list (cddr lambda-list)))
    (do ((llk '(&MANDATORY &OPTIONAL &KEY &ALLOW-OTHER-KEYS &AUX &REST &BODY))
         (args (if (member (first lambda-list) +cl-lambda-list-keywords+)
                   lambda-list
                   (cons '&MANDATORY lambda-list))
               (cdr args))
         (chunk '())
         (result '()))
        ((null args)
         (when chunk (push (nreverse chunk) result))
         (nreverse (nconc sing-result result)))
      (if (member (car args) llk)
          (progn
            (when chunk (push (nreverse chunk) result))
            (setf chunk (list (car args))))
          (push (car args) chunk)))))


(defun get-split-parameters (keyword split-lambda-list)
  "
keyword:  (member (cons '&MANDATORY +cl-lambda-list-keywords+))
RETURN:   The selected list of arguments.
"
  (cdar (member* keyword split-lambda-list
                :key (function first))))


(defun parameter-name (parameter)
  "
RETURN: The name of the parameter (the variable that will be bound).
"
  (etypecase parameter
    (symbol parameter)
    (list   (etypecase (first parameter)
              (symbol  (first parameter))
              (list    (second (first parameter)))))))


(defun parameter-keyword (parameter)
  "
RETURN: The keyword used to introduce an argument for this parameter.
"
  (etypecase parameter
    (symbol (intern* (cl:string parameter) "KEYWORD"))
    (list   (etypecase (first parameter)
              (symbol  (intern* (cl:string (first parameter)) "KEYWORD"))
              (list    (first (first parameter)))))))


(defun parameter-specializer (parameter)
  "
RETURN: The specializer of the parameter (the expression used to initalize
        an optional or keyword parameter).
"
  (etypecase parameter
    (symbol (values nil nil))
    (list   (if (cdr parameter)
                (values (second parameter) t)
                (values nil nil)))))


(defun parameter-init-form (parameter)
  "
RETURN: The init-form of the parameter (the expression used to initalize
        an optional or keyword parameter).
"
  (etypecase parameter
    (symbol (values nil nil))
    (list   (if (cdr parameter)
                (values (second parameter) t)
                (values nil nil)))))


(defun parameter-indicator (parameter)
  "
RETURN: The name of the indicator variable for the parameter (the variable
        that is bound to true or NIL to indicate whether the value of the
        optional or keyword parameter comes from an argument or the init-form).
"
  (etypecase parameter
    (symbol (values nil nil))
    (list   (if (cddr parameter)
                (values (third parameter) t)
                (values nil nil)))))


;; ordinary-lambda-list::= (var*
;;         [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;         [&rest var]
;;         [&key {var | ({var | (keyword-name var)}
;;                       [init-form [supplied-p-parameter]])}*
;;               [&allow-other-keys]]
;;         [&aux {var | (var [init-form])}*])

;; generic-lambda-list::= (var*
;;         [&optional {var | (var)}*]
;;         [&rest var]
;;         [&key {var | ({var | (keyword-name var)})}* [&allow-other-keys]])

;; specialized-lambda-list::= ({var | (var [specializer])}*
;;         [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;         [&rest var]
;;         [&key {var | ({var | (keyword-name var)}
;;                       [init-form [supplied-p-parameter]])}*
;;               [&allow-other-keys]]
;;         [&aux {var | (var [init-form])}*])

;; reqvars::= var*
;;
;; optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;
;; restvar::= [{&rest | &body} var]
;;
;; keyvars::= [&key {var | ({var | (keyword-name var)}
;;                          [init-form [supplied-p-parameter]])}*
;;                  [&allow-other-keys]]
;;
;; auxvars::= [&aux {var | (var [init-form])}*]
;;
;; envvar::= [&environment var]
;;
;; wholevar::= [&whole var]
;;
;; macro-lambda-list::= (wholevar envvar  reqvars envvar  optvars envvar
;;                       restvar envvar  keyvars envvar  auxvars envvar)
;;                 | (wholevar envvar  reqvars envvar  optvars envvar .  var)
;;
;; pattern::= (wholevar reqvars optvars restvar keyvars auxvars) |
;;            (wholevar reqvars optvars . var)

;; destructuring-lambda-list::= (wholevar reqvars optvars restvar keyvars auxvars)
;;                       | (wholevar reqvars optvars . var)

;; defsetf-lambda-list::= (var*
;;         [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;         [&rest var]
;;         [&key {var | ({var | (keyword-name var)}
;;                       [init-form [supplied-p-parameter]])}*
;;               [&allow-other-keys]]
;;         [&environment var]


(defun* lambda-list-get-help (lambda-list effective-argument-list)
  (let ((curhelp (elt (lambda-list-help lambda-list)
                      (1- (length effective-argument-list))))
        m)
    ;; TODO: This is botched when there are keywords, the index is wrong!
    (format "%s:  %s"
      (lambda-list-symbol lambda-list)
      (case (car curhelp)
        ((:mandatory :optional) (cdr curhelp))
        ((:key)
         (apply (function concatenate) 'string
                (mapcar (lambda (x) (format " [%s]" x))
                        (nconc
                         (set-difference
                          (mapcar (function cdr)
                                  (delete-if-not (lambda (x) (eq :key (car x)))
                                                 (lambda-list-help lambda-list)))
                          effective-argument-list
                          :test (function STRING-EQUAL))
                         (member* '&ALLOW-OTHER-KEYS (lambda-list-help lambda-list))))))
        (otherwise
         (cond
           ((setf m (member* :allow-other-keys (lambda-list-help lambda-list)
                             :key (function car))) (cdar m))
           ((setf m (member* :rest             (lambda-list-help lambda-list)
                             :key (function car))) (cdar m))
           ((setf m (member* :body             (lambda-list-help lambda-list)
                             :key (function car))) (cdar m))
           (t "-- complete")))))))


(defun* make-help-from-split-lambda-list (split-lambda-list)
  (append
   ;; mandatory:
   (mapcar (lambda (arg) (cons :mandatory (format "%s" 
                                            (if (consp arg) 
                                                arg
                                                (parameter-name arg)))))
           (get-split-parameters '&MANDATORY split-lambda-list))
   ;; optional:
   (mapcar (lambda (arg) (cons :optional (format "[%s]" (parameter-name arg))))
           (get-split-parameters '&OPTIONAL split-lambda-list))
   ;; keywords:
   (mapcar (lambda (arg) (cons :key (format "%s" (parameter-keyword arg))))
           (get-split-parameters '&KEY split-lambda-list))
   (let (m)
     (cond
       ((setf m (get-split-parameters '&ALLOW-OTHER-KEYS split-lambda-list))
        (list (cons :allow-other-keys "other keys allowed...")))
       ((setf m (get-split-parameters '&REST split-lambda-list))
        (list (cons :rest (format "%s..." (parameter-name (first m))))))
       ((setf m (get-split-parameters '&BODY split-lambda-list))
        (list (cons :body (format "%s..." (parameter-name (first m))))))
       (t nil)))))


(defvar *raw-lambda-lists* 
  (mapcar
   (lambda (item) (nsubst nil 'NIL item))
   '(
     (:FUNCTION * (&REST ARGS)) 
     (:FUNCTION + (&REST ARGS)) 
     (:FUNCTION - (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION / (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION /= (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION 1+ (NUMBER)) 
     (:FUNCTION 1- (NUMBER)) 
     (:FUNCTION < (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION <= (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION = (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION > (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION >= (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION ABORT (&OPTIONAL CONDITION)) 
     (:FUNCTION ABS (NUMBER)) 
     (:FUNCTION ACONS (KEY DATUM ALIST)) 
     (:FUNCTION ACOS (NUMBER)) 
     (:FUNCTION ACOSH (NUMBER)) 
     (:GENERIC ADD-METHOD (GENERIC-FUNCTION METHOD)) 
     (:FUNCTION ADJOIN (ITEM LIST &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION ADJUST-ARRAY (ARRAY DIMENSIONS &KEY (ELEMENT-TYPE (ARRAY-ELEMENT-TYPE ARRAY)) (INITIAL-ELEMENT NIL INITIAL-ELEMENT-P) (INITIAL-CONTENTS NIL INITIAL-CONTENTS-P) FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)) 
     (:FUNCTION ADJUSTABLE-ARRAY-P (ARRAY)) 
     (:GENERIC ALLOCATE-INSTANCE (CLASS &REST INITARGS)) 
     (:FUNCTION ALPHA-CHAR-P (CHAR)) 
     (:FUNCTION ALPHANUMERICP (CHAR)) 
     (:MACRO AND (&REST FORMS)) 
     (:FUNCTION APPEND (&REST LISTS)) 
     (:FUNCTION APPLY (FUNCTION ARG &REST ARGUMENTS)) 
     (:FUNCTION APROPOS (STRING-DESIGNATOR &OPTIONAL PACKAGE EXTERNAL-ONLY)) 
     (:FUNCTION APROPOS-LIST (STRING-DESIGNATOR &OPTIONAL PACKAGE-DESIGNATOR EXTERNAL-ONLY)) 
     (:FUNCTION AREF (ARRAY &REST SUBSCRIPTS)) 
     (:FUNCTION ARITHMETIC-ERROR-OPERANDS (CONDITION)) 
     (:FUNCTION ARITHMETIC-ERROR-OPERATION (CONDITION)) 
     (:FUNCTION ARRAY-DIMENSION (ARRAY AXIS-NUMBER)) 
     (:FUNCTION ARRAY-DIMENSIONS (ARRAY)) 
     (:FUNCTION ARRAY-DISPLACEMENT (ARRAY)) 
     (:FUNCTION ARRAY-ELEMENT-TYPE (ARRAY)) 
     (:FUNCTION ARRAY-HAS-FILL-POINTER-P (ARRAY)) 
     (:FUNCTION ARRAY-IN-BOUNDS-P (ARRAY &REST SUBSCRIPTS)) 
     (:FUNCTION ARRAY-RANK (ARRAY)) 
     (:FUNCTION ARRAY-ROW-MAJOR-INDEX (ARRAY &REST SUBSCRIPTS)) 
     (:FUNCTION ARRAY-TOTAL-SIZE (ARRAY)) 
     (:FUNCTION ARRAYP (OBJECT)) 
     (:FUNCTION ASH (INTEGER COUNT)) 
     (:FUNCTION ASIN (NUMBER)) 
     (:FUNCTION ASINH (NUMBER)) 
     (:MACRO ASSERT (TEST-FORM &OPTIONAL PLACES DATUM &REST ARGUMENTS)) 
     (:FUNCTION ASSOC (ITEM ALIST &KEY KEY (TEST NIL TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION ASSOC-IF (PREDICATE ALIST &KEY KEY)) 
     (:FUNCTION ASSOC-IF-NOT (PREDICATE ALIST &KEY KEY)) 
     (:FUNCTION ATAN (Y &OPTIONAL (X NIL XP))) 
     (:FUNCTION ATANH (NUMBER)) 
     (:FUNCTION ATOM (OBJECT)) 
     (:FUNCTION BIT (BIT-ARRAY &REST SUBSCRIPTS)) 
     (:FUNCTION BIT-AND (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-ANDC1 (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-ANDC2 (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-EQV (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-IOR (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-NAND (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-NOR (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-NOT (BIT-ARRAY &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-ORC1 (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-ORC2 (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:FUNCTION BIT-VECTOR-P (OBJECT)) 
     (:FUNCTION BIT-XOR (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)) 
     (:SPECIAL-OPERATOR BLOCK (NAME &REST FORMS)) 
     (:FUNCTION BOOLE (OP INTEGER1 INTEGER2)) 
     (:FUNCTION BOTH-CASE-P (CHAR)) 
     (:FUNCTION BOUNDP (SYMBOL)) 
     (:FUNCTION BREAK (&OPTIONAL (DATUM "break") &REST ARGUMENTS)) 
     (:FUNCTION BROADCAST-STREAM-STREAMS (INSTANCE)) 
     (:FUNCTION BUTLAST (LIST &OPTIONAL (N 1))) 
     (:FUNCTION BYTE (SIZE POSITION)) 
     (:FUNCTION BYTE-POSITION (BYTESPEC)) 
     (:FUNCTION BYTE-SIZE (BYTESPEC)) 
     (:FUNCTION CAAAAR (LIST)) 
     (:FUNCTION CAAADR (LIST)) 
     (:FUNCTION CAAAR (LIST)) 
     (:FUNCTION CAADAR (LIST)) 
     (:FUNCTION CAADDR (LIST)) 
     (:FUNCTION CAADR (LIST)) 
     (:FUNCTION CAAR (LIST)) 
     (:FUNCTION CADAAR (LIST)) 
     (:FUNCTION CADADR (LIST)) 
     (:FUNCTION CADAR (LIST)) 
     (:FUNCTION CADDAR (LIST)) 
     (:FUNCTION CADDDR (LIST)) 
     (:FUNCTION CADDR (LIST)) 
     (:FUNCTION CADR (LIST)) 
     (:MACRO CALL-METHOD (&REST ARGS)) 
     (:FUNCTION CAR (LIST)) 
     (:MACRO CASE (KEYFORM &BODY CASES)) 
     (:SPECIAL-OPERATOR CATCH (TAG &BODY BODY)) 
     (:MACRO CCASE (KEYFORM &BODY CASES)) 
     (:FUNCTION CDAAAR (LIST)) 
     (:FUNCTION CDAADR (LIST)) 
     (:FUNCTION CDAAR (LIST)) 
     (:FUNCTION CDADAR (LIST)) 
     (:FUNCTION CDADDR (LIST)) 
     (:FUNCTION CDADR (LIST)) 
     (:FUNCTION CDAR (LIST)) 
     (:FUNCTION CDDAAR (LIST)) 
     (:FUNCTION CDDADR (LIST)) 
     (:FUNCTION CDDAR (LIST)) 
     (:FUNCTION CDDDAR (LIST)) 
     (:FUNCTION CDDDDR (LIST)) 
     (:FUNCTION CDDDR (LIST)) 
     (:FUNCTION CDDR (LIST)) 
     (:FUNCTION CDR (LIST)) 
     (:FUNCTION CEILING (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION CELL-ERROR-NAME (CONDITION)) 
     (:FUNCTION CERROR (CONTINUE-STRING DATUM &REST ARGUMENTS)) 
     (:GENERIC CHANGE-CLASS (INSTANCE NEW-CLASS &REST INITARGS)) 
     (:FUNCTION CHAR (STRING INDEX)) 
     (:FUNCTION CHAR-CODE (CHAR)) 
     (:FUNCTION CHAR-DOWNCASE (CHAR)) 
     (:FUNCTION CHAR-EQUAL (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR-GREATERP (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR-INT (CHAR)) 
     (:FUNCTION CHAR-LESSP (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR-NAME (CHAR)) 
     (:FUNCTION CHAR-NOT-EQUAL (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR-NOT-GREATERP (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR-NOT-LESSP (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR-UPCASE (CHAR)) 
     (:FUNCTION CHAR/= (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR< (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR<= (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR= (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR> (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHAR>= (CHARACTER &REST MORE-CHARACTERS)) 
     (:FUNCTION CHARACTER (OBJECT)) 
     (:FUNCTION CHARACTERP (OBJECT)) 
     (:MACRO CHECK-TYPE (PLACE TYPE &OPTIONAL TYPE-STRING)) 
     (:FUNCTION CIS (THETA)) 
     (:GENERIC CLASS-NAME (CLASS)) 
     (:FUNCTION CLASS-OF (X)) 
     (:FUNCTION CLEAR-INPUT (&OPTIONAL (STREAM *STANDARD-INPUT*))) 
     (:FUNCTION CLEAR-OUTPUT (&OPTIONAL (STREAM *STANDARD-OUTPUT*))) 
     (:GENERIC CLOSE (STREAM &KEY ABORT)) 
     (:FUNCTION CLRHASH (HASH-TABLE)) 
     (:FUNCTION CODE-CHAR (CODE)) 
     (:FUNCTION COERCE (OBJECT OUTPUT-TYPE-SPEC)) 
     (:FUNCTION COMPILE (NAME &OPTIONAL (DEFINITION (OR (MACRO-FUNCTION NAME) (FDEFINITION NAME))))) 
     (:FUNCTION COMPILE-FILE (INPUT-FILE &KEY (OUTPUT-FILE (CFP-OUTPUT-FILE-DEFAULT INPUT-FILE)) ((VERBOSE *COMPILE-VERBOSE*) *COMPILE-VERBOSE*) ((PRINT *COMPILE-PRINT*) *COMPILE-PRINT*) (EXTERNAL-FORMAT DEFAULT) (TRACE-FILE NIL) ((BLOCK-COMPILE *BLOCK-COMPILE-ARG*) NIL))) 
     (:FUNCTION COMPILE-FILE-PATHNAME (INPUT-FILE &KEY (OUTPUT-FILE (CFP-OUTPUT-FILE-DEFAULT INPUT-FILE)) &ALLOW-OTHER-KEYS)) 
     (:FUNCTION COMPILED-FUNCTION-P (OBJECT)) 
     (:FUNCTION COMPILER-MACRO-FUNCTION (NAME &OPTIONAL ENV)) 
     (:FUNCTION COMPLEMENT (FUNCTION)) 
     (:FUNCTION COMPLEX (REALPART &OPTIONAL (IMAGPART 0))) 
     (:FUNCTION COMPLEXP (OBJECT)) 
     (:GENERIC COMPUTE-APPLICABLE-METHODS (GENERIC-FUNCTION ARGUMENTS)) 
     (:FUNCTION COMPUTE-RESTARTS (&OPTIONAL CONDITION)) 
     (:FUNCTION CONCATENATE (OUTPUT-TYPE-SPEC &REST SEQUENCES)) 
     (:FUNCTION CONCATENATED-STREAM-STREAMS (INSTANCE)) 
     (:MACRO COND (&REST CLAUSES)) 
     (:FUNCTION CONJUGATE (NUMBER)) 
     (:FUNCTION CONS (SE1 SE2)) 
     (:FUNCTION CONSP (OBJECT)) 
     (:FUNCTION CONSTANTLY (VALUE)) 
     (:FUNCTION CONSTANTP (OBJECT &OPTIONAL ENVIRONMENT)) 
     (:FUNCTION CONTINUE (&OPTIONAL CONDITION)) 
     (:FUNCTION COPY-ALIST (ALIST)) 
     (:FUNCTION COPY-LIST (LIST)) 
     (:FUNCTION COPY-PPRINT-DISPATCH (&OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*))) 
     (:FUNCTION COPY-READTABLE (&OPTIONAL (FROM-READTABLE *READTABLE*) TO-READTABLE)) 
     (:FUNCTION COPY-SEQ (SEQUENCE)) 
     (:FUNCTION COPY-STRUCTURE (STRUCTURE)) 
     (:FUNCTION COPY-SYMBOL (SYMBOL &OPTIONAL (COPY-PROPS NIL) &AUX NEW-SYMBOL)) 
     (:FUNCTION COPY-TREE (OBJECT)) 
     (:FUNCTION COS (NUMBER)) 
     (:FUNCTION COSH (NUMBER)) 
     (:FUNCTION COUNT (ITEM SEQUENCE &KEY FROM-END (START 0) (END NIL) (KEY NIL) (TEST (FUNCTION EQL) TEST-P) (TEST-NOT NIL TEST-NOT-P))) 
     (:FUNCTION COUNT-IF (PRED SEQUENCE &KEY FROM-END (START 0) (END NIL) (KEY NIL))) 
     (:FUNCTION COUNT-IF-NOT (PRED SEQUENCE &KEY FROM-END (START 0) (END NIL) (KEY NIL))) 
     (:MACRO CTYPECASE (KEYFORM &BODY CASES)) 
     (:MACRO DECF (G299 &OPTIONAL (DELTA 1) &ENVIRONMENT G298)) 
     (:MACRO DECLAIM (&REST SPECS)) 
     (:FUNCTION DECODE-FLOAT (F)) 
     (:FUNCTION DECODE-UNIVERSAL-TIME (UNIVERSAL-TIME &OPTIONAL TIME-ZONE)) 
     (:MACRO DEFCLASS (&ENVIRONMENT ENV NAME %DIRECT-SUPERCLASSES %DIRECT-SLOTS &REST %OPTIONS)) 
     (:MACRO DEFCONSTANT (NAME VALUE &OPTIONAL DOCUMENTATION)) 
     (:MACRO DEFGENERIC (FUN-NAME LAMBDA-LIST &BODY OPTIONS)) 
     (:MACRO DEFINE-COMPILER-MACRO (NAME LAMBDA-LIST &BODY BODY)) 
     (:MACRO DEFINE-CONDITION (NAME (&REST PARENT-TYPES) (&REST SLOT-SPECS) &BODY OPTIONS)) 
     (:MACRO DEFINE-METHOD-COMBINATION (&WHOLE FORM &REST ARGS)) 
     (:MACRO DEFINE-MODIFY-MACRO (NAME LAMBDA-LIST FUNCTION &OPTIONAL DOC-STRING)) 
     (:MACRO DEFINE-SETF-EXPANDER (ACCESS-FN LAMBDA-LIST &BODY BODY)) 
     (:MACRO DEFINE-SYMBOL-MACRO (NAME EXPANSION)) 
     (:MACRO DEFMACRO (NAME LAMBDA-LIST &REST BODY)) 
     (:MACRO DEFMETHOD (&REST ARGS &ENVIRONMENT ENV)) 
     (:MACRO DEFPACKAGE (PACKAGE &REST OPTIONS)) 
     (:MACRO DEFPARAMETER (VAR VAL &OPTIONAL (DOC NIL DOCP))) 
     (:MACRO DEFSETF (ACCESS-FN &REST REST)) 
     (:MACRO DEFSTRUCT (NAME-AND-OPTIONS &REST SLOT-DESCRIPTIONS)) 
     (:MACRO DEFTYPE (NAME ARGLIST &BODY BODY)) 
     (:MACRO DEFUN (&ENVIRONMENT ENV NAME ARGS &BODY BODY)) 
     (:MACRO DEFVAR (VAR &OPTIONAL (VAL NIL VALP) (DOC NIL DOCP))) 
     (:FUNCTION DELETE (ITEM SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:FUNCTION DELETE-DUPLICATES (SEQUENCE &KEY (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0) (END NIL) FROM-END (KEY NIL))) 
     (:FUNCTION DELETE-FILE (FILE)) 
     (:FUNCTION DELETE-IF (PREDICATE SEQUENCE &KEY FROM-END (START 0) (KEY NIL) (END NIL) (COUNT NIL))) 
     (:FUNCTION DELETE-IF-NOT (PREDICATE SEQUENCE &KEY FROM-END (START 0) (END NIL) (KEY NIL) (COUNT NIL))) 
     (:FUNCTION DELETE-PACKAGE (PACKAGE-OR-NAME)) 
     (:FUNCTION DENOMINATOR (NUMBER)) 
     (:FUNCTION DEPOSIT-FIELD (NEWBYTE BYTESPEC INTEGER)) 
     (:FUNCTION DESCRIBE (X &OPTIONAL (STREAM-DESIGNATOR *STANDARD-OUTPUT*))) 
     (:GENERIC DESCRIBE-OBJECT (PACKAGE STREAM)) 
     (:MACRO DESTRUCTURING-BIND (LAMBDA-LIST ARG-LIST &REST BODY)) 
     (:FUNCTION DIGIT-CHAR (WEIGHT &OPTIONAL (RADIX 10))) 
     (:FUNCTION DIGIT-CHAR-P (CHAR &OPTIONAL (RADIX 10))) 
     (:FUNCTION DIRECTORY (PATHNAME &KEY)) 
     (:FUNCTION DIRECTORY-NAMESTRING (PATHNAME)) 
     (:FUNCTION DISASSEMBLE (OBJECT &KEY (STREAM *STANDARD-OUTPUT*) (USE-LABELS T))) 
     (:MACRO DO (VARLIST ENDLIST &BODY BODY)) 
     (:MACRO DO* (VARLIST ENDLIST &BODY BODY)) 
     (:MACRO DO-ALL-SYMBOLS ((VAR &OPTIONAL RESULT-FORM) &BODY BODY-DECLS)) 
     (:MACRO DO-EXTERNAL-SYMBOLS ((VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM) &BODY BODY-DECLS)) 
     (:MACRO DO-SYMBOLS ((VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM) &BODY BODY-DECLS)) 
     (:GENERIC DOCUMENTATION (SLOTD DOC-TYPE)) 
     (:MACRO DOLIST ((VAR LIST &OPTIONAL (RESULT NIL)) &BODY BODY)) 
     (:MACRO DOTIMES ((VAR COUNT &OPTIONAL (RESULT NIL)) &BODY BODY)) 
     (:FUNCTION DPB (NEWBYTE BYTESPEC INTEGER)) 
     (:FUNCTION DRIBBLE (&OPTIONAL PATHNAME &KEY (IF-EXISTS APPEND))) 
     (:MACRO ECASE (KEYFORM &BODY CASES)) 
     (:FUNCTION ECHO-STREAM-INPUT-STREAM (INSTANCE)) 
     (:FUNCTION ECHO-STREAM-OUTPUT-STREAM (INSTANCE)) 
     (:FUNCTION ED (&OPTIONAL X)) 
     (:FUNCTION EIGHTH (LIST)) 
     (:FUNCTION ELT (SEQUENCE INDEX)) 
     (:FUNCTION ENCODE-UNIVERSAL-TIME (SECOND MINUTE HOUR DATE MONTH YEAR &OPTIONAL TIME-ZONE)) 
     (:FUNCTION ENDP (OBJECT)) 
     (:FUNCTION ENOUGH-NAMESTRING (PATHNAME &OPTIONAL (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))) 
     (:FUNCTION ENSURE-DIRECTORIES-EXIST (PATHSPEC &KEY VERBOSE (MODE 511))) 
     (:FUNCTION ENSURE-GENERIC-FUNCTION (FUN-NAME &REST ALL-KEYS &KEY ENVIRONMENT &ALLOW-OTHER-KEYS)) 
     (:FUNCTION EQ (OBJ1 OBJ2)) 
     (:FUNCTION EQL (OBJ1 OBJ2)) 
     (:FUNCTION EQUAL (X Y)) 
     (:FUNCTION EQUALP (X Y)) 
     (:FUNCTION ERROR (DATUM &REST ARGUMENTS)) 
     (:MACRO ETYPECASE (KEYFORM &BODY CASES)) 
     (:FUNCTION EVAL (ORIGINAL-EXP)) 
     (:SPECIAL-OPERATOR EVAL-WHEN (SITUATIONS &REST FORMS)) 
     (:FUNCTION EVENP (NUMBER)) 
     (:FUNCTION EVERY (PRED FIRST-SEQ &REST MORE-SEQS)) 
     (:FUNCTION EXP (NUMBER)) 
     (:FUNCTION EXPORT (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION EXPT (BASE POWER)) 
     (:FUNCTION FBOUNDP (NAME)) 
     (:FUNCTION FCEILING (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION FDEFINITION (NAME)) 
     (:FUNCTION FFLOOR (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION FIFTH (LIST)) 
     (:FUNCTION FILE-AUTHOR (FILE)) 
     (:FUNCTION FILE-ERROR-PATHNAME (CONDITION)) 
     (:FUNCTION FILE-LENGTH (STREAM)) 
     (:FUNCTION FILE-NAMESTRING (PATHNAME)) 
     (:FUNCTION FILE-POSITION (STREAM &OPTIONAL POSITION)) 
     (:FUNCTION FILE-STRING-LENGTH (STREAM OBJECT)) 
     (:FUNCTION FILE-WRITE-DATE (FILE)) 
     (:FUNCTION FILL (SEQUENCE ITEM &KEY (START 0) (END NIL))) 
     (:FUNCTION FILL-POINTER (VECTOR)) 
     (:FUNCTION FIND (ITEM SEQUENCE &KEY FROM-END (START 0) END KEY TEST TEST-NOT)) 
     (:FUNCTION FIND-ALL-SYMBOLS (STRING-OR-SYMBOL)) 
     (:FUNCTION FIND-CLASS (SYMBOL &OPTIONAL (ERRORP T) ENVIRONMENT)) 
     (:FUNCTION FIND-IF (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY)) 
     (:FUNCTION FIND-IF-NOT (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY)) 
     (:GENERIC FIND-METHOD (GENERIC-FUNCTION QUALIFIERS SPECIALIZERS &OPTIONAL (ERRORP T))) 
     (:FUNCTION FIND-PACKAGE (PACKAGE-DESIGNATOR)) 
     (:FUNCTION FIND-RESTART (NAME &OPTIONAL CONDITION)) 
     (:FUNCTION FIND-SYMBOL (NAME &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION FINISH-OUTPUT (&OPTIONAL (STREAM *STANDARD-OUTPUT*))) 
     (:FUNCTION FIRST (LIST)) 
     (:SPECIAL-OPERATOR FLET (DEFINITIONS &BODY BODY)) 
     (:FUNCTION FLOAT (NUMBER &OPTIONAL (OTHER NIL OTHERP))) 
     (:FUNCTION FLOAT-DIGITS (F)) 
     (:FUNCTION FLOAT-PRECISION (F)) 
     (:FUNCTION FLOAT-RADIX (X)) 
     (:FUNCTION FLOAT-SIGN (FLOAT1 &OPTIONAL (FLOAT2 (FLOAT 1 FLOAT1)))) 
     (:FUNCTION FLOATP (OBJECT)) 
     (:FUNCTION FLOOR (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION FMAKUNBOUND (NAME)) 
     (:FUNCTION FORCE-OUTPUT (&OPTIONAL (STREAM *STANDARD-OUTPUT*))) 
     (:FUNCTION FORMAT (DESTINATION CONTROL-STRING &REST FORMAT-ARGUMENTS)) 
     (:MACRO FORMATTER (CONTROL-STRING)) 
     (:FUNCTION FOURTH (LIST)) 
     (:FUNCTION FRESH-LINE (&OPTIONAL (STREAM *STANDARD-OUTPUT*))) 
     (:FUNCTION FROUND (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION FTRUNCATE (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION FUNCALL (FUNCTION &REST ARGUMENTS)) 
     (:SPECIAL-OPERATOR FUNCTION (THING)) 
     (:GENERIC FUNCTION-KEYWORDS (METHOD)) 
     (:FUNCTION FUNCTION-LAMBDA-EXPRESSION (FUN)) 
     (:FUNCTION FUNCTIONP (OBJECT)) 
     (:FUNCTION GCD (&REST NUMBERS)) 
     (:FUNCTION GENSYM (&OPTIONAL (THING "G"))) 
     (:FUNCTION GENTEMP (&OPTIONAL (PREFIX "T") (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION GET (SYMBOL INDICATOR &OPTIONAL (DEFAULT NIL))) 
     (:FUNCTION GET-DECODED-TIME NIL) 
     (:FUNCTION GET-DISPATCH-MACRO-CHARACTER (DISP-CHAR SUB-CHAR &OPTIONAL (RT *READTABLE*))) 
     (:FUNCTION GET-INTERNAL-REAL-TIME NIL) 
     (:FUNCTION GET-INTERNAL-RUN-TIME NIL) 
     (:FUNCTION GET-MACRO-CHARACTER (CHAR &OPTIONAL (READTABLE *READTABLE*))) 
     (:FUNCTION GET-OUTPUT-STREAM-STRING (STREAM)) 
     (:FUNCTION GET-PROPERTIES (PLACE INDICATOR-LIST)) 
     (:FUNCTION GET-SETF-EXPANSION (FORM &OPTIONAL ENVIRONMENT)) 
     (:FUNCTION GET-UNIVERSAL-TIME NIL) 
     (:FUNCTION GETF (PLACE INDICATOR &OPTIONAL (DEFAULT NIL))) 
     (:FUNCTION GETHASH (KEY HASH-TABLE &OPTIONAL DEFAULT)) 
     (:SPECIAL-OPERATOR GO (TAG)) 
     (:FUNCTION GRAPHIC-CHAR-P (CHAR)) 
     (:MACRO HANDLER-BIND (BINDINGS &BODY FORMS)) 
     (:MACRO HANDLER-CASE (FORM &REST CASES)) 
     (:FUNCTION HASH-TABLE-COUNT (HASH-TABLE)) 
     (:FUNCTION HASH-TABLE-P (OBJECT)) 
     (:FUNCTION HASH-TABLE-REHASH-SIZE (INSTANCE)) 
     (:FUNCTION HASH-TABLE-REHASH-THRESHOLD (INSTANCE)) 
     (:FUNCTION HASH-TABLE-SIZE (HASH-TABLE)) 
     (:FUNCTION HASH-TABLE-TEST (INSTANCE)) 
     (:FUNCTION HOST-NAMESTRING (PATHNAME)) 
     (:FUNCTION IDENTITY (THING)) 
     (:SPECIAL-OPERATOR IF (TEST THEN &OPTIONAL ELSE)) 
     (:MACRO IGNORE-ERRORS (&REST FORMS)) 
     (:FUNCTION IMAGPART (NUMBER)) 
     (:FUNCTION IMPORT (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:MACRO IN-PACKAGE (PACKAGE-DESIGNATOR)) 
     (:MACRO INCF (G280 &OPTIONAL (DELTA 1) &ENVIRONMENT G279)) 
     (:GENERIC INITIALIZE-INSTANCE (INSTANCE &REST INITARGS)) 
     (:GENERIC INPUT-STREAM-P (NON-STREAM)) 
     (:FUNCTION INSPECT (OBJECT)) 
     (:FUNCTION INTEGER-DECODE-FLOAT (X)) 
     (:FUNCTION INTEGER-LENGTH (INTEGER)) 
     (:FUNCTION INTEGERP (OBJECT)) 
     (:FUNCTION INTERACTIVE-STREAM-P (STREAM)) 
     (:FUNCTION INTERN (NAME &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION INTERSECTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION INVALID-METHOD-ERROR (METHOD FORMAT-CONTROL &REST FORMAT-ARGUMENTS)) 
     (:FUNCTION INVOKE-DEBUGGER (CONDITION)) 
     (:FUNCTION INVOKE-RESTART (RESTART &REST VALUES)) 
     (:FUNCTION INVOKE-RESTART-INTERACTIVELY (RESTART)) 
     (:FUNCTION ISQRT (N)) 
     (:FUNCTION KEYWORDP (OBJECT)) 
     (:SPECIAL-OPERATOR LABELS (DEFINITIONS &BODY BODY)) 
     (:MACRO LAMBDA (&WHOLE WHOLE ARGS &BODY BODY)) 
     (:FUNCTION LAST (LIST &OPTIONAL (N 1))) 
     (:FUNCTION LCM (&REST NUMBERS)) 
     (:FUNCTION LDB (BYTESPEC INTEGER)) 
     (:FUNCTION LDB-TEST (BYTESPEC INTEGER)) 
     (:FUNCTION LDIFF (LIST OBJECT)) 
     (:FUNCTION LENGTH (SEQUENCE)) 
     (:SPECIAL-OPERATOR LET (BINDINGS &BODY BODY)) 
     (:SPECIAL-OPERATOR LET* (BINDINGS &BODY BODY)) 
     (:FUNCTION LISP-IMPLEMENTATION-TYPE NIL) 
     (:FUNCTION LISP-IMPLEMENTATION-VERSION NIL) 
     (:FUNCTION LIST (&REST ARGS)) 
     (:FUNCTION LIST* (ARG &REST OTHERS)) 
     (:FUNCTION LIST-ALL-PACKAGES NIL) 
     (:FUNCTION LIST-LENGTH (LIST)) 
     (:FUNCTION LISTEN (&OPTIONAL (STREAM *STANDARD-INPUT*))) 
     (:FUNCTION LISTP (OBJECT)) 
     (:FUNCTION LOAD (FILESPEC &KEY (VERBOSE *LOAD-VERBOSE*) (PRINT *LOAD-PRINT*) (IF-DOES-NOT-EXIST T) (EXTERNAL-FORMAT DEFAULT))) 
     (:FUNCTION LOAD-LOGICAL-PATHNAME-TRANSLATIONS (HOST)) 
     (:SPECIAL-OPERATOR LOAD-TIME-VALUE (FORM &OPTIONAL READ-ONLY-P)) 
     (:SPECIAL-OPERATOR LOCALLY (&BODY BODY)) 
     (:FUNCTION LOG (NUMBER &OPTIONAL (BASE NIL BASE-P))) 
     (:FUNCTION LOGAND (&REST INTEGERS)) 
     (:FUNCTION LOGANDC1 (INTEGER1 INTEGER2)) 
     (:FUNCTION LOGANDC2 (INTEGER1 INTEGER2)) 
     (:FUNCTION LOGBITP (INDEX INTEGER)) 
     (:FUNCTION LOGCOUNT (INTEGER)) 
     (:FUNCTION LOGEQV (&REST INTEGERS)) 
     (:FUNCTION LOGICAL-PATHNAME (PATHSPEC)) 
     (:FUNCTION LOGICAL-PATHNAME-TRANSLATIONS (HOST)) 
     (:FUNCTION LOGIOR (&REST INTEGERS)) 
     (:FUNCTION LOGNAND (INTEGER1 INTEGER2)) 
     (:FUNCTION LOGNOR (INTEGER1 INTEGER2)) 
     (:FUNCTION LOGNOT (NUMBER)) 
     (:FUNCTION LOGORC1 (INTEGER1 INTEGER2)) 
     (:FUNCTION LOGORC2 (INTEGER1 INTEGER2)) 
     (:FUNCTION LOGTEST (INTEGER1 INTEGER2)) 
     (:FUNCTION LOGXOR (&REST INTEGERS)) 
     (:FUNCTION LONG-SITE-NAME NIL) 
     (:MACRO LOOP (&ENVIRONMENT ENV &REST KEYWORDS-AND-FORMS)) 
     (:MACRO LOOP-FINISH NIL) 
     (:FUNCTION LOWER-CASE-P (CHAR)) 
     (:FUNCTION MACHINE-INSTANCE NIL) 
     (:FUNCTION MACHINE-TYPE NIL) 
     (:FUNCTION MACHINE-VERSION NIL) 
     (:FUNCTION MACRO-FUNCTION (SYMBOL &OPTIONAL ENV)) 
     (:FUNCTION MACROEXPAND (FORM &OPTIONAL ENV)) 
     (:FUNCTION MACROEXPAND-1 (FORM &OPTIONAL ENV)) 
     (:SPECIAL-OPERATOR MACROLET (DEFINITIONS &REST BODY)) 
     (:FUNCTION MAKE-ARRAY (DIMENSIONS &KEY (ELEMENT-TYPE T) (INITIAL-ELEMENT NIL INITIAL-ELEMENT-P) (INITIAL-CONTENTS NIL INITIAL-CONTENTS-P) ADJUSTABLE FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)) 
     (:FUNCTION MAKE-BROADCAST-STREAM (&REST STREAMS)) 
     (:FUNCTION MAKE-CONCATENATED-STREAM (&REST STREAMS)) 
     (:FUNCTION MAKE-CONDITION (THING &REST ARGS)) 
     (:FUNCTION MAKE-DISPATCH-MACRO-CHARACTER (CHAR &OPTIONAL (NON-TERMINATING-P NIL) (RT *READTABLE*))) 
     (:FUNCTION MAKE-ECHO-STREAM (INPUT-STREAM OUTPUT-STREAM)) 
     (:FUNCTION MAKE-HASH-TABLE (&KEY (TEST (QUOTE EQL)) (SIZE +MIN-HASH-TABLE-SIZE+) (REHASH-SIZE 1.5) (REHASH-THRESHOLD 1) (WEAK-P NIL))) 
     (:GENERIC MAKE-INSTANCE (CLASS &REST INITARGS)) 
     (:GENERIC MAKE-INSTANCES-OBSOLETE (CLASS)) 
     (:FUNCTION MAKE-LIST (SIZE &KEY INITIAL-ELEMENT)) 
     (:GENERIC MAKE-LOAD-FORM (RANDOM-STATE &OPTIONAL ENVIRONMENT)) 
     (:FUNCTION MAKE-LOAD-FORM-SAVING-SLOTS (OBJECT &KEY SLOT-NAMES ENVIRONMENT)) 
     (:FUNCTION MAKE-PACKAGE (NAME &KEY (USE (QUOTE NIL)) NICKNAMES (INTERNAL-SYMBOLS 10) (EXTERNAL-SYMBOLS 10))) 
     (:FUNCTION MAKE-PATHNAME (&KEY HOST (DEVICE NIL DEVP) (DIRECTORY NIL DIRP) (NAME NIL NAMEP) (TYPE NIL TYPEP) (VERSION NIL VERSIONP) DEFAULTS (CASE LOCAL))) 
     (:FUNCTION MAKE-RANDOM-STATE (&OPTIONAL STATE)) 
     (:FUNCTION MAKE-SEQUENCE (TYPE LENGTH &KEY (INITIAL-ELEMENT NIL IEP))) 
     (:FUNCTION MAKE-STRING (COUNT &KEY (ELEMENT-TYPE (QUOTE CHARACTER)) ((INITIAL-ELEMENT FILL-CHAR)))) 
     (:FUNCTION MAKE-STRING-INPUT-STREAM (STRING &OPTIONAL (START 0) END)) 
     (:FUNCTION MAKE-STRING-OUTPUT-STREAM (&KEY (ELEMENT-TYPE (QUOTE CHARACTER)) &AUX (STRING (MAKE-STRING 40)))) 
     (:FUNCTION MAKE-SYMBOL (STRING)) 
     (:FUNCTION MAKE-SYNONYM-STREAM (SYMBOL)) 
     (:FUNCTION MAKE-TWO-WAY-STREAM (INPUT-STREAM OUTPUT-STREAM)) 
     (:FUNCTION MAKUNBOUND (SYMBOL)) 
     (:FUNCTION MAP (RESULT-TYPE FUNCTION FIRST-SEQUENCE &REST MORE-SEQUENCES)) 
     (:FUNCTION MAP-INTO (RESULT-SEQUENCE FUNCTION &REST SEQUENCES)) 
     (:FUNCTION MAPC (FUNCTION LIST &REST MORE-LISTS)) 
     (:FUNCTION MAPCAN (FUNCTION LIST &REST MORE-LISTS)) 
     (:FUNCTION MAPCAR (FUNCTION LIST &REST MORE-LISTS)) 
     (:FUNCTION MAPCON (FUNCTION LIST &REST MORE-LISTS)) 
     (:FUNCTION MAPHASH (FUNCTION-DESIGNATOR HASH-TABLE)) 
     (:FUNCTION MAPL (FUNCTION LIST &REST MORE-LISTS)) 
     (:FUNCTION MAPLIST (FUNCTION LIST &REST MORE-LISTS)) 
     (:FUNCTION MASK-FIELD (BYTESPEC INTEGER)) 
     (:FUNCTION MAX (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION MEMBER (ITEM LIST &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))) 
     (:FUNCTION MEMBER-IF (TEST LIST &KEY KEY)) 
     (:FUNCTION MEMBER-IF-NOT (TEST LIST &KEY KEY)) 
     (:FUNCTION MERGE (RESULT-TYPE SEQUENCE1 SEQUENCE2 PREDICATE &KEY KEY)) 
     (:FUNCTION MERGE-PATHNAMES (PATHNAME &OPTIONAL (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*) (DEFAULT-VERSION NEWEST))) 
     (:FUNCTION METHOD-COMBINATION-ERROR (FORMAT-CONTROL &REST FORMAT-ARGUMENTS)) 
     (:GENERIC METHOD-QUALIFIERS (METHOD)) 
     (:FUNCTION MIN (NUMBER &REST MORE-NUMBERS)) 
     (:FUNCTION MINUSP (NUMBER)) 
     (:FUNCTION MISMATCH (SEQUENCE1 SEQUENCE2 &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START1 0) (END1 NIL) (START2 0) (END2 NIL) (KEY NIL))) 
     (:FUNCTION MOD (NUMBER DIVISOR)) 
     (:FUNCTION MUFFLE-WARNING (&OPTIONAL CONDITION)) 
     (:MACRO MULTIPLE-VALUE-BIND (VARS VALUE-FORM &BODY BODY)) 
     (:SPECIAL-OPERATOR MULTIPLE-VALUE-CALL (FUN &REST ARGS)) 
     (:MACRO MULTIPLE-VALUE-LIST (VALUE-FORM)) 
     (:SPECIAL-OPERATOR MULTIPLE-VALUE-PROG1 (VALUES-FORM &REST FORMS)) 
     (:MACRO MULTIPLE-VALUE-SETQ (VARS VALUE-FORM)) 
     (:FUNCTION NAME-CHAR (NAME)) 
     (:FUNCTION NAMESTRING (PATHNAME)) 
     (:FUNCTION NBUTLAST (LIST &OPTIONAL (N 1))) 
     (:FUNCTION NCONC (&REST LISTS)) 
     (:FUNCTION NINTERSECTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION NINTH (LIST)) 
     (:GENERIC NO-APPLICABLE-METHOD (GENERIC-FUNCTION &REST ARGS)) 
     (:GENERIC NO-NEXT-METHOD (GENERIC-FUNCTION METHOD &REST ARGS)) 
     (:FUNCTION NOT (OBJECT)) 
     (:FUNCTION NOTANY (PRED FIRST-SEQ &REST MORE-SEQS)) 
     (:FUNCTION NOTEVERY (PRED FIRST-SEQ &REST MORE-SEQS)) 
     (:FUNCTION NRECONC (X Y)) 
     (:FUNCTION NREVERSE (SEQUENCE)) 
     (:FUNCTION NSET-DIFFERENCE (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION NSET-EXCLUSIVE-OR (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))) 
     (:FUNCTION NSTRING-CAPITALIZE (STRING &KEY (START 0) END)) 
     (:FUNCTION NSTRING-DOWNCASE (STRING &KEY (START 0) END)) 
     (:FUNCTION NSTRING-UPCASE (STRING &KEY (START 0) END)) 
     (:FUNCTION NSUBLIS (ALIST TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))) 
     (:FUNCTION NSUBST (NEW OLD TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))) 
     (:FUNCTION NSUBST-IF (NEW TEST TREE &KEY KEY)) 
     (:FUNCTION NSUBST-IF-NOT (NEW TEST TREE &KEY KEY)) 
     (:FUNCTION NSUBSTITUTE (NEW OLD SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT NIL) (END NIL) (COUNT NIL) (KEY NIL) (START 0))) 
     (:FUNCTION NSUBSTITUTE-IF (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:FUNCTION NSUBSTITUTE-IF-NOT (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:FUNCTION NTH (N LIST)) 
     (:MACRO NTH-VALUE (N FORM)) 
     (:FUNCTION NTHCDR (N LIST)) 
     (:FUNCTION NULL (OBJECT)) 
     (:FUNCTION NUMBERP (OBJECT)) 
     (:FUNCTION NUMERATOR (NUMBER)) 
     (:FUNCTION NUNION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION ODDP (NUMBER)) 
     (:FUNCTION OPEN (FILENAME &KEY (DIRECTION INPUT) (ELEMENT-TYPE (QUOTE BASE-CHAR)) (IF-EXISTS NIL IF-EXISTS-GIVEN) (IF-DOES-NOT-EXIST NIL IF-DOES-NOT-EXIST-GIVEN) (EXTERNAL-FORMAT DEFAULT) &AUX (DIRECTION DIRECTION) (IF-DOES-NOT-EXIST IF-DOES-NOT-EXIST) (IF-EXISTS IF-EXISTS))) 
     (:GENERIC OPEN-STREAM-P (NON-STREAM)) 
     (:MACRO OR (&REST FORMS)) 
     (:GENERIC OUTPUT-STREAM-P (NON-STREAM)) 
     (:FUNCTION PACKAGE-ERROR-PACKAGE (CONDITION)) 
     (:FUNCTION PACKAGE-NAME (PACKAGE-DESIGNATOR)) 
     (:FUNCTION PACKAGE-NICKNAMES (X)) 
     (:FUNCTION PACKAGE-SHADOWING-SYMBOLS (X)) 
     (:FUNCTION PACKAGE-USE-LIST (X)) 
     (:FUNCTION PACKAGE-USED-BY-LIST (X)) 
     (:FUNCTION PACKAGEP (OBJECT)) 
     (:FUNCTION PAIRLIS (KEYS DATA &OPTIONAL (ALIST (QUOTE NIL)))) 
     (:FUNCTION PARSE-INTEGER (STRING &KEY (START 0) END (RADIX 10) JUNK-ALLOWED)) 
     (:FUNCTION PARSE-NAMESTRING (THING &OPTIONAL HOST (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*) &KEY (START 0) END JUNK-ALLOWED)) 
     (:FUNCTION PATHNAME (THING)) 
     (:FUNCTION PATHNAME-DEVICE (PATHNAME &KEY (CASE LOCAL))) 
     (:FUNCTION PATHNAME-DIRECTORY (PATHNAME &KEY (CASE LOCAL))) 
     (:FUNCTION PATHNAME-HOST (PATHNAME &KEY (CASE LOCAL))) 
     (:FUNCTION PATHNAME-MATCH-P (IN-PATHNAME IN-WILDNAME)) 
     (:FUNCTION PATHNAME-NAME (PATHNAME &KEY (CASE LOCAL))) 
     (:FUNCTION PATHNAME-TYPE (PATHNAME &KEY (CASE LOCAL))) 
     (:FUNCTION PATHNAME-VERSION (PATHNAME)) 
     (:FUNCTION PATHNAMEP (OBJECT)) 
     (:FUNCTION PEEK-CHAR (&OPTIONAL (PEEK-TYPE NIL) (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)) 
     (:FUNCTION PHASE (NUMBER)) 
     (:FUNCTION PLUSP (NUMBER)) 
     (:MACRO POP (PLACE &ENVIRONMENT ENV)) 
     (:FUNCTION POSITION (ITEM SEQUENCE &KEY FROM-END (START 0) END KEY TEST TEST-NOT)) 
     (:FUNCTION POSITION-IF (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY)) 
     (:FUNCTION POSITION-IF-NOT (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY)) 
     (:FUNCTION PPRINT (OBJECT &OPTIONAL STREAM)) 
     (:FUNCTION PPRINT-DISPATCH (OBJECT &OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*))) 
     (:MACRO PPRINT-EXIT-IF-LIST-EXHAUSTED NIL) 
     (:FUNCTION PPRINT-FILL (STREAM LIST &OPTIONAL (COLON-P T) ATSIGN-P)) 
     (:FUNCTION PPRINT-INDENT (RELATIVE-TO N &OPTIONAL STREAM)) 
     (:FUNCTION PPRINT-LINEAR (STREAM LIST &OPTIONAL (COLON-P T) ATSIGN-P)) 
     (:MACRO PPRINT-LOGICAL-BLOCK ((STREAM-SYMBOL OBJECT &KEY (PREFIX NIL PREFIXP) (PER-LINE-PREFIX NIL PER-LINE-PREFIX-P) (SUFFIX "" SUFFIXP)) &BODY BODY)) 
     (:FUNCTION PPRINT-NEWLINE (KIND &OPTIONAL STREAM)) 
     (:MACRO PPRINT-POP NIL) 
     (:FUNCTION PPRINT-TAB (KIND COLNUM COLINC &OPTIONAL STREAM)) 
     (:FUNCTION PPRINT-TABULAR (STREAM LIST &OPTIONAL (COLON-P T) ATSIGN-P TABSIZE)) 
     (:FUNCTION PRIN1 (OBJECT &OPTIONAL STREAM)) 
     (:FUNCTION PRIN1-TO-STRING (OBJECT)) 
     (:FUNCTION PRINC (OBJECT &OPTIONAL STREAM)) 
     (:FUNCTION PRINC-TO-STRING (OBJECT)) 
     (:FUNCTION PRINT (OBJECT &OPTIONAL STREAM)) 
     (:FUNCTION PRINT-NOT-READABLE-OBJECT (CONDITION)) 
     (:GENERIC PRINT-OBJECT (SELF OUT)) 
     (:MACRO PRINT-UNREADABLE-OBJECT ((OBJECT STREAM &KEY TYPE IDENTITY) &BODY BODY)) 
     (:FUNCTION PROBE-FILE (PATHNAME)) 
     (:FUNCTION PROCLAIM (RAW-FORM)) 
     (:MACRO PROG (VARLIST &BODY BODY-DECLS)) 
     (:MACRO PROG* (VARLIST &BODY BODY-DECLS)) 
     (:MACRO PROG1 (RESULT &BODY BODY)) 
     (:MACRO PROG2 (FORM1 RESULT &BODY BODY)) 
     (:SPECIAL-OPERATOR PROGN (&REST FORMS)) 
     (:SPECIAL-OPERATOR PROGV (VARS VALS &BODY BODY)) 
     (:FUNCTION PROVIDE (MODULE-NAME)) 
     (:MACRO PSETF (&REST ARGS &ENVIRONMENT ENV)) 
     (:MACRO PSETQ (&REST PAIRS)) 
     (:MACRO PUSH (OBJ PLACE &ENVIRONMENT ENV)) 
     (:MACRO PUSHNEW (OBJ PLACE &REST KEYS &ENVIRONMENT ENV)) 
     (:SPECIAL-OPERATOR QUOTE (THING)) 
     (:FUNCTION RANDOM (ARG &OPTIONAL (STATE *RANDOM-STATE*))) 
     (:FUNCTION RANDOM-STATE-P (OBJECT)) 
     (:FUNCTION RASSOC (ITEM ALIST &KEY KEY (TEST NIL TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION RASSOC-IF (PREDICATE ALIST &KEY KEY)) 
     (:FUNCTION RASSOC-IF-NOT (PREDICATE ALIST &KEY KEY)) 
     (:FUNCTION RATIONAL (X)) 
     (:FUNCTION RATIONALIZE (X)) 
     (:FUNCTION RATIONALP (OBJECT)) 
     (:FUNCTION READ (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) (EOF-VALUE NIL) (RECURSIVEP NIL))) 
     (:FUNCTION READ-BYTE (STREAM &OPTIONAL (EOF-ERROR-P T) EOF-VALUE)) 
     (:FUNCTION READ-CHAR (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)) 
     (:FUNCTION READ-CHAR-NO-HANG (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)) 
     (:FUNCTION READ-DELIMITED-LIST (ENDCHAR &OPTIONAL (INPUT-STREAM *STANDARD-INPUT*) RECURSIVE-P)) 
     (:FUNCTION READ-FROM-STRING (STRING &OPTIONAL EOF-ERROR-P EOF-VALUE &KEY (START 0) END PRESERVE-WHITESPACE)) 
     (:FUNCTION READ-LINE (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)) 
     (:FUNCTION READ-PRESERVING-WHITESPACE (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) (EOF-VALUE NIL) (RECURSIVEP NIL))) 
     (:FUNCTION READ-SEQUENCE (SEQ STREAM &KEY (START 0) END)) 
     (:FUNCTION READTABLE-CASE (INSTANCE)) 
     (:FUNCTION READTABLEP (OBJECT)) 
     (:FUNCTION REALP (OBJECT)) 
     (:FUNCTION REALPART (NUMBER)) 
     (:FUNCTION REDUCE (FUNCTION SEQUENCE &KEY (KEY NIL) FROM-END (START 0) (END NIL) (INITIAL-VALUE NIL IVP))) 
     (:GENERIC REINITIALIZE-INSTANCE (INSTANCE &REST INITARGS)) 
     (:FUNCTION REM (NUMBER DIVISOR)) 
     (:MACRO REMF (PLACE INDICATOR &ENVIRONMENT ENV)) 
     (:FUNCTION REMHASH (KEY HASH-TABLE)) 
     (:FUNCTION REMOVE (ITEM SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:FUNCTION REMOVE-DUPLICATES (SEQUENCE &KEY (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0) (END NIL) FROM-END (KEY NIL))) 
     (:FUNCTION REMOVE-IF (PREDICATE SEQUENCE &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:FUNCTION REMOVE-IF-NOT (PREDICATE SEQUENCE &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:GENERIC REMOVE-METHOD (GENERIC-FUNCTION METHOD)) 
     (:FUNCTION REMPROP (SYMBOL INDICATOR)) 
     (:FUNCTION RENAME-FILE (FILE NEW-NAME)) 
     (:FUNCTION RENAME-PACKAGE (PACKAGE NAME &OPTIONAL (NICKNAMES NIL))) 
     (:FUNCTION REPLACE (SEQUENCE1 SEQUENCE2 &KEY (START1 0) (END1 NIL) (START2 0) (END2 NIL))) 
     (:FUNCTION REQUIRE (MODULE-NAME &OPTIONAL PATHNAMES)) 
     (:FUNCTION REST (LIST)) 
     (:MACRO RESTART-BIND (BINDINGS &BODY FORMS)) 
     (:MACRO RESTART-CASE (EXPRESSION &BODY CLAUSES &ENVIRONMENT ENV)) 
     (:FUNCTION RESTART-NAME (INSTANCE)) 
     (:MACRO RETURN (&OPTIONAL (VALUE NIL))) 
     (:SPECIAL-OPERATOR RETURN-FROM (NAME &OPTIONAL VALUE)) 
     (:FUNCTION REVAPPEND (X Y)) 
     (:FUNCTION REVERSE (SEQUENCE)) 
     (:FUNCTION ROOM (&OPTIONAL (VERBOSITY DEFAULT))) 
     (:MACRO ROTATEF (&REST ARGS &ENVIRONMENT ENV)) 
     (:FUNCTION ROUND (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION ROW-MAJOR-AREF (ARRAY INDEX)) 
     (:FUNCTION RPLACA (X Y)) 
     (:FUNCTION RPLACD (X Y)) 
     (:FUNCTION SBIT (SIMPLE-BIT-ARRAY &REST SUBSCRIPTS)) 
     (:FUNCTION SCALE-FLOAT (F EX)) 
     (:FUNCTION SCHAR (STRING INDEX)) 
     (:FUNCTION SEARCH (SEQUENCE1 SEQUENCE2 &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START1 0) (END1 NIL) (START2 0) (END2 NIL) (KEY NIL))) 
     (:FUNCTION SECOND (LIST)) 
     (:FUNCTION SET (SYMBOL NEW-VALUE)) 
     (:FUNCTION SET-DIFFERENCE (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION SET-DISPATCH-MACRO-CHARACTER (DISP-CHAR SUB-CHAR FUNCTION &OPTIONAL (RT *READTABLE*))) 
     (:FUNCTION SET-EXCLUSIVE-OR (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))) 
     (:FUNCTION SET-MACRO-CHARACTER (CHAR FUNCTION &OPTIONAL (NON-TERMINATINGP NIL) (READTABLE *READTABLE*))) 
     (:FUNCTION SET-PPRINT-DISPATCH (TYPE FUNCTION &OPTIONAL (PRIORITY 0) (TABLE *PRINT-PPRINT-DISPATCH*))) 
     (:FUNCTION SET-SYNTAX-FROM-CHAR (TO-CHAR FROM-CHAR &OPTIONAL (TO-READTABLE *READTABLE*) (FROM-READTABLE NIL))) 
     (:MACRO SETF (&REST ARGS &ENVIRONMENT ENV)) 
     (:SPECIAL-OPERATOR SETQ (&WHOLE SOURCE &REST THINGS)) 
     (:FUNCTION SEVENTH (LIST)) 
     (:FUNCTION SHADOW (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION SHADOWING-IMPORT (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:GENERIC SHARED-INITIALIZE (INSTANCE SLOT-NAMES &REST INITARGS &ALLOW-OTHER-KEYS)) 
     (:MACRO SHIFTF (&WHOLE FORM &REST ARGS &ENVIRONMENT ENV)) 
     (:FUNCTION SHORT-SITE-NAME NIL) 
     (:FUNCTION SIGNAL (DATUM &REST ARGUMENTS)) 
     (:FUNCTION SIGNUM (NUMBER)) 
     (:FUNCTION SIMPLE-BIT-VECTOR-P (OBJECT)) 
     (:FUNCTION SIMPLE-CONDITION-FORMAT-ARGUMENTS (CONDITION)) 
     (:FUNCTION SIMPLE-CONDITION-FORMAT-CONTROL (CONDITION)) 
     (:FUNCTION SIMPLE-STRING-P (OBJECT)) 
     (:FUNCTION SIMPLE-VECTOR-P (OBJECT)) 
     (:FUNCTION SIN (NUMBER)) 
     (:FUNCTION SINH (NUMBER)) 
     (:FUNCTION SIXTH (LIST)) 
     (:FUNCTION SLEEP (N)) 
     (:FUNCTION SLOT-BOUNDP (OBJECT SLOT-NAME)) 
     (:FUNCTION SLOT-EXISTS-P (OBJECT SLOT-NAME)) 
     (:FUNCTION SLOT-MAKUNBOUND (OBJECT SLOT-NAME)) 
     (:GENERIC SLOT-MISSING (CLASS INSTANCE SLOT-NAME OPERATION &OPTIONAL NEW-VALUE)) 
     (:GENERIC SLOT-UNBOUND (CLASS INSTANCE SLOT-NAME)) 
     (:FUNCTION SLOT-VALUE (OBJECT SLOT-NAME)) 
     (:FUNCTION SOFTWARE-TYPE NIL) 
     (:FUNCTION SOFTWARE-VERSION NIL) 
     (:FUNCTION SOME (PRED FIRST-SEQ &REST MORE-SEQS)) 
     (:FUNCTION SORT (SEQUENCE PREDICATE &KEY KEY)) 
     (:FUNCTION SPECIAL-OPERATOR-P (SYMBOL)) 
     (:FUNCTION SQRT (NUMBER)) 
     (:FUNCTION STABLE-SORT (SEQUENCE PREDICATE &KEY KEY)) 
     (:FUNCTION STANDARD-CHAR-P (CHAR)) 
     (:MACRO STEP (FORM)) 
     (:FUNCTION STORE-VALUE (VALUE &OPTIONAL CONDITION)) 
     (:GENERIC STREAM-ELEMENT-TYPE (NON-STREAM)) 
     (:FUNCTION STREAM-ERROR-STREAM (CONDITION)) 
     (:FUNCTION STREAM-EXTERNAL-FORMAT (STREAM)) 
     (:FUNCTION STREAMP (STREAM)) 
     (:FUNCTION STRING (X)) 
     (:FUNCTION STRING-CAPITALIZE (STRING &KEY (START 0) END)) 
     (:FUNCTION STRING-DOWNCASE (STRING &KEY (START 0) END)) 
     (:FUNCTION STRING-EQUAL (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING-GREATERP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING-LEFT-TRIM (CHAR-BAG STRING)) 
     (:FUNCTION STRING-LESSP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING-NOT-EQUAL (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING-NOT-GREATERP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING-NOT-LESSP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING-RIGHT-TRIM (CHAR-BAG STRING)) 
     (:FUNCTION STRING-TRIM (CHAR-BAG STRING)) 
     (:FUNCTION STRING-UPCASE (STRING &KEY (START 0) END)) 
     (:FUNCTION STRING/= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING< (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING<= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING> (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRING>= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)) 
     (:FUNCTION STRINGP (OBJECT)) 
     (:FUNCTION SUBLIS (ALIST TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))) 
     (:FUNCTION SUBSEQ (SEQUENCE START &OPTIONAL END)) 
     (:FUNCTION SUBSETP (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION SUBST (NEW OLD TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))) 
     (:FUNCTION SUBST-IF (NEW TEST TREE &KEY KEY)) 
     (:FUNCTION SUBST-IF-NOT (NEW TEST TREE &KEY KEY)) 
     (:FUNCTION SUBSTITUTE (NEW OLD SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0) (COUNT NIL) (END NIL) (KEY NIL))) 
     (:FUNCTION SUBSTITUTE-IF (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:FUNCTION SUBSTITUTE-IF-NOT (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END NIL) (COUNT NIL) (KEY NIL))) 
     (:FUNCTION SUBTYPEP (TYPE1 TYPE2 &OPTIONAL ENVIRONMENT)) 
     (:FUNCTION SVREF (SIMPLE-VECTOR INDEX)) 
     (:FUNCTION SXHASH (X)) 
     (:FUNCTION SYMBOL-FUNCTION (SYMBOL)) 
     (:SPECIAL-OPERATOR SYMBOL-MACROLET (MACROBINDINGS &BODY BODY)) 
     (:FUNCTION SYMBOL-NAME (SYMBOL)) 
     (:FUNCTION SYMBOL-PACKAGE (SYMBOL)) 
     (:FUNCTION SYMBOL-PLIST (SYMBOL)) 
     (:FUNCTION SYMBOL-VALUE (SYMBOL)) 
     (:FUNCTION SYMBOLP (OBJECT)) 
     (:FUNCTION SYNONYM-STREAM-SYMBOL (INSTANCE)) 
     (:SPECIAL-OPERATOR TAGBODY (&REST STATEMENTS)) 
     (:FUNCTION TAILP (OBJECT LIST)) 
     (:FUNCTION TAN (NUMBER)) 
     (:FUNCTION TANH (NUMBER)) 
     (:FUNCTION TENTH (LIST)) 
     (:FUNCTION TERPRI (&OPTIONAL (STREAM *STANDARD-OUTPUT*))) 
     (:SPECIAL-OPERATOR THE (TYPE VALUE)) 
     (:FUNCTION THIRD (LIST)) 
     (:SPECIAL-OPERATOR THROW (TAG RESULT)) 
     (:MACRO TIME (FORM)) 
     (:MACRO TRACE (&REST SPECS)) 
     (:FUNCTION TRANSLATE-LOGICAL-PATHNAME (PATHNAME &KEY)) 
     (:FUNCTION TRANSLATE-PATHNAME (SOURCE FROM-WILDNAME TO-WILDNAME &KEY)) 
     (:FUNCTION TREE-EQUAL (X Y &KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:FUNCTION TRUENAME (PATHNAME)) 
     (:FUNCTION TRUNCATE (NUMBER &OPTIONAL (DIVISOR 1))) 
     (:FUNCTION TWO-WAY-STREAM-INPUT-STREAM (INSTANCE)) 
     (:FUNCTION TWO-WAY-STREAM-OUTPUT-STREAM (INSTANCE)) 
     (:FUNCTION TYPE-ERROR-DATUM (CONDITION)) 
     (:FUNCTION TYPE-ERROR-EXPECTED-TYPE (CONDITION)) 
     (:FUNCTION TYPE-OF (OBJECT)) 
     (:MACRO TYPECASE (KEYFORM &BODY CASES)) 
     (:FUNCTION TYPEP (OBJECT TYPE &OPTIONAL ENVIRONMENT)) 
     (:FUNCTION UNBOUND-SLOT-INSTANCE (CONDITION)) 
     (:FUNCTION UNEXPORT (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION UNINTERN (SYMBOL &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION UNION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))) 
     (:MACRO UNLESS (TEST &BODY FORMS)) 
     (:FUNCTION UNREAD-CHAR (CHARACTER &OPTIONAL (STREAM *STANDARD-INPUT*))) 
     (:MACRO UNTRACE (&REST SPECS)) 
     (:FUNCTION UNUSE-PACKAGE (PACKAGES-TO-UNUSE &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:SPECIAL-OPERATOR UNWIND-PROTECT (PROTECTED &BODY CLEANUP)) 
     (:GENERIC UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (PREVIOUS CURRENT &REST INITARGS)) 
     (:GENERIC UPDATE-INSTANCE-FOR-REDEFINED-CLASS (INSTANCE ADDED-SLOTS DISCARDED-SLOTS PROPERTY-LIST &REST INITARGS)) 
     (:FUNCTION UPGRADED-ARRAY-ELEMENT-TYPE (SPEC &OPTIONAL ENVIRONMENT)) 
     (:FUNCTION UPGRADED-COMPLEX-PART-TYPE (SPEC &OPTIONAL ENVIRONMENT)) 
     (:FUNCTION UPPER-CASE-P (CHAR)) 
     (:FUNCTION USE-PACKAGE (PACKAGES-TO-USE &OPTIONAL (PACKAGE (SANE-PACKAGE)))) 
     (:FUNCTION USE-VALUE (VALUE &OPTIONAL CONDITION)) 
     (:FUNCTION USER-HOMEDIR-PATHNAME (&OPTIONAL HOST)) 
     (:FUNCTION VALUES (&REST VALUES)) 
     (:FUNCTION VALUES-LIST (LIST)) 
     (:FUNCTION VECTOR (&REST OBJECTS)) 
     (:FUNCTION VECTOR-POP (ARRAY)) 
     (:FUNCTION VECTOR-PUSH (NEW-EL ARRAY)) 
     (:FUNCTION VECTOR-PUSH-EXTEND (NEW-ELEMENT VECTOR &OPTIONAL (EXTENSION (1+ (LENGTH VECTOR))))) 
     (:FUNCTION VECTORP (OBJECT)) 
     (:FUNCTION WARN (DATUM &REST ARGUMENTS)) 
     (:MACRO WHEN (TEST &BODY FORMS)) 
     (:FUNCTION WILD-PATHNAME-P (PATHNAME &OPTIONAL FIELD-KEY)) 
     (:MACRO WITH-ACCESSORS (SLOTS INSTANCE &BODY BODY)) 
     (:MACRO WITH-COMPILATION-UNIT (OPTIONS &BODY BODY)) 
     (:MACRO WITH-CONDITION-RESTARTS (CONDITION-FORM RESTARTS-FORM &BODY BODY)) 
     (:MACRO WITH-HASH-TABLE-ITERATOR ((FUNCTION HASH-TABLE) &BODY BODY)) 
     (:MACRO WITH-INPUT-FROM-STRING ((VAR STRING &KEY INDEX START END) &BODY FORMS-DECLS)) 
     (:MACRO WITH-OPEN-FILE ((STREAM FILESPEC &REST OPTIONS) &BODY BODY)) 
     (:MACRO WITH-OPEN-STREAM ((VAR STREAM) &BODY FORMS-DECLS)) 
     (:MACRO WITH-OUTPUT-TO-STRING ((VAR &OPTIONAL STRING &KEY (ELEMENT-TYPE (QUOTE (QUOTE CHARACTER)))) &BODY FORMS-DECLS)) 
     (:MACRO WITH-PACKAGE-ITERATOR ((MNAME PACKAGE-LIST &REST SYMBOL-TYPES) &BODY BODY)) 
     (:MACRO WITH-SIMPLE-RESTART ((RESTART-NAME FORMAT-STRING &REST FORMAT-ARGUMENTS) &BODY FORMS)) 
     (:MACRO WITH-SLOTS (SLOTS INSTANCE &BODY BODY)) 
     (:MACRO WITH-STANDARD-IO-SYNTAX (&BODY BODY)) 
     (:FUNCTION WRITE (OBJECT &KEY ((STREAM STREAM) *STANDARD-OUTPUT*) ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*) ((RADIX *PRINT-RADIX*) *PRINT-RADIX*) ((BASE *PRINT-BASE*) *PRINT-BASE*) ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*) ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*) ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*) ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*) ((CASE *PRINT-CASE*) *PRINT-CASE*) ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*) ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*) ((READABLY *PRINT-READABLY*) *PRINT-READABLY*) ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*) *PRINT-RIGHT-MARGIN*) ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*) ((LINES *PRINT-LINES*) *PRINT-LINES*) ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*) *PRINT-PPRINT-DISPATCH*))) 
     (:FUNCTION WRITE-BYTE (INTEGER STREAM)) 
     (:FUNCTION WRITE-CHAR (CHARACTER &OPTIONAL (STREAM *STANDARD-OUTPUT*))) 
     (:FUNCTION WRITE-LINE (STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY (START 0) END)) 
     (:FUNCTION WRITE-SEQUENCE (SEQ STREAM &KEY (START 0) (END NIL))) 
     (:FUNCTION WRITE-STRING (STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY (START 0) END)) 
     (:FUNCTION WRITE-TO-STRING (OBJECT &KEY ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*) ((RADIX *PRINT-RADIX*) *PRINT-RADIX*) ((BASE *PRINT-BASE*) *PRINT-BASE*) ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*) ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*) ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*) ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*) ((CASE *PRINT-CASE*) *PRINT-CASE*) ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*) ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*) ((READABLY *PRINT-READABLY*) *PRINT-READABLY*) ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*) *PRINT-RIGHT-MARGIN*) ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*) ((LINES *PRINT-LINES*) *PRINT-LINES*) ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*) *PRINT-PPRINT-DISPATCH*))) 
     (:FUNCTION Y-OR-N-P (&OPTIONAL FORMAT-STRING &REST ARGUMENTS)) 
     (:FUNCTION YES-OR-NO-P (&OPTIONAL FORMAT-STRING &REST ARGUMENTS)) 
     (:FUNCTION ZEROP (NUMBER)) 
     ))
  "PRIVATE: The list of raw lambda-lists: (kind symbol lambda-list).
It's used only while loading, to generate the *cl-lambda-lists* map,
and could be garbage collected, but it's kept for debugging purposes.
SEE: cl-intro.lisp (that uses sb-introspection to generate this list).
")
;; TODO: garbage-collect *raw-lambda-lists*


(defun raw-lambda-list-of (symbol)
  (car (member* symbol  *raw-lambda-lists*
               :test (function cl:string-equal) :key (function second))))

(defun* compile-lambda-lists ()
  (setf *cl-lambda-lists* (make-hash-table :test (function equal) :size 978))
  ;; We use equal to be case sensitive because Common-Lisp is.
  ;; get-lambda-list will process the input symbol accordingly
  ;; to the *readtable-case*.
  (dolist (item *raw-lambda-lists*)
    (let* ((split (split-lambda-list-on-keywords
                   (third item)
                   (case (first item)
                     ((:function         :FUNCTION)         :ordinary)
                     ((:macro            :MACRO)            :macro)
                     ((:special-operator :SPECIAL-OPERATOR) :macro)
                     ((:generic          :GENERIC)          :generic)
                     (otherwise (error "Unexpected definition kind %S" item)))))
           (arglrec
            (make-lambda-list
             :kind   (first  item)
             :symbol (second item)
             :lambda-list (third  item)
             :help   (make-help-from-split-lambda-list split)
             :cnt-mandatory (if (and (atom (car (car split)))
                                     (not (member (car (car split))
                                                  +cl-lambda-list-keywords+)))
                                (length (car split))
                                0)
             :cnt-optional  (let ((opt (car (member* '&OPTIONAL
                                                    split
                                                    :key (function first)))))
                              (if opt
                                  (length (cdr opt))
                                  0))
             :rest-p (or (member* '&REST split :key (function first))
                          (member* '&BODY split :key (function first)))
              :allow-other-keys-p (member* '&ALLOW-OTHER-KEYS split
                                          :key (function first))
              :keys (nreverse 
                    (mapcar
                     (lambda (x) (if (consp x)
                                     (if (consp (car x)) (caar x) (car x)) x))
                     (cdar (member* '&KEY split :key (function first)))))))
           (name (symbol-name (second item))))
      (lambda-list-compute-actions arglrec)
      (setf (gethash name *cl-lambda-lists*) arglrec)
      (VALUES))))


(defun save-lambda-lists (path)
  (save-excursion
    (delete-file path)
    (find-file path)
    (erase-buffer)
    (print '(setf *cl-lambda-lists* 
             (make-hash-table :test (function equal) :size 978)) 
           (current-buffer))
    (maphash (lambda (k v) (print `(setf (gethash ,k *cl-lambda-lists*) ',v)
                                  (current-buffer))) *cl-lambda-lists*)
    (save-buffer)
    (kill-buffer (current-buffer))
    (message "  done")))


;;----------------------------------------------------------------------


(defstruct (ossn (:constructor alloc-ossn))
  "Open-Sexp-Stack-Node
Upon typing a SPC, the current sexp from the top-level to the current point
is parsed, and stored in a stack of open sexps. An open sexp keeps the list
of enclosed closed sexp (and atoms). 
- start, end       The point in the buffer where the sexp or atom starts
                   and ends (for closed sexps and atoms, -1 for open sexps)
- next, previous   A doubly linked list of sub-expression in the current node.
- kind             Whether the current node is an :atom or a :list.
- below:           The enclosing open sexp (the next frame in the stack).
- length:          The length of the doubly-linked list of sub-expressions.
- first, last      The first and last sub-expressions.
To avoid consing, a heap of ossn structures is allocated once and reused
each time a new SPC is typed. SEE: *oss-heap* and make-ossn.
"
  (start    0     :type integer)
  (end      0     :type integer)
  (next     nil   :type (or null ossn)) ; brother
  (previous nil   :type (or null ossn)) ; brother
  (kind     :list :type (member :atom :list))
  (below    nil   :type (or null ossn)) ; next frame
  (length   0     :type fixnum)         ; number of children
  (first    nil   :type (or null ossn)) ; first child
  (last     nil   :type (or null ossn)) ; last  child
  )                                     ;;ossn


(defparameter *oss-top*       nil
  "The top of the open sexps stack. Use rather: (oss-top-list)")


(defparameter *oss-heap-size* 4096
  "The size of the ossn heap.")


(defparameter *oss-heap-free* 0 
  "The index of the next free ossn in the oss heap.")


(defparameter *oss-heap*
  (let ((oss (make-vector *oss-heap-size* nil)))
    (dotimes (i  *oss-heap-size* oss)
      (setf (aref oss i) (alloc-ossn))))
  "The heap of ossn.")


(defmacro oss-bottom-p (node)
  "
RETURN: Whether the node is the bottom of the stack.
"
  `(< (ossn-start ,node) 0))


(defmacro oss-top-list ()
  "
RETURN: The top of the Open Sexp Stack.
"
  `*oss-top*)


(defun* make-ossn (&key (start    0     )
                  (end      -1    )
                  (next     nil   )
                  (previous nil   )
                  (kind     :list )
                  (below    nil   )
                  (length   0     )
                  (first    nil   )
                  (last     nil   ))
  "
RETURN:  A new ossn record. Actually don't allocate it, just take it from
         the oss heap and initialize it.
"
  (when (<= *oss-heap-size* *oss-heap-free*) (error "Sexp too big."))
  (let ((node (aref *oss-heap* *oss-heap-free*)))
    (incf *oss-heap-free*)
    (setf (ossn-start    node) start
          (ossn-end      node) end
          (ossn-next     node) next
          (ossn-previous node) previous
          (ossn-kind     node) kind
          (ossn-below    node) below
          (ossn-length   node) length
          (ossn-first    node) first
          (ossn-last     node) last)
    node))


(defun* oss-reset ()
  "
DO:     Re-initialize the the stack, reverting the old stack to the heap.
RETURN: the top of the stack, which is a guard with start = -1.
"
  (setf *oss-heap-free* 0
        *oss-top* (make-ossn :start -1))
  (when-debugging (:invariant) (oss-check-invariant "oss-reset"))
  *oss-top*)


(defun* trace-ossn (ossn where)
  (let ((print-level 2))
    (assert (ossn-p ossn) nil
            "%s: not an ossn: %S" where ossn)
    (message "%s: l=%d\nf=%s\nl=%s" where (ossn-length ossn)
             (ossn-first ossn) (ossn-last ossn))))


(defun* oss-check-invariant (&optional where)
  (setf where (or where ""))
  (when-debugging (:trace-ossn) (message "%s" where))
  (assert (or (oss-bottom-p (oss-top-list)) (typep (oss-top-list) 'ossn))
          nil "%s: oss top must be a :list or bottom" where)
  (let ((frames
         (loop with levels = '()
            for level = (oss-top-list) then (ossn-below level)
            until (oss-bottom-p level) do
            (when-debugging (:trace-ossn) (trace-ossn level where))
            (assert (not (member level levels))
                    nil "%s: oss must not be a circular list" where)
            (push level levels)
            (assert (typep level 'ossn)
                    nil "%s: all frames must be of type ossn" where)
            (assert (eq :list (ossn-kind level))
                    nil "%s: all frames must be lists" where)
            (assert (not (ossn-closed-p level))
                    nil "%s: all frames must be open" where)
            finally (return (nreverse levels)))))
    (loop
       for level in frames do
       (let ((children
              (loop with children = '()
                 for child = (ossn-first level) then (ossn-next child)
                 while child 
                 do
                 (when-debugging (:trace-ossn) (trace-ossn child where))
                 (assert (typep child 'ossn)
                         nil "%s: all child must be of type ossn" where)
                 (assert 
                  (not (member child children)) nil
                  "%s: children forward list must not be a circular list" 
                  where)
                 (push child children)
                 finally (return (nreverse children))))
             (nerdlihc
              (loop with children = '()
                 for child = (ossn-last level) then (ossn-previous child)
                 while child
                 do
                 (when-debugging (:trace-ossn) (trace-ossn child where))
                 (assert (typep child 'ossn)
                         nil "%s: all child must be of type ossn" where)
                 (assert
                  (not (member child children)) nil
                  "%s: children backward list must not be a circular list"
                  where)
                 (push child children)
                 finally (return (nreverse children)))))
         (assert (eq (ossn-first level) (first children))
                 nil "%s: first children is not first" where)
         (assert (eq (ossn-last level) (car (last children)))
                 nil "%s: last children is not last" where)
         (assert (eq (ossn-first level) (car (last nerdlihc)))
                 nil "%s: last nerdlihc is not first" where)
         (assert (eq (ossn-last level) (first nerdlihc))
                 nil "%s: first nerdlihc is not last" where)
         (assert (= (ossn-length level) (length children))
                 nil "%s: inconsistent forward children count" where)
         (assert (= (ossn-length level) (length nerdlihc))
                 nil "%s: inconsistent backward children count" where)
         (assert (equalp children (reverse nerdlihc))
                 nil "%s: inconsistent children double-linked list" where)
         (let ((sublists (delete-if-not (lambda (x) (eq :list (ossn-kind x)))
                                        (set-difference children frames))))
           (assert (every (lambda (x)  (ossn-closed-p x)) sublists)
                   nil "%s: all sublists must be closed" where)
           (assert (every (lambda (x) (eq level (ossn-below x))) sublists)
                   nil "%s: all sublists must be above their parent below them"
                   where))))))

;;  (byte-compile 'oss-check-invariant)

(defun* ossn-closed-p (node)
  (<= 0 (ossn-end node)))


(defun* ossn-string (node)
  (cond
    ((null node) "")
    ((ossn-closed-p node)
     (buffer-substring-no-properties (ossn-start node) (ossn-end node)))
    ((< (ossn-start node) 0)
     "BEGIN-OF-BUFFER")
    (t
     (buffer-substring-no-properties (ossn-start node) (point)))))


(defun* ossn-argument-list (node)
  (when-debugging (:invariant) (oss-check-invariant "ossn-argument-list"))
  (do ((result '())
       (argument (ossn-first node) (ossn-next argument)))
      ((null argument) (nreverse result))
    (push (ossn-string argument) result)))


(defparameter *oss-dumped* '() "List of nodes already dumped to avoid circles.")


(defun* dump-ossn (node &optional level)
  "
NOTE:  The entry point is dump-oss below.
"
  (if (member node *oss-dumped*)
      (message "oss:%salready dumped" level)
      (let* ((level (or level "")))
        (push node *oss-dumped*)
        (cond
          ((eq :list (ossn-kind node))
           (message "oss:%s%6s/k %6d/s %6d/e %6d %s"
                    level
                    (ossn-kind   node)
                    (ossn-start  node)
                    (ossn-end    node)
                    (ossn-length node)
                    (if (oss-bottom-p node) "bottom" ""))
           (do ((i 0 (1+ i))
                (child (ossn-first node) (and child (ossn-next child))))
               ((>= i (ossn-length node)))
             (if child
                 (dump-ossn child (format "%s c[%d]" level i))
                 (message "oss:%s c[%d]   nil" level i)))
           (unless (oss-bottom-p node)
             (dump-ossn (ossn-below node) (format "%s |" level))))
          (t
           (message "oss:%s%6s/k %6d/s %6d/e %s"
                    level
                    (ossn-kind   node)
                    (ossn-start  node)
                    (ossn-end    node)
                    (ossn-string node)))))))


(defun* dump-oss ()
  "
DO:     Dump the oss in the *Message* buffer.
"
  (setf *oss-dumped* nil)
  (dump-ossn (oss-top-list)))

;; f=nil, l=nil   | nop
;; f=x,   l=x     | last=x, f=nil, l=nil
;; f=y,   l=x     | last=x, l=last.p, l.n=nil, last.p=nil


(defun* ossn-cut-the-last (ossn)
  (let ((last (ossn-last ossn)))
    (when-debugging (:invariant)
      (oss-check-invariant "before ossn-cut-the-last")
      (dump-oss))
    (when last
      (if (eq (ossn-first ossn) last)
          (setf (ossn-first ossn) nil
                (ossn-last  ossn) nil)
          (when (setf (ossn-last ossn) (ossn-previous last))
            (setf (ossn-next (ossn-last ossn)) nil
                  (ossn-previous last)         nil)))
      (decf (ossn-length ossn)))
    (when-debugging (:invariant) 
      (oss-check-invariant "after ossn-cut-the-last")
      (dump-oss)
      (assert (null (ossn-previous last))
              nil "%s: invalid previous in cut last"  "after ossn-cut-the-last")
      (assert (null (ossn-next last))
              nil "%s: invalid next in cut last"  "after ossn-cut-the-last"))
    last))


(defun* ossn-append (node item)
  (when-debugging (:invariant) (oss-check-invariant "before ossn-append"))
  (if (ossn-last node) 
      (setf (ossn-next (ossn-last node)) item)
      (setf (ossn-first node)  item))
  (setf (ossn-next     item)  nil
        (ossn-previous item)  (ossn-last node)
        (ossn-last     node)  item)
  (incf (ossn-length node))
  (when-debugging (:invariant) (oss-check-invariant "after ossn-append"))
  node)


(defun* oss-push-atom (start end)
  (when-debugging (:invariant) (oss-check-invariant "before oss-push-atom"))
  (let ((new (make-ossn :start    start
                        :end      end
                        :kind     :atom
                        :below    (oss-top-list))))
    (ossn-append (oss-top-list) new)
    (when-debugging (:invariant) (oss-check-invariant "after oss-push-atom"))
    new))


(defun* oss-push-list (start)
  (when-debugging (:invariant) (oss-check-invariant "before oss-push-list"))
  (let ((new (make-ossn :start start
                        :end   -1
                        :kind  :list
                        :below (oss-top-list)
                        :length 0)))
    (ossn-append (oss-top-list) new)
    (setf (oss-top-list) new)
    (when-debugging (:invariant) (oss-check-invariant "after oss-push-list"))
    new))


(defun* oss-close-list (end)
  (prog1 (oss-top-list)
    (setf (ossn-end (oss-top-list)) end
          (oss-top-list) (ossn-below (oss-top-list)))
    (when-debugging (:invariant) 
      (oss-check-invariant "oss-close-list"))))


(defun* oss-closed-list-p (node)
  (<= 0 (ossn-end node)))


(defun* oss-enlist-last ()
  "the last node of the top list is put in a new list."
  (when-debugging (:invariant) (oss-check-invariant "before oss-enlist-last"))
  (let* ((last (ossn-cut-the-last (oss-top-list)))
         (new (make-ossn :start  (1- (ossn-start last))
                         :end    -1     ; an open list
                         :kind   :list
                         :below  (oss-top-list)
                         :length 1
                         :first  last
                         :last   last)))
    (setf (oss-top-list) new)
    (when-debugging (:invariant) (oss-check-invariant "after oss-enlist-last"))
    new))


(defun* oss-close-at-previous ()
  "close the top list at the previous node"
  (when-debugging (:invariant)
    (oss-check-invariant "before oss-close-at-previous"))
  (let* ((last (ossn-cut-the-last (oss-top-list))))
    ;; close the top list
    (setf (ossn-end (oss-top-list)) (1+ (ossn-end (ossn-last (oss-top-list)))))
    (setf (oss-top-list) (ossn-below (oss-top-list)))
    ;; append the cut item to the new top
    (ossn-append (oss-top-list) last)
    (when-debugging (:invariant)
      (oss-check-invariant "after oss-close-at-previous"))
    last))


(defparameter +terminating-macro-char-regexp+    "[()'\",` \t\r\l\n\v\f]")
(defparameter +top-level-sexp-or-comment-regexp+ "^\\([(;]\\|#|\\)") ;
;; (defun* char-constituant-p (ch) (not (position ch "()'\",` \t\r\l\n\v\f")))


(defun* open-sexp-stack (where)
  "
DO:      Build a new open-sexp-stack from the top-level to the where point.
RETURN:  A stack of nodes.
"
  (let ((frame-start
         (progn
           (goto-char where)
           (if (search-backward-regexp +top-level-sexp-or-comment-regexp+ nil t)
               (point)
               (point-min))))
        (parse-sexp-ignore-comments t))
    (goto-char frame-start)
    (oss-reset)
    (while (< (point) where)
      (let ((what (cl-looking-at-what)))
        (case what
          ((:beginning-of-list)
           (oss-push-list (point))
           (cl-skip-over what))
          ((:end-of-list)
           (cl-skip-over what)
           (if (oss-bottom-p (oss-top-list))
               (error "Too many closing parentheses")
               (oss-close-list (point))))
          ((:string :atom)
           (let ((start (point)))
             (cl-skip-over what)
             (oss-push-atom start (point))))
          (otherwise (cl-skip-over what))))))
  (when-debugging (:invariant) (oss-check-invariant "after open-sexp-stack"))
  (oss-top-list))


(defmacro insert-spaces (n) `(self-insert-command ,n))
;;(defmacro insert-spaces (n))


(defun* lambda-list-accept (lambda-list n)
  (when-debugging (:processing) 
    (message "lambda-list-acccept %S" lambda-list))
  (insert-spaces n))


(defun* lambda-list-close (lambda-list n)
  (when-debugging (:processing) 
    (message "lambda-list-close %S" lambda-list))
  (insert ")")
  (oss-close-list (point))
  (cl-process-oss n))


(defun* name-keywordp (name)
  (and (< 1 (length name)) (char= (character ":") (char name 0))))


(defun* lambda-list-check-current-keyword  (lambda-list n)
  (when-debugging (:processing) 
    (message "lambda-list-check-current-keyword %S" lambda-list))
  (let* ((last (ossn-last (oss-top-list)))
         (name (ossn-string last)))
    (if (and (name-keywordp name)
             (or (lambda-list-allow-other-keys-p lambda-list)
                 (member* (subseq name 1) (lambda-list-keys lambda-list)
                         :test (function cl:string-equal))))
        (progn
          (case-lisp-region (ossn-start last) (ossn-end last)
                            *pjb-cl-magic-case*)
          (insert-spaces n))
        (progn
          (goto-char (ossn-end (ossn-previous last)))
          (insert ")")
          (incf (ossn-end (ossn-previous last)))
          (incf (ossn-start last))
          (incf (ossn-end last))
          (oss-close-at-previous)
          (goto-char (ossn-end last))
          (cl-process-oss n)))))


(defun* lambda-list-check-previous-keyword (lambda-list n)
  (when-debugging (:processing) 
    (message "lambda-list-check-previous-keyword %S" lambda-list))
  (let* ((last (ossn-last (oss-top-list)))
         (prev (ossn-previous last))
         (name (ossn-string prev)))
    (if (and (name-keywordp name)
             (or (lambda-list-allow-other-keys-p lambda-list)
                 (member* (subseq name 1) (lambda-list-keys lambda-list)
                         :test (function cl:string-equal))))
        (progn
          (case-lisp-region (ossn-start prev) (ossn-end prev) *pjb-cl-magic-case*)
          (insert-spaces n))
        (progn
          (goto-char (ossn-end (ossn-previous prev)))
          (insert ")")
          (incf (ossn-end   (ossn-previous prev)))
          (incf (ossn-start prev))
          (incf (ossn-end   prev))
          (incf (ossn-start last))
          (incf (ossn-end   last))
          (oss-close-at-previous)
          (goto-char (ossn-end last))
          (cl-process-oss n)))))


;; parameter --> accept.
;; &KEY --> if a keyword in the list then accept an argument more else end.
;; &ALLOW-OTHER-KEYS --> if a keyword then accept an argument more else end.
;; &OPTIONAL --> accept or ).
;; &BODY, &REST --> any number of additionnal parameter ==> ) mandatory.
;; &AUX, &ENVIRONMENT, &WHOLE --> ignore

;;     conditions:
;;     ( symbol-in-common                                 ==> NOP
;;     ( something                                        ==> NOP
;;     ( ... symbol-in-common                             ==> (a function
;;     ( ... something                                    ==> a variable
;;
;;     ( ... something              parameter            ==> wait for cnt or )
;;     ( ... something              &OPTIONAL parameter  ==> wait for cnt or )
;;     ( ... :keyword               &key keyword         ==> wait for not kw
;;     ( ... :keyword something     &key keyword         ==> wait for not kw
;;     ( ... :keyword               &ALLOW-OTHER-KEYS    ==> wait for not kw
;;     ( ... :keyword something     &ALLOW-OTHER-KEYS    ==> wait for not kw
;;     ( ... something              &BODY, &REST         ==> wait for )
;;
;;     (&body + &rest) xor (&key + &allow-other-keys)


(defvar *pjb-cl-magic-help-message* "" "PRIVATE")


(defun* cl-process-function (n node)
  (when-debugging (:processing) (message "cl-process-function"))
  (case-lisp-region (ossn-start node) (ossn-end node) *pjb-cl-magic-case*)
  (when (oss-bottom-p (oss-top-list))
    (goto-char (ossn-start node))
    (insert "(")
    (incf (ossn-start node))
    (incf (ossn-end   node))
    (goto-char (ossn-end node))
    (oss-enlist-last))
  (insert-spaces n))


(defun* cl-process-arguments (n)
  (when-debugging (:processing) (message "cl-process-arguments"))
  (let* ((fun-node (ossn-first  (oss-top-list)))
         (fun-name (ossn-string fun-node))
         (fun-argl (get-lambda-list-of fun-name)))
    (if fun-argl
        (progn
          (setf *pjb-cl-magic-help-message*
                (lambda-list-get-help fun-argl (ossn-argument-list (oss-top-list))))
          (case-lisp-region (ossn-start fun-node) (ossn-end fun-node)
                            *pjb-cl-magic-case*)
          (funcall (lambda-list-action fun-argl (ossn-length (oss-top-list)))
                   fun-argl n))
        (insert-spaces n))))


(defun* cl-process-oss (n)
  (when-debugging (:processing) (message "cl-process-oss -- step 1"))
  (cond
    ((zerop (ossn-length (oss-top-list))) ; (_ --> (lisp-indent-line) ?
     (when-debugging (:processing) (message "cl-process-oss -- step 2"))
     (insert-spaces n))
    ((oss-closed-list-p (oss-top-list))
     (when-debugging (:invariant) (error "Should not occur")))
    ((= 1 (ossn-length (oss-top-list))) ; (fun_ or |fun_
     (when-debugging (:processing) (message "cl-process-oss -- step 3"))
     ;; (when (oss-bottom-p (oss-top-list))
     (cl-process-function n (ossn-last (oss-top-list))))
    (t                                  ; (fun ... stuff_
     (let* ((node (ossn-last (oss-top-list)))
            (name (ossn-string node))
            (argl (get-lambda-list-of name)))
       (when-debugging (:processing)
         (message "cl-process-oss -- step 4 name=%S node=%S argl=%S" 
                  name node argl))
       (if argl
           ;; (stuff ... CL:stuff_ --> (stuff ... (CL:STUFF_
           (cl-process-function n node) 
           ;; (fun ... stuff_ --> (fun ... stuff) or (fun ... stuff_
           (cl-process-arguments n))))))

;; (_                     t1
;; (fun_                  t2
;; (fun ... stuff_        t3


(defun* pjb-cl-magic-space (n)
  (interactive "p")
  (open-sexp-stack (point))
  (cl-process-oss n)
  (when *pjb-cl-magic-indent* (lisp-indent-line))
  (message "%s" *pjb-cl-magic-help-message*))


(eval-when (load) (byte-compile 'oss-check-invariant))

(let* ((load-file-name (or load-file-name 
                           ;; for testing:
                           "/home/pjb/src/public/emacs/pjb-cl-magic.el"))
       (lambda-lists-el (format "%s-lambda-lists.el" 
                          (subseq load-file-name
                                  0 (search ".el" load-file-name :from-end t))))
       (lambda-lists-elc (format "%sc" lambda-lists-el)))
  (unless (file-newer-than-file-p lambda-lists-el load-file-name)
    (message "Compiling lambda lists...")
    (compile-lambda-lists)
    (save-lambda-lists lambda-lists-el)
    (message "Compiled  lambda lists."))
  (unless (file-newer-than-file-p lambda-lists-elc lambda-lists-el)
    (byte-compile-file lambda-lists-el))
  (load lambda-lists-elc))

;; (when (require 'slime nil t)
;;   (define-key slime-mode-map  " " 'pjb-cl-magic-space)
;;   (define-key slime-mode-map  " " 'slime-space))


;; car_
;; (CAR_ / LIST
;;      mylist_
;; (CAR_MYLIST)_
;;
;; car_
;; (CAR_ / LIST
;;      car_
;; (CAR_(CAR_ / LIST
;;           print_
;; (CAR_(CAR_(PRINT_ / OBJECT &OPTIONAL STREAM
;;                  toto_
;; (CAR_(CAR_(PRINT_TOTO_ / &OPTIONAL STREAM
;;                       OUT)
;; (CAR_(CAR_(PRINT_TOTO_OUT)))

;;;; pjb-cl-magic.el                  --                     --          ;;;;
