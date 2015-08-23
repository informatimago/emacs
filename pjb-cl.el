;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;*****************************************************************************
;;;;FILE:               pjb-cl.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports a few Common Lisp operators missing from cl.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2006-11-14 <PJB> Removed most of CL functions and downcased the rest
;;;;                     to avoid collision with emacs-cl.
;;;;    2004-11-01 <PJB> Upcased COMMON-LISP symbols.  This will avoid name
;;;;                     collision between Common-Lisp and emacs-lisp. 
;;;;    2003-01-20 <PJB> Replaced pjb-cl%%string-replace
;;;;                     by regexp-replace-in-string.
;;;;    2002-04-10 <PJB> Creation.
;;;;BUGS
;;;;
;;;;    encode-time/decode-time have a range too limited (32-bit around 1/1/1970)
;;;;    we should implement our own version.
;;;;
;;;;
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
;;;;*****************************************************************************
(require 'cl)
(load "cl-seq"   nil t)
(load "cl-extra" nil t)
(require 'eieio)
(require 'eieio-opt)
(require 'parse-time)
(setf lexical-binding t)

;; Let's teach emacs how to format Common-Lisp:

(mapc (lambda (sym) (put sym 'lisp-indent-function 1))
      '(concatenate with-open-file with-open-stream
        with-input-from-string with-output-to-string dolist lambda
        when unless while until if let let* handler-case))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions.


(defconst pjb-cl%%epoch 2208988800.0
  "I compute 25567 days, or  2208988800 seconds from 1900-01-01 00:00:00
   to 1970-01-01 00:00:00.  Please correct me if I'm wrong.")


(defun pjb-cl%%leap-year (year)
  "PRIVATE.
RETURN: Whether the `year' is a leap-year.
NOTE:   `year' should be 2002 if it's the third year of the XXth century...
"
  (or (= 0 (mod year 400))
      (and (/= 0 (mod year 100))
           (= 0 (mod year 4)) )))


(defun pjb-cl%%days-in-year (year)
  "PRIVATE.
RETURN: The number of days in the given year.
"
  (if (pjb-cl%%leap-year year) 366 365))


(defun pjb-cl%%seconds-to-emacs-time (secs)
  "PRIVATE.
PRE:     secs is a number of seconds.
RETURN:  The time represented by secs in emacs time format, ie.
         a list ( h l us ) with h=secs/2^16, l=secs%2^16, us=(secs*1e6)%1e6.
"
  (let* ( (h  (truncate (/ secs 65536)))
         (lf (- secs (* h 65536.0)))
          (l  (truncate lf))
          (us (truncate (* 1000000.0 (- lf l)))) )
    (list h l us) ))


(defun pjb-cl%%emacs-time-to-seconds (et)
  "PRIVATE.
PRE:     et is a time in emacs time format, ie. a list ( h l [us])
         with h and l being in [0..65535] and us in [0..999999].
RETURN:  et expressed as a scalar."
  (+ (let ((h (nth 0 et))) (if (< h 1024) (* h 65536) (* h 65536.0)))
     (nth 1 et)
     (let ((us (nth 2 et))) (if (or (null us) (= 0 us)) 0 (/ us 1000000.0)))))


(defun pjb-cl%%char-in-bag (char bag)
  "PRIVATE.
RETURN: Whether `char' is in `bag'.
"
  (let ( (len (length bag))
        (i   0) )
    (while (and (< i len) (/= char (elt bag i)))
      (setq i (1+ i)))
    (< i len)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common-Lisp


;; -----------------------------
;; - 5 - Data and Control Flow -
;; -----------------------------

(defmacro defconstant (symbol initvalue &optional docstring)
  `(defconst ,symbol ,initvalue ,docstring))


(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))


(defmacro multiple-value-prog1 (first-form &rest forms) 
  ;; Multiple values are implemented as list of values. BAD.
  `(prog1 ,first-form ,@forms)) 


(defconstant multiple-value-limit 4098)




;; From: Stefan Monnier <monnier@iro.umontreal.ca>
;; Subject: Re: condition-case
;; Newsgroups: gnu.emacs.help
;; Date: Thu, 09 Dec 2010 10:04:12 -0500
;; Organization: A noiseless patient Spider
;; Message-ID: <jwvbp4v9enz.fsf-monnier+gnu.emacs.help@gnu.org>
;;
;; Oh wait, I just noticed this one: `subst' is wrong here.  I know CL
;; already uses it for similar purposes elsewhere, but it's simply wrong
;; because `subst' doesn't know about Elisp binding rules.
;; So (subst 'b 'a '(lambda () '(a b c))) will happily return
;; (lambda () '(b b c)).  Better simply use `let', even if it has
;; a performance cost.

;; (defmacro handler-case (expression &rest clauses)
;;   "Evaluate expression with `condition-case' and catch errors with CLAUSES.
;; 
;; Longer explanation here..."
;;   (let* ((var (gensym))
;;          (neclause (assoc :NO-ERROR clauses))
;;          (nell     (cadr neclause))
;;          (nebody   (cddr neclause))
;;          (handlers (mapcar (lambda (clause)
;;                              (let ((typespec (car clause))
;;                                    (clausvar (cadr clause))
;;                                    (body     (cddr clause)))
;;                                (cons (if (and (consp typespec)
;;                                               (eq 'or (car typespec)))
;;                                          (cdr typespec)
;;                                        typespec)
;;                                      (if (null clausvar)
;;                                          body
;;                                        (let ((var (car clausvar)))
;;                                          body)))))
;;                            (remove neclause clauses))))
;;     (if neclause
;;         `(condition-case ,var
;;              (multiple-value-bind ,nell ,expression ,@nebody)
;;            ,@handlers)
;;       `(condition-case ,var
;;            ,expression
;;          ,@handlers))))

(defun complement (fun)
  (lambda (&rest arguments)
    (not (apply fun arguments))))

;; ------------------
;; - 9 - Conditions -
;; ------------------




;; (condition-case VAR BODYFORM HANDLERS...)
;;
;; Regain control when an error is signaled.
;; Executes BODYFORM and returns its value if no error happens.
;; Each element of HANDLERS looks like (CONDITION-NAME BODY...)
;; where the BODY is made of Lisp expressions.
;;
;; A handler is applicable to an error
;; if CONDITION-NAME is one of the error's condition names.
;; If an error happens, the first applicable handler is run.
;;
;; The car of a handler may be a list of condition names
;; instead of a single condition name.
;;
;; When a handler handles an error,
;; control returns to the condition-case and the handler BODY... is executed
;; with VAR bound to (SIGNALED-CONDITIONS . SIGNAL-DATA).
;; VAR may be nil; then you do not get access to the signal information.
;;
;; The value of the last BODY form is returned from the condition-case.
;; See also the function `signal' for more info.

(defmacro handler-case (expression &rest clauses)
  "Common-Lisp
IMPLEMENTATION: The clause variable symbols are substituted by one single
                condition-case variable symbol.  This may cause problems
                if the same symbol is used as data or if it's a dynamic 
                variable.
"
  (let* ((var (gensym))
         (neclause (assoc :NO-ERROR clauses))
         (nell     (cadr neclause))
         (nebody   (cddr neclause))
         (handlers (mapcar (lambda (clause)
                             (let ((typespec (car clause))
                                   (clausvar (cadr clause))
                                   (body     (cddr clause)))
                               (cons (if (and (consp typespec)
                                              (eq 'or (car typespec)))
                                         (cdr typespec) 
                                         typespec)
                                     (if (null clausvar)
                                         body
                                         (subst  var (car clausvar) body)))))
                           (remove neclause clauses))))
    (if neclause
        `(condition-case ,var
             (multiple-value-bind ,nell ,expression ,@nebody)
           ,@handlers)
        `(condition-case ,var
             ,expression
             ,@handlers))))


(defmacro cl:ignore-errors (&rest body)
  `(handler-case
       (progn ,@body)
     (error (err)
       (message "cl:ignore-errors %S" err)
       (values nil err))))


;; ----------------
;; - 10 - Symbols -
;; ----------------


(defun copy-symbol (symbol &optional copy-properties)
  (let ((new (make-symbol (cl:string symbol))))
    (when copy-properties
      (setf (symbol-plist new) (COPY-SEQ (symbol-plist symbol))))
    (setf (symbol-value    new)  (symbol-value    symbol) 
          (symbol-function new)  (symbol-function symbol))
    new))


(defun cl:symbol-package (sym)
  "Return the name of the package of the symbol.
This is \"emacs-lisp\" if the symbol doesn't contain any colon,
\"keyword\" if it starts with a colon, or the substring before the
colon if it contains one."
  (let* ((name (symbol-name sym))
         (colon (position (character ":") name)))
    (case colon
      ((nil) "emacs-lisp")
      ((0)   "keyword")
      (otherwise (subseq name 0 colon)))))


(defun cl:symbol-name (sym)
  "Return the name of the symbol.  This is what's after the colon or
double colon if any, or the symbol name."
  (let* ((name (symbol-name sym))
         (colon (position (character ":") name)))
    (cond 
      ((and colon (char= (character ":") (char name (1+ colon))))
       (subseq name (+ 2 colon)))
      (colon
       (subseq name (+ 1 colon)))
      (t name))))


;; -----------------
;; - 11 - Packages -
;; -----------------


(defun intern* (string &optional package)
  (cond ((and (stringp package) (string= "KEYWORD" package))
         (intern (format ":%s" string)))
        (t
         (intern string))))


;; ----------------
;; - 12 - Numbers -
;; ----------------

(defun compute-most-positive-fixnum ()
  (loop
     for p from 0
     for i = 1 then (* 2 i)
     while (plusp i)
     finally (return (+ (expt 2 (1- p)) (1- (expt 2 (1- p)))))))


(unless (boundp 'most-positive-fixnum)
  (defconstant most-positive-fixnum  (compute-most-positive-fixnum)
   "Common-Lisp: most-positive-fixnum is that fixnum closest in value to positive infinity provided by the implementation, and greater than or equal to both 2^15 - 1 and array-dimension-limit.
URL: http://www.informatimago.com/local/lisp/HyperSpec/Body/v_most_p.htm#most-positive-fixnum"))


(unless (boundp 'most-negative-fixnum)
 (defconstant most-negative-fixnum  (- -1 (compute-most-positive-fixnum))
   "Common-Lisp: most-negative-fixnum is that fixnum closest in value to negative infinity provided by the implementation, and less than or equal to both -2^15.
URL: http://www.informatimago.com/local/lisp/HyperSpec/Body/v_most_p.htm#most-negative-fixnum"))



(defun integer-length (x)
  "Returns the number of bits needed to represent integer in binary two's-complement format."
  (if (minusp x)
    (integer-length (- x))
    (loop
     until (zerop x)
     do (setf x (ash x -1))
     sum 1 into result
     finally (return result))))


(defun* cl:digit-char-p (char &optional (radix 10))
  (let ((value (position (upcase char) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (and value (< value radix) value)))

(defun* cl:parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  (let ((end    (or end (length string)))
        (n      0)
        (sign   1)
        (plus   (character "+"))
        (minus  (character "-"))
        (space  (character " ")))
    (labels ((parse-integer-error ()
               (error "Not an integer string %S (:start %d :end %d :radix %d)"
                      string start end radix))
             (check-range ()
               (unless (< start end)
                 (parse-integer-error)))
             (eat-spaces (i)
               (loop
                 while (and (< i end) (char= space (aref string i)))
                 do (incf i)
                 finally (return i))))
      (setf start (eat-spaces start))
      (check-range)
      (cond
        ((char= plus  (aref string start)) (setf sign +1) (incf start) (check-range))
        ((char= minus (aref string start)) (setf sign -1) (incf start) (check-range)))
      (loop
        for i from start below end
        for digit = (cl:digit-char-p (aref string i) radix)
        while digit
        do (setf n (+ (* n radix) digit))
        finally (when (< i end)
                  (setf i (eat-spaces i)))
                (when (and (not junk-allowed) (< i end))
                  (parse-integer-error))
                (return (values (* sign n) i))))))



;; -------------------
;; - 13 - Characters -
;; -------------------


(unless (fboundp 'characterp)
 (defun characterp (x)
   "Common-Lisp: Returns true if object is of type character; otherwise, returns false.
URL:    http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chp.htm#characterp
RETURN: Whether the argument is a character.
NOTE:   With emacs, characters are integers."
   (integerp x)))


(defun character (character)
  "Common-Lisp: Returns the denoted character

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_ch.htm
RETURN:   An emacs character.
EXAMPLES: (CHARACTER #\\a) => ?a   INVALID: # is invalid read syntax for emacs!
          (CHARACTER \"a\") => ?a
          (CHARACTER 'a)  => ?A    WARNING: emacs symbols are multicase!
          (CHARACTER '\\a) => ?a
          (CHARACTER 65.) is an error.
          (CHARACTER 'apple) is an error.
"
  (cond
    ((characterp character)   character)
    ((symbolp character)
     (if (= 1 (length (cl:symbol-name character)))
         (string-to-char (cl:symbol-name character))
         (error "character does not accept symbols of more than one character.")))
    ((stringp character)
     (string-to-char character))
    (t (error "Unexpected type fo character argument."))))


(defun upper-case-p (character)
  "common-lisp:
"
  (and (equal (upcase character) character)
       (not (equal (downcase character) character))))


(defun lower-case-p (character)
  "common-lisp:
"
  (and (equal (downcase character) character)
       (not (equal (upcase character) character))))


(defun both-case-p (character)
  "common-lisp:
"
  (or (upper-case-p character) (lower-case-p character)))


(defun char-upcase   (character)  (upcase   character))
(defun char-downcase (character)  (downcase character))

(defun code-char* (code)
  "Common-Lisp:
"
  (if (numberp code)
      (coerce code 'character)
      (error "%s is not a character code." code)))


(defun char-code* (character)
  "Common-Lisp: Return the code attribute of character.

RETURN:  character (emacs characters are numbers).
"
  character)


(defun char= (&rest characters)
  "common-lisp: compare character arguments.
url:      http://www.informatimago.com/local/lisp/hyperspec/body/f_chareq.htm
"
  (unless characters
    (error "too few arguments given to char=."))
  (let ((a (car characters)))
    (every (lambda (b) (= a b)) (cdr characters))))


(defun char/= (&rest characters)
  "common-lisp: compare character arguments.
Returns true if all characters are different (no two equal).

url:      http://www.informatimago.com/local/lisp/hyperspec/body/f_chareq.htm
"
  (unless characters
    (error "too few arguments given to char/=."))
  (if (= 1 (length characters))
      t
      (loop with already-seen = nil
            with result = t 
            for c in characters
            while result 
            do (if (memq c already-seen)
                   (setq result nil)
                   (push c already-seen))
            finally (return result))))


(defmacro pjb-cl%%compare-chars (name order characters)
  `(progn
     (unless ,characters
       (error "Too few arguments given to %s." ',name))
     (let ((prev (car ,characters))
           (result t))
       (loop for next in (cdr ,characters)
          while  result
          do (unless (,order (char-code* prev) (char-code* next))
               (setq result nil))
          (setq prev next))
       result)))



(defun char< (&rest characters)
  "Common-Lisp: returns true if the characters are monotonically increasing; \
otherwise, it returns false.

If two characters have identical implementation-defined attributes,
then their ordering by char< is consistent with the numerical ordering
by the predicate < on their codes.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (pjb-cl%%compare-chars char< < characters))


(defun char> (&rest characters)
  "Common-Lisp:  returns true if the characters are monotonically decreasing; \
otherwise, it returns false.

If two characters have identical implementation-defined attributes,
then their ordering by char> is consistent with the numerical ordering
by the predicate > on their codes.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (pjb-cl%%compare-chars char> > characters))


(defun char<= (&rest characters)
  "Common-Lisp: returns true if the characters are monotonically nondecreasing;\
otherwise, it returns false.

If two characters have identical implementation-defined attributes,
then their ordering by char<= is consistent with the numerical
ordering by the predicate <= on their codes.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (pjb-cl%%compare-chars char<=  <=  characters))


(defun char>= (&rest characters)
  "Common-Lisp: returns true if the characters are monotonically nonincreasing;\
otherwise, it returns false.

If two characters have identical implementation-defined attributes,
then their ordering by char>= is consistent with the numerical
ordering by the predicate >= on their codes.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (pjb-cl%%compare-chars char>=  >=  characters))


(defun char-equal (&rest characters)
  "Common-Lisp: similar to char= except it ignores differences in case and \
might have an implementation-defined behavior for non-simple characters.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (apply (function char=) (mapcar 'upcase characters)))


(defun char-not-equal (&rest characters)
  "Common-Lisp: similar to char/= except it ignores differences in case and \
might have an implementation-defined behavior for non-simple characters.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (apply (function char/=) (mapcar 'upcase characters)))


(defun char-lessp (&rest characters)
  "Common-Lisp: similar to char< except it ignores differences in case and \
might have an implementation-defined behavior for non-simple characters.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (apply (function char<) (mapcar 'upcase characters)))


(defun char-greaterp (&rest characters)
  "Common-Lisp: similar to char> except it ignores differences in case and \
might have an implementation-defined behavior for non-simple characters.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (apply (function char>) (mapcar 'upcase characters)))


(defun char-not-greaterp (&rest characters)
  "Common-Lisp: similar to char<= except it ignores differences in case and \
might have an implementation-defined behavior for non-simple characters.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (apply (function char<=) (mapcar 'upcase characters)))


(defun char-not-lessp (&rest characters)
  "Common-Lisp: similar to char>= except it ignores differences in case and \
might have an implementation-defined behavior for non-simple characters.

URL:      http://www.informatimago.com/local/lisp/HyperSpec/Body/f_chareq.htm
"
  (apply (function char>=) (mapcar 'upcase characters)))


(defun ensure-character (ch)
  (cond 
    ((characterp ch) ch)
    ((stringp ch)    (char ch 0))
    ((symbolp ch)    (char (cl:symbol-name ch) 0))
    ((numberp ch)    (code-char* ch))
    (t               (error "Expected a character, not: %S" ch))))


(defun alpha-char-p (ch)
  "
COMMON-LISP
IMPLEMENTATION: Assumes ISO-8859-1!
"
;;;   (string-match 
;;;    "\\(\\c0\\|\\c1\\|\\c2\\|\\c3\\|\\c4\\|\\c6\\|\\c7\\|\\c8\\|\\c9\\)" 
;;;    (format "%c" ch))
  ;; 0: consonant
  ;; 1: base (independent) vowel
  ;; 2: upper diacritical mark (including upper vowel)
  ;; 3: lower diacritical mark (including lower vowel)
  ;; 4: tone mark
  ;; 5: symbol
  ;; 6: digit
  ;; 7: vowel-modifying diacritical mark
  ;; 8: vowel-signs
  ;; 9: semivowel lower
  (let ((ch (ensure-character ch)))
    (or (and (char<= (character "A") ch) (char<= ch (character "Z")))
        (and (char<= (character "a") ch) (char<= ch (character "z")))
        (and (char<= (character "à") ch) (char<= ch (character "ö")))
        (and (char<= (character "ø") ch) (char<= ch (character "ö")))
        (and (char<= (character "ø") ch) (char<= ch (character "ÿ"))))))

(defun alphanumericp (ch)
  "
COMMON-LISP
IMPLEMENTATION: Assumes ISO-8859-1!
"
  (let ((ch (ensure-character ch)))
   (or (digit-char-p ch)
       (alpha-char-p ch))))


(defun* cl:digit-char-p (char &optional (radix 10))
  (let ((value (position (upcase char) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (and value (< value radix) value)))

(defun* cl:parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  (let ((end    (or end (length string)))
        (n      0)
        (sign   1)
        (plus   (character "+"))
        (minus  (character "-"))
        (space  (character " ")))
    (labels ((parse-integer-error ()
               (error "Not an integer string %S (:start %d :end %d :radix %d)"
                      string start end radix))
             (check-range ()
               (unless (< start end)
                 (parse-integer-error)))
             (eat-spaces (i)
               (loop
                 while (and (< i end) (char= space (aref string i)))
                 do (incf i)
                 finally (return i))))
      (setf start (eat-spaces start))
      (check-range)
      (cond
        ((char= plus  (aref string start)) (setf sign +1) (incf start) (check-range))
        ((char= minus (aref string start)) (setf sign -1) (incf start) (check-range)))
      (loop
        for i from start below end
        for digit = (cl:digit-char-p (aref string i) radix)
        while digit
        do (setf n (+ (* n radix) digit))
        finally (when (< i end)
                  (setf i (eat-spaces i)))
                (when (and (not junk-allowed) (< i end))
                  (parse-integer-error))
                (return (values (* sign n) i))))))


;; ---------------
;; - 14 - Conses -
;; ---------------

;; c[ad]{2,4}r are defined in cl-macs.el
;; butlast, nbutlast are primitives in emacs 21.2.
 

;; ---------------
;; - 15 - Arrays -
;; ---------------

;; TODO: We should store the dimensions of the array somewhere!
;; TODO: We should add the fill-pointer!

(defconstant array-dimension-limit 16777216
  "Common-Lisp: A positive fixnum, the exact magnitude of which is
implementation-dependent, but which is not less than 1024.

IMPLEMENTATION: Actually, it seems the limit depends on the available memory
somewhat around 2^27 in 32-bit emacs, or less depending on the current
memory usage.")


(defconstant array-rank-limit 16777216
  "Common-Lisp: A positive fixnum, the exact magnitude of which is
implementation-dependent, but which is not less than 8.

IMPLEMENTATION: Since we make vector of vectors, there's no other limit than
available memory.")


(defun* make-array (dimensions &key element-type initial-element
                               initial-contents adjustable
                               fill-pointer displaced-to
                               displaced-index-offset)
  "Common-Lisp: Creates and returns an array constructed of the most \
specialized type that can accommodate elements of type given by element-type. \
If dimensions is nil then a zero-dimensional array is created.
URL: http://www.informatimago.com/local/lisp/HyperSpec/Body/f_mk_ar.htm#MAKE-ARRAY
RETURN: A vector {of vectors}.
NOTE:   :element-type is ignored.
        :adjustable is ignored.
        :fill-pointer is not supported.
        :displaced-to  is not supported.
        :displaced-index-offset is not supported.
"
  (when (numberp dimensions) (setq dimensions (list dimensions)))
  (when (and displaced-to (or initial-element initial-contents))
    (error (concatenate 'string
             ":displaced-to must not be specified along with "
             ":initial-element or :initial-contents.")))
  (when (and initial-element (or displaced-to initial-contents))
    (error (concatenate 'string
             ":initial-element must not be specified along with "
             " or :displaced-to :initial-contents.")))
  (when (and displaced-index-offset (null displaced-to))
    (error (concatenate 'string
             ":displaced-index-offset must not be specified "
             "without :displaced-to.")))
  (when (and fill-pointer (/= 1 (length dimensions)))
    (error (concatenate 'string
             ":fill-pointer can only be specified for vectors "
             " (uni-dimensional arrays).")))
  (when fill-pointer
    (error "fill-pointers not implemented. Sorry."))
  (when displaced-to
    (error "displaced-to arrays are not implemented. Sorry."))
  (cond
    ((null dimensions)
     (error "a-dimensional arrays are not implemented. Sorry."))
    (initial-contents
     (if  (= 1 (length dimensions))
          (loop with result = (make-vector (car dimensions) initial-element)
             for i from 0 below (length initial-contents)
             do (setf (aref result i) (elt initial-contents i))
             finally return result)
          (loop with result = (make-vector (car dimensions) initial-element)
             for i from 0 below (length initial-contents)
             do (setf (aref result i)
                      (make-array (cdr dimensions)
                                  :initial-contents (elt initial-contents i)
                                  :adjustable adjustable
                                  ;; the rest is not implemented.
                                  ))
             finally return result)))
    (t ;; let's use initial-element
     (if (= 1 (length dimensions))
         (make-vector (car dimensions) initial-element)
         (loop with result = (make-vector (car dimensions) initial-element)
            for i from 0 below (car dimensions)
            do (setf (aref result i)
                     (make-array (cdr dimensions)
                                 :initial-element initial-element
                                 :adjustable adjustable
                                 ;; the rest is not implemented.
                                 ))
            finally return result)))))


(defun array-dimension (array axis-number)
  "Common-Lisp: array-dimension returns the axis-number dimension[1] of array.
Any fill pointer is ignored.
"
  (if (= 0 axis-number)
      (length array)
      (array-dimension (aref array 0) (1- axis-number))))


(defadvice aref
    (around pjb-cl-aref first (array &rest subscripts) activate)
  "Common-Lisp: Accesses the array element specified by the subscripts. \
If no subscripts are supplied and array is zero rank, aref accesses the \
sole element of array.
NOTE: zero rank arrays are not supported.
"
  (setq ad-return-value
        (if (= 1 (length subscripts))
            ad-do-it
            (loop for vector = array then (aref vector index)
               for index in subscripts
               ;;do (printf "vector = %S index = %S\n" vector index)
               finally return vector))))
(ad-activate 'aref)


(defun pjb-cl%%aset (array &rest subscripts-and-value)
  "PRIVATE
DO:     sets the value of the element in array at subcripts.
NOTE:   Used as (defsetf aref pjb-cl%%aset).
"
  (let ((value (car (last subscripts-and-value)))
        (subscripts (butlast subscripts-and-value)))
    (aset (apply 'aref array (butlast subscripts))
          (car (last subscripts)) value)))

(defsetf aref pjb-cl%%aset)



(deftype vector* () '(or string vector))
(defun vectorp* (item) (or (vectorp item) (stringp item)))


;; -----------------
;; - 16 - Strings  -  http://.../cltl/clm/node165.html
;; -----------------


(defun cl:string (x)
  "Common-Lisp: If X is a string, then X, else if it's a symbol, then (cl:symbol-name X).

X---a string, a symbol, or a character.

Returns a string described by x; specifically:

    * If X is a string, it is returned.
    * If X is a symbol, its name is returned.
    * If X is a character, then a string containing that one character is returned.
    * string might perform additional, implementation-defined conversions.

If there are more than one arguments, then emacs string is called.
"
  (cond
    ((stringp x) x)
    ((symbolp x) (cl:symbol-name x))
    ((characterp x) (make-string* 1 :initial-element x))
    (t (signal 'type-error "Expected a string, a symbol or a character."))))



(defun char (str index)
  "Common-Lisp: The character at position index of the string is returned \
as a character object.

RETURN: [cltl2] The given index must be a non-negative integer less
        than the length of string, which must be a string. The
        character at position index of the string is returned as a
        character object.
"
  (setq str (cl:string str))
  (aref (the string str) index))


(defsetf char aset)


(defun schar (simple-string index)
  "Common-Lisp: The character at position index of the string is returned \
as a character object.

RETURN: [cltl2] The given index must be a non-negative integer less
        than the length of string, which must be a string. The
        character at position index of the string is returned as a
        character object. (This character will necessarily satisfy the
        predicate string-char-p.)
"
  (setq simple-string (cl:string simple-string))
  (aref simple-string index))


(defsetf schar aset)

(defun cl:string-equal (string1 string2 &rest cl-keys)
  (setq string1 (cl:string string1)
        string2 (cl:string string2))
  (let ((start1 (or (cadr (memq :start1 cl-keys)) 0))
        (end1   (or (cadr (memq :end1   cl-keys)) (length string1)))
        (start2 (or (cadr (memq :start2 cl-keys)) 0))
        (end2   (or (cadr (memq :end2   cl-keys)) (length string2))))
    (if (/= (- end1 start1) (- end2 start2))
        nil
        (string-equal
         (string-upcase (substring string1 start1 end1))
         (string-upcase (substring string2 start2 end2))))))


(defun cl:string-lessp (string1 string2 &rest cl-keys)
  (setq string1 (cl:string string1)
        string2 (cl:string string2))
  (let ((start1 (or (cadr (memq :start1 cl-keys)) 0))
        (end1   (or (cadr (memq :end1   cl-keys)) (length string1)))
        (start2 (or (cadr (memq :start2 cl-keys)) 0))
        (end2   (or (cadr (memq :end2   cl-keys)) (length string2)))
        )
    (if (/= (- end1 start1) (- end2 start2))
        nil
        (string-lessp
         (string-upcase (substring string1 start1 end1))
         (string-upcase (substring string2 start2 end2))))))


(defun string-greaterp* (string1 string2 &rest cl-keys)
  (not (string-not-lessp
        string2 string1
        :start1 (or (cadr (memq :start2 cl-keys)) nil)
        :end1   (or (cadr (memq :end2   cl-keys)) (length string2))
        :start2 (or (cadr (memq :start1 cl-keys)) nil)
        :end2   (or (cadr (memq :end1   cl-keys)) (length string1)))))



(defun string-not-lessp (string1 string2 &rest cl-keys)
  (not (apply (function string-lessp) string1 string2 cl-keys)))


(defun string-not-greaterp (string1 string2 &rest cl-keys)
  (not (apply (function string-greaterp) string1 string2 cl-keys)))


(defun string-not-equal (string1 string2 &rest cl-keys)
  (not (apply (function string-equal) string1 string2 cl-keys)))



(defun cl:string= (string1 string2 &rest cl-keys)
  ;; &key :start1 :end1 :start2 :end2)
  "Common-Lisp: compares two strings and is true if they are the \
same (corresponding characters are identical) but is false if they are not.

DO:     [cltl2] string= compares two strings and is true if they are
        the same (corresponding characters are identical) but is false
        if they are not. The function equal calls string= if applied
        to two strings.

        The keyword arguments :start1 and :start2 are the places in
        the strings to start the comparison. The arguments :end1 and
        :end2 are the places in the strings to stop comparing;
        comparison stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is examined. These
        arguments are provided so that substrings can be compared
        efficiently.

        string= is necessarily false if the (sub)strings being
        compared are of unequal length; that is, if (not (= (- end1
        start1) (- end2 start2)))  is true, then string= is false.
"
  ;; TODO: should use compare-string
  (setq string1 (cl:string string1)
        string2 (cl:string string2))
  (let ((start1 (or (cadr (memq :start1 cl-keys)) 0))
        (end1   (or (cadr (memq :end1   cl-keys)) (length string1)))
        (start2 (or (cadr (memq :start2 cl-keys)) 0))
        (end2   (or (cadr (memq :end2   cl-keys)) (length string2))))
    (if (/= (- end1 start1) (- end2 start2))
        nil
        (string-equal (substring string1 start1 end1)
                      (substring string2 start2 end2)))))


(defun cl:string< (string1 string2 &rest cl-keys)
  ;; &key :start1 :end1 :start2 :end2)
  "Common-Lisp: compares two strings.

        The keyword arguments :start1 and :start2 are the places in
        the strings to start the comparison. The arguments :end1 and
        :end2 are the places in the strings to stop comparing;
        comparison stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is examined. These
        arguments are provided so that substrings can be compared
        efficiently.
"
  ;; TODO: should use compare-string
  (setq string1 (cl:string string1)
        string2 (cl:string string2))
  (let ((start1 (or (cadr (memq :start1 cl-keys)) 0))
        (end1   (or (cadr (memq :end1   cl-keys)) (length string1)))
        (start2 (or (cadr (memq :start2 cl-keys)) 0))
        (end2   (or (cadr (memq :end2   cl-keys)) (length string2))))
    (string-lessp (substring string1 start1 end1)
                  (substring string2 start2 end2))))


(defun cl:string> (string1 string2 &rest cl-keys)
  ;; &key :start1 :end1 :start2 :end2)
  "Common-Lisp: compares two strings.

        The keyword arguments :start1 and :start2 are the places in
        the strings to start the comparison. The arguments :end1 and
        :end2 are the places in the strings to stop comparing;
        comparison stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is examined. These
        arguments are provided so that substrings can be compared
        efficiently.
"
  ;; TODO: should use compare-string
  (setq string1 (cl:string string1)
        string2 (cl:string string2))
  (let ((start1 (or (cadr (memq :start1 cl-keys)) 0))
        (end1   (or (cadr (memq :end1   cl-keys)) (length string1)))
        (start2 (or (cadr (memq :start2 cl-keys)) 0))
        (end2   (or (cadr (memq :end2   cl-keys)) (length string2))))
    (string-lessp (substring string2 start2 end2)
                  (substring string1 start1 end1))))


(defun cl:string<= (string1 string2 &rest cl-keys)
  ;; &key :start1 :end1 :start2 :end2)
  "Common-Lisp: compares two strings.

        The keyword arguments :start1 and :start2 are the places in
        the strings to start the comparison. The arguments :end1 and
        :end2 are the places in the strings to stop comparing;
        comparison stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is examined. These
        arguments are provided so that substrings can be compared
        efficiently.
"
  ;; TODO: should use compare-string
  (setq string1 (cl:string string1)
        string2 (cl:string string2))
  (let ( (start1 (or (cadr (memq :start1 cl-keys)) 0))
         (end1   (or (cadr (memq :end1   cl-keys)) (length string1)))
         (start2 (or (cadr (memq :start2 cl-keys)) 0))
         (end2   (or (cadr (memq :end2   cl-keys)) (length string2))))
    (not (string-lessp (substring string2 start2 end2)
                       (substring string1 start1 end1)))))


(defun cl:string>= (string1 string2 &rest cl-keys)
  ;; &key :start1 :end1 :start2 :end2)
  "Common-Lisp: compares two strings.

        The keyword arguments :start1 and :start2 are the places in
        the strings to start the comparison. The arguments :end1 and
        :end2 are the places in the strings to stop comparing;
        comparison stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is examined. These
        arguments are provided so that substrings can be compared
        efficiently.
"
  ;; TODO: should use compare-string
  (setq string1 (cl:string string1)
        string2 (cl:string string2))
  (let ((start1 (or (cadr (memq :start1 cl-keys)) 0))
        (end1   (or (cadr (memq :end1   cl-keys)) (length string1)))
        (start2 (or (cadr (memq :start2 cl-keys)) 0))
        (end2   (or (cadr (memq :end2   cl-keys)) (length string2))))
    (not (string-lessp (substring string1 start1 end1)
                       (substring string2 start2 end2)))))


(defun cl:string/= (string1 string2 &rest cl-keys)
  ;; &key :start1 :end1 :start2 :end2)
  "Common-Lisp: compares two strings and return whether they're different.

        The keyword arguments :start1 and :start2 are the places in
        the strings to start the comparison. The arguments :end1 and
        :end2 are the places in the strings to stop comparing;
        comparison stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is examined. These
        arguments are provided so that substrings can be compared
        efficiently.
"
  ;; TODO: should use compare-string
  (not (apply 'string= string1 string2 cl-keys)))


(defun* make-string* (size &key (initial-element 0) (element-type 'character))
  "Common-Lisp: MAKE-STRING takes a key :initial-element argument
"
  (make-string size initial-element))


(defun string-trim (character-bag string-designator)
  "Common-Lisp: returns a substring of string, with all characters in \
character-bag stripped off the beginning and end.
"
  (unless (sequencep character-bag)
    (signal 'type-error  "Expected a sequence for `character-bag'."))
  (let* ((string (cl:string string-designator))
         (margin (format "[%s]*" (regexp-quote
                                  (if (stringp character-bag)
                                      character-bag
                                      (map 'string 'identity character-bag)))))
         (trimer (format "\\`%s\\(\\(.\\|\n\\)*?\\)%s\\'" margin margin)))
    (replace-regexp-in-string  trimer "\\1" string)))


(defun string-left-trim (character-bag string-designator)
  "Common-Lisp: returns a substring of string, with all characters in \
character-bag stripped off the beginning.

"
  (unless (sequencep character-bag)
    (signal 'type-error  "Expected a sequence for `character-bag'."))
  (let* ((string (cl:string string-designator))
         (margin (format "[%s]*" (regexp-quote
                                  (if (stringp character-bag)
                                      character-bag
                                      (map 'string 'identity character-bag)))))
         (trimer (format "\\`%s\\(\\(.\\|\n\\)*?\\)\\'" margin)))
    (replace-regexp-in-string  trimer "\\1" string)))


(defun string-right-trim (character-bag string-designator)
  "Common-Lisp: returns a substring of string, with all characters in \
`character-bag' stripped off the end.

"
  (unless (sequencep character-bag)
    (signal 'type-error  "Expected a sequence for `character-bag'."))
  (let* ((string (cl:string string-designator))
         (margin (format "[%s]*" (regexp-quote
                                  (if (stringp character-bag)
                                      character-bag
                                      (map 'string 'identity character-bag)))))
         (trimer (format "\\`\\(\\(.\\|\n\\)*?\\)%s\\'" margin)))
    (replace-regexp-in-string  trimer "\\1" string)))


(defun string-upcase (str &rest cl-keys)
  ;; &key :start :end
  "Common-Lisp: returns a string just like `str' with all lowercase \
characters replaced by the corresponding uppercase characters.

        The keyword argument :start specifies the place in the string
        where to start the upcase. The argument :end specifies
        the place in the string where to stop upcase;
        upcase stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is upcased.
"
  (setq str (cl:string str))
  (let ( (start (or (cadr (memq :start cl-keys)) 0))
        (end   (or (cadr (memq :end   cl-keys)) (length str))) )
    (concat (substring str 0 start)
            (upcase (substring str start end))
            (substring str end))))


(defun string-downcase (str &rest cl-keys)
  ;; &key :start :end
  "Common-Lisp: returns a string just like `str' with all uppercase \
characters replaced by the corresponding lowercase characters.

        The keyword argument :start specifies the place in the string
        where to start the downcase. The argument :end specifies
        the place in the string where to stop downcase;
        downcase stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is downcased.
"
  (setq str (cl:string str))
  (let ( (start (or (cadr (memq :start cl-keys)) 0))
        (end   (or (cadr (memq :end   cl-keys)) (length str))) )
    (concat (substring str 0 start)
            (downcase (substring str start end))
            (substring str end))))


(defun string-capitalize (str &rest cl-keys)
  ;; &key :start :end
  "Common-Lisp: returns a string just like `str' with all words capitalized.

        The keyword argument :start specifies the place in the string
        where to start the capitalization. The argument :end specifies
        the place in the string where to stop capitalization;
        capitalization stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is capitalized.
"
  (setq str (cl:string str))
  (let ( (start (or (cadr (memq :start cl-keys)) 0))
        (end   (or (cadr (memq :end   cl-keys)) (length str))) )
    (concat (substring str 0 start)
            (capitalize (substring str start end))
            (substring str end))))


(defun nstring-upcase (str &rest cl-keys)
  ;; &key :start :end
  "Common-Lisp: modify `str' replacing lowercase characters \
by the corresponding uppercase characters.

        The keyword argument :start specifies the place in the string
        where to start the upcase. The argument :end specifies
        the place in the string where to stop upcase;
        upcase stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is upcased.
"
  (unless (stringp str) (signal 'type-error "Expected a string."))
  (let ( (start (or (cadr (memq :start cl-keys)) 0))
        (end   (or (cadr (memq :end   cl-keys)) (length str))) )
    (do ((i start (1+ i))) ((<= end i) str)
      (setf (aref str i) (upcase (aref str i))))))


(defun nstring-downcase (str &rest cl-keys)
  ;; &key :start :end
  "Common-Lisp: modify `str' replacing uppercase characters \
by the corresponding lowercase characters.

        The keyword argument :start specifies the place in the string
        where to start the downcase. The argument :end specifies
        the place in the string where to stop downcase;
        downcase stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is downcased.
"
  (unless (stringp str) (signal 'type-error "Expected a string."))
  (let ( (start (or (cadr (memq :start cl-keys)) 0))
        (end   (or (cadr (memq :end   cl-keys)) (length str))) )
    (do ((i start (1+ i))) ((<= end i) str)
      (setf (aref str i) (downcase (aref str i))))))


(defun nstring-capitalize (str &rest cl-keys)
  ;; &key :start :end
  "Common-Lisp: modify `str' capitalizing words.

        The keyword argument :start specifies the place in the string
        where to start the capitalization. The argument :end specifies
        the place in the string where to stop capitalization;
        capitalization stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is capitalized.
"
  (unless (stringp str) (signal 'type-error "Expected a string."))
  (let* ( (start (or (cadr (memq :start cl-keys)) 0))
         (end   (or (cadr (memq :end   cl-keys)) (length str)))
          (capit (capitalize (substring str start end)))
          (len   (length capit)) )
    (do ((i 0 (1+ i))) ((<= len i) str)
      (setf (aref str (+ start i)) (aref capit i)))))



;; --------------------------------
;; - 17 - Sequences
;; --------------------------------

(defclass iterator () ())
(defclass iterator-list   (iterator) ((head   :initarg :sequence :type list)))
(defclass iterator-vector (iterator) ((vector :initarg :sequence :type VECTOR)
                                      (index :initform 0)))


(defmethod end-of-sequence-p ((self iterator-list))
  (null (slot-value self 'head)))


(defmethod end-of-sequence-p ((self iterator-vector))
  (>= (slot-value self 'index) (length (slot-value self 'vector))))


(defmethod current-item ((self iterator-list))
  (car (slot-value self 'head)))


(defmethod current-item ((self iterator-vector))
  (aref (slot-value self 'vector) (slot-value self 'index)))


(defmethod set-current-item ((self iterator-list) value)
  (setf (car (slot-value self 'head)) value))


(defmethod set-current-item ((self iterator-vector) value)
  (setf (aref (slot-value self 'vector) (slot-value self 'index)) value))


(defmethod advance ((self iterator-list))
  (setf (slot-value self 'head) (cdr (slot-value self 'head))))


(defmethod advance ((self iterator-vector))
  (incf (slot-value self 'index)))


(defun map-into (result-sequence function &rest sequences)
  (cond
    ((every (function listp) sequences)
     (cond
       ((listp result-sequence)
        (do ((sequences sequences (mapcar (function cdr) sequences))
             (target result-sequence (cdr target)))
            ((or (null target) (some (function null) sequences)) result-sequence)
          (setf (car target) (apply function
                                    (mapcar (function car) sequences)))))
       ((vectorp* result-sequence)
        (do ((sequences sequences (mapcar (function cdr) sequences))
             (target 0 (1+ target)))
            ((or (>= target (length result-sequence))
                 (some (function null) sequences)) result-sequence)
          (setf (aref result-sequence target)
                (apply function (mapcar (function car) sequences)))))
       (t (error "RESULT-SEQUENCE is neither a LIST or a VECTOR."))))
    ((every (function vectorp*) sequences)
     (cond
       ((listp result-sequence)
        (do ((source 0 (1+ source))
             (min (apply (function min) (mapcar (function length) sequences)))
             (target result-sequence (cdr target)))
            ((or (null target) (>= source min)) result-sequence)
          (setf (car target) 
                (apply function (mapcar (lambda (seq) (aref seq source))
                                        sequences)))))
       ((vectorp* result-sequence)
        (do ((index 0 (1+ index))
             (min (apply (function min) (length result-sequence)
                         (mapcar (function length) sequences))))
            ((>= index min) result-sequence)
          (setf (aref result-sequence index)
                (apply function (mapcar (lambda (seq) (aref seq index))
                                        sequences)))))
       (t (error "RESULT-SEQUENCE is neither a LIST or a VECTOR."))))
    (t
     (do ((res
           (make-instance
               (cond 
                 ((listp    result-sequence) 'iterator-list)
                 ((vectorp* result-sequence) 'iterator-vector)
                 (t (error "RESULT-SEQUENCE is neither a LIST or a VECTOR.")))
             :sequence result-sequence))
          (sequences
           (mapcar
            (lambda (seq)
              (make-instance
                  (cond 
                    ((listp    seq) 'iterator-list)
                    ((vectorp* seq) 'iterator-vector)
                    (t (error "A SEQUENCE is neither a LIST or a VECTOR.")))
                :sequence seq)) sequences)))
         ((some (function end-of-sequence-p) (cons res sequences))
          result-sequence)
       (set-current-item res (apply function
                                    (mapcar (function current-item) sequences)))
       (dolist (seq (cons res sequences)) (advance seq))))))


;; (MAP-INTO [0 0 0 0] (function +) "ABCD"       '(1 1 1))
;; (MAP-INTO [0 0 0 0] (function +) '(1 2 3 4)   '(1 1 1))
;; (MAP-INTO [0 0 0 0] (function +) '(1 2 3 4)   '(1 1 1))
;; (MAP-INTO [0 0 0 0] (function +) '(1 2 3 4 5) [1 1 1])
;; (MAP-INTO (copy-seq '(0 0 0 0)) (function +) '(1 2 3 4 5) [1 1 1])
;; (MAP-INTO (copy-seq "------") (function +) '(1 1 1 1) "HAL")
;; (MAP-INTO (copy-seq "------") (function +) [1 1 1 1] "HAL")




;; --------------------------------
;; - 19.4 - Pathnames - Filenames -
;; --------------------------------

(defun namestring (pathname)
  "Common-Lisp: returns the full form of pathname.
These functions convert pathname into a namestring. The name represented by pathname is
returned as a namestring in an implementation-dependent canonical form.
URL:        http://www.informatimago.com/local/lisp/HyperSpec/Body/f_namest.htm
NOTE:       in current implementation pathname=namestring.
"
  pathname)


(defun file-namestring (pathname)
  "Common-Lisp: returns just the name, type, and version components of pathname.
These functions convert pathname into a namestring. The name represented by pathname is
returned as a namestring in an implementation-dependent canonical form.
URL:        http://www.informatimago.com/local/lisp/HyperSpec/Body/f_namest.htm
NOTE:       in current implementation pathname=namestring.
RETURN:     the 'basename' of the pathname.
"
  ;; (if (string-match "\\(^.*/\\([^/][^/]*\\)/*$\\)\\|\\(^\\([^/][^/]*\\)/*$\\)"
  ;;                   pathname)
  ;;     (let ((res (match-string 2 pathname)))
  ;;       (if res res (match-string 4 pathname)))
  ;;     pathname)
  (file-name-nondirectory pathname))


(defun directory-namestring (pathname)
  "Common-Lisp: returns the directory name portion.
These functions convert pathname into a namestring. The name represented by pathname is
returned as a namestring in an implementation-dependent canonical form.
URL:        http://www.informatimago.com/local/lisp/HyperSpec/Body/f_namest.htm
NOTE:       in current implementation pathname=namestring.
RETURN:     the 'basename' of the pathname.
"
  ;; (if (string-match "\\(^.*[^/].*\\)/[^/][^/]*/*$" pathname)
  ;;     (match-string 1 pathname)
  ;;     (if (= ?/ (aref pathname 0)) "/" "."))
  (file-name-directory pathname))


(defun host-namestring (pathname)
  "Common-Lisp: returns the host name.
These functions convert pathname into a namestring. The name represented by pathname is
returned as a namestring in an implementation-dependent canonical form.
URL:        http://www.informatimago.com/local/lisp/HyperSpec/Body/f_namest.htm
NOTE:       in current implementation pathname=namestring.
RETURN:     (system-name)
"
  (system-name))


(defun pathname-host      (path) nil)
(defun pathname-directory (path) (directory-namestring path))


(defun pathname-name (path)
  (setq path (file-namestring path))
  (cond ((string-match "^\\(.*\\)\\.\\([^.]*\\)\\(\\.~[0-9]+~\\)$" path)
         (match-string 1 path))
        ((string-match "^\\(.*\\)\\.\\([^.]*\\)$" path)
         (match-string 1 path))
        (t :unspecific)))


;; (mapcar (lambda (x) (list (dirname x) (basename x) (pathname-name x)))
;;         '("abc" "abc.def" "abc.def.ghi"
;;           "/abc" "/abc.def" "/abc.def.ghi"
;;           "./abc" "./abc.def" "./abc.def.ghi"
;;           "ddd/abc" "ddd/abc.def" "ddd/abc.def.ghi"
;;           "eee/ddd/abc" "eee/ddd/abc.def" "eee/ddd/abc.def.ghi"
;;           "/eee/ddd/abc" "/eee/ddd/abc.def" "/eee/ddd/abc.def.ghi"))



(defun pathname-type (path)
  "RETURN: The file name extension (excluding the version number .~N~)."
  (cond ((string-match ".*\\.\\([^.]*\\)\\(\\.~[0-9]+~\\)$" path)
         (match-string 1 path))
        ((string-match ".*\\.\\([^.]*\\)$" path)
         (match-string 1 path))
        (t :unspecific)))


(defun pathname-version (path)
  "RETURN: The version number."
  (if (string-match ".*\\.[^\\.]*?\\.~\\([0-9]+\\)~$" path)
      (nth-value 0 (cl:parse-integer (match-string 1 path)))
      :unspecific))


;;; (defun enough-namestring (pathname &optional defaults)
;;;   "Common-Lisp: returns an abbreviated namestring that is just sufficient to identify the \
;;; file named by pathname when considered relative to the defaults.

;;; It is required that:

;;;       (merge-pathnames (enough-namestring pathname defaults) defaults)
;;;   ==  (merge-pathnames (parse-namestring pathname nil defaults) defaults)

;;; in all cases, and the result of enough-namestring is the shortest reasonable string that will
;;; satisfy this criterion.

;;; It is not necessarily possible to construct a valid namestring by concatenating some of the
;;; three shorter namestrings in some order.

;;; These functions convert pathname into a namestring. The name represented by pathname is
;;; returned as a namestring in an implementation-dependent canonical form.

;;; URL:        http://www.informatimago.com/local/lisp/HyperSpec/Body/f_namest.htm
;;; NOTE:       in current implementation pathname=namestring.
;;; RETURN:     (system-name)
;;; "
;;;   )



;; --------------
;; - 20 - Files -
;; --------------


(defun* directory (pathspec &key relative-paths)
  "Common-Lisp: Determines which, if any, files that are present in the \
file system have names matching pathspec, and returns a fresh list of \
pathnames corresponding to the truenames of those files.
pathspec:  a pathname designator, which may contain wild components.
pathnames: a list of physical pathnames.
NOTE:      An implementation may be extended to accept implementation-defined
           keyword arguments to directory.
NOTE:      this implementation accepts an :relative-paths  key
           that can be set to t to avoid forcing absolute paths result.
URL:       http://www.informatimago.com/local/lisp/HyperSpec/Body/f_dir.htm
"
  (when (search "**" pathspec)
    (error "CL ** wild-inferior is not implemented."))
  (file-expand-wildcards pathspec (not relative-paths)))


(defun probe-file (pathspec)
  "Common-Lisp: Returns false if there is no file named pathspec, and otherwise returns the truename of pathspec.
RETURN:  nil, or pathspec (we don't distinguish truename for now).
URL:     http://www.informatimago.com/local/lisp/HyperSpec/Body/f_probe_.htm
"
  (if (file-exists-p pathspec) pathspec nil))


(defun* ensure-directories-exist (pathspec &key verbose)
  (declare (ignore verbose))
  (make-directory pathspec t))


(defun truename (filespec)
  "
RETURN:  The absolute path name corresponding to fielspec.
"
  (car (file-expand-wildcards (shell-quote-argument filespec) t)))



(defun file-author (pathspec)
  "Common-Lisp: Returns a string naming the author of the file specified \
by pathspec, or nil if the author's name cannot be determined.
RETURN:  nil (we cannot determine no author on unix systems.
URL:     http://www.informatimago.com/local/lisp/HyperSpec/Body/f_file_a.htm#file-author
"
  nil)


(defun file-write-date (pathspec)
  "Common-lisp: Returns a universal time representing the time at which \
the file specified by pathspec was last written (or created), \
or returns nil if such a time cannot be determined.
URL:     http://www.informatimago.com/local/lisp/HyperSpec/Body/f_file_w.htm
NOTE:    Current implementation does not accept a stream for a pathspec.
"
  (let ((attributes (file-attributes pathspec)))
    (unless attributes
      ;; TODO: see the error processing. We should signal a file-error here.
      ;; (signal 'file-error
      ;;  (FORMAT nil "file-write-date cannot get attributes for file ~S"
      ;;          pathspec))
      (error "file-write-date cannot get attributes for file %S" pathspec))
    (+ pjb-cl%%epoch
       (pjb-cl%%emacs-time-to-seconds
        (nth 5 attributes)))))


;; TODO: rename-file exists but does not return the same.
;; http://www.informatimago.com/local/lisp/HyperSpec/Body/f_rn_fil.htm

;; TODO: delete-file exists but does not return the same.
;; http://www.informatimago.com/local/lisp/HyperSpec/Body/f_del_fi.htm

;; file-error
;; http://www.informatimago.com/local/lisp/HyperSpec/Body/e_file_e.htm

;; file-error-pathname (condition)
;; http://www.informatimago.com/local/lisp/HyperSpec/Body/f_file_e.htm



;; ----------------
;; - 21 - Streams -
;; ----------------


(defvar *debug-io*        :stderr
  "The value of *DEBUG-IO*, called debug I/O, is a stream to be used for
interactive debugging purposes.

Bi-directional stream.")


(defvar *error-output*    :stderr
  "The value of *ERROR-OUTPUT*, called error output, is a stream to which
warnings and non-interactive error messages should be sent.

Output stream.")


(defvar *query-io*        t
  "The value of *QUERY-IO*, called query I/O, is a bidirectional stream
to be used when asking questions of the user. The question should be
output to this stream, and the answer read from it.

Bi-directional stream.")


(defvar *standard-input*  t
  "The value of *STANDARD-INPUT*, called standard input, is a stream that
is used by many operators as a default source of input when no
specific input stream is explicitly supplied.

Input stream.")


(defvar *standard-output* :stdout
  "The value of *STANDARD-OUTPUT*, called standard output, is a stream
that is used by many operators as a default destination for output
when no specific output stream is explicitly supplied.

Output stream.")


(defvar *trace-output*    :stderr
  "The value of *TRACE-OUTPUT*, called trace output, is the stream on
which traced functions (see trace) and the time macro print their
output.

Output stream.")


(defvar *terminal-io*    t
  "The value of *TERMINAL-IO*, called terminal I/O, is ordinarily a
bidirectional stream that connects to the user's console. Typically,
writing to this stream would cause the output to appear on a display
screen, for example, and reading from the stream would accept input
from a keyboard. It is intended that standard input functions such as
read and read-char, when used with this stream, cause echoing of the
input into the output side of the stream. The means by which this is
accomplished are implementation-dependent.

Bi-directional stream.")



;; --------------------
;; - 25 - Environment -
;; --------------------

;; Time functions -  http://.../cltl/clm/node232.html


(defun get-internal-real-time ()
  (destructuring-bind (high low microsec &rest ignored) (current-time)
    (declare (ignore ignored))
    (+ (* high 65536.0) low (* 1e-6 microsec))))


(defmacro time (&rest body)
  "Common-Lisp:  time evaluates form in the current environment (lexical and \
dynamic). A call to time can be compiled.
DO:      time prints various timing data and other information to trace output.
         The nature and format of the printed information is
         implementation-defined . Implementations are encouraged to provide
         such information as elapsed real time, machine run time, and storage
         management statistics.
URL: http://www.informatimago.com/local/lisp/HyperSpec/Body/m_time.htm
"
  (let ((start (gensym))
        (result (gensym))
        (stop (gensym))
        (time (gensym)))
    `(let* ((,start  (get-internal-real-time))
            (,result (progn ,@body))
            (,stop   (get-internal-real-time)) 
            (,time   (- ,stop ,start)) )
       (printf *trace-output* "Time: %e ms\n" ,time))))


(defun sleep (seconds)
  "Common-Lisp: Sleeps a specified number of seconds.

DO:     (sleep n) causes execution to cease and become dormant for
        approximately n seconds of real time, whereupon execution is
        resumed. The argument may be any non-negative non-complex
        number. sleep returns nil.
"
  (sleep-for seconds))


(defmacro trace (&rest function-names)
  "Common-Lisp:"
  (map nil (function trace-function-background) function-names))


(defmacro untrace (&rest function-names)
  "Common-Lisp:"
  (map nil (function untrace-function) function-names))


;;- Other Environment Inquiries -  http://.../cltl/clm/node233.html

(defun lisp-implementation-type ()
  "Common-Lisp: A string is returned that identifies this Common Lisp implementation.

RETURN: \"emacs\"
"
  "emacs")


(defun lisp-implementation-version ()
  "Common-Lisp: A string is returned that identifies the version of this Common Lisp implementation.

RETURN: emacs-version.
"
  emacs-version)


(defun machine-type ()
  "Common-Lisp: A string is returned that identifies the generic name of the computer hardware.

RETURN: The first part of system-configuration

NOTE:   This is  not exactly the machine-type, but rather, the type of
        machine this emacs was compiled for.
        More information could be got with (shell-command ...) and OS-specific
        commands.
"
  (substring system-configuration 0
             (string-match "-" system-configuration 0)))


(defun machine-version ()
  "Common-Lisp: A string is returned that identifies the version of the \
computer hardware.

RETURN: The same as (machine-type). Sorry about that.
"
  (machine-type))


(defun machine-instance ()
  "Common-Lisp: A string is returned that identifies the particular instance \
of the computer hardware on which Common Lisp is running.

RETURN: (system-name)
"
  (system-name))


(defun software-type ()
  "Common-Lisp: A string is returned that identifies the generic name of \
any relevant supporting software.

RETURN: The rest of system-configuration.
"
  (substring system-configuration
             (1+ (string-match "-" system-configuration 0))))


(defun software-version ()
  "Common-Lisp: A string is returned that identifies the version of any \
relevant supporting software; this information should be of use to \
maintainers of the implementation.

RETURN: \"\"
"
  emacs-version)


(defun short-site-name ()
  "Common-Lisp: A string is returned that identifies the physical location \
of the computer hardware.

RETURN: (machine-instance).
"
  (machine-instance))


(defun long-site-name ()
  "Common-Lisp: A string is returned that identifies the physical location \
of the computer hardware.

RETURN: (machine-instance).
"
  (machine-instance))


(defun dirname (path)
  (if (string-match "^\\(.*/\\)\\([^/]*\\)$" path)
      (match-string 1 path)
      "./"))

(defun basename (path &optional extension)
  (let ((extension (or extension "")))
    (if (string-match (format "^\\(.*/\\)\\([^/]*\\)%s$" (regexp-quote extension)) path)
        (match-string 2 path)
        path)))

(defvar cl::*user-homedir-pathname*)
(setf cl::*user-homedir-pathname*
      (cond (user-init-file  (dirname user-init-file))
            ((getenv "HOME") (concat (getenv "HOME") "/"))
            (t               (dirname (first (file-expand-wildcards "~/.emacs"))))))
(defun user-homedir-pathname (&optional host) cl::*user-homedir-pathname*)


;; ------------------------------------------------------------------------


(defun cl-indent (symbol num-forms)
  "
Put on the SYMBOL and its lower case and upper case variants
a 'lisp-indent-function property set to NUM-FORMS.
"
  (dolist (property '(lisp-indent-function common-lisp-indent-function))
    (put symbol property num-forms)
    (put (intern (string-downcase (cl:symbol-name symbol))) property num-forms)
    (put (intern (string-upcase   (cl:symbol-name symbol))) property num-forms)))



(defun loop-doc (&rest args)
  "(loop CLAUSE...): The Common Lisp `loop' macro.
Valid clauses are:

    for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM
    for VAR in LIST by FUNC
    for VAR on LIST by FUNC
    for VAR = INIT then EXPR
    for VAR across ARRAY
    with VAR = INIT

 Miscellaneous Clauses:

    named NAME
    initially EXPRS...

 Accumulation Clauses:

    collect EXPR into VAR
    append EXPR into VAR
    nconc EXPR into VAR
    sum EXPR into VAR
    count EXPR into VAR
    maximize EXPR into VAR
    minimize EXPR into VAR

 Termination Test Clauses:

    repeat NUM
    while COND
    until COND
    always COND
    never COND
    thereis COND

 Unconditional Execution Clause:

    do EXPRS...

 Conditional Execution Clauses:

    if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]

 Miscellaneous Clauses:

    finally EXPRS...
    return EXPR
    finally return EXPR

;;; (loop for i in '(1 2 3 4)
;;;       collect i into col
;;;       append  i into app
;;;       nconc   i into nco
;;;       sum     i into sum
;;;       count   i into cnt
;;;       maximize i into max
;;;       minimize i into min
;;;       do (printf \"%d \" i)
;;;       return (progn (printf \"\n\" i)
;;;                     (values col app nco sum cnt max min)))
")




(defconst +common-lisp-external-symbols+
  '(
    &ALLOW-OTHER-KEYS *PRINT-MISER-WIDTH* &AUX *PRINT-PPRINT-DISPATCH*
    &BODY *PRINT-PRETTY* &ENVIRONMENT *PRINT-RADIX* &KEY *PRINT-READABLY*
    &OPTIONAL *PRINT-RIGHT-MARGIN* &REST *QUERY-IO* &WHOLE *RANDOM-STATE*
    * *READ-BASE* ** *READ-DEFAULT-FLOAT-FORMAT* *** *READ-EVAL*
    *BREAK-ON-SIGNALS* *READ-SUPPRESS* *COMPILE-FILE-PATHNAME* *READTABLE*
    *COMPILE-FILE-TRUENAME* *STANDARD-INPUT* *COMPILE-PRINT*
    *STANDARD-OUTPUT* *COMPILE-VERBOSE* *TERMINAL-IO* *DEBUG-IO*
    *TRACE-OUTPUT* *DEBUGGER-HOOK* + *DEFAULT-PATHNAME-DEFAULTS* ++
    *ERROR-OUTPUT* +++ *FEATURES* - *GENSYM-COUNTER* / *LOAD-PATHNAME* //
    *LOAD-PRINT* /// *LOAD-TRUENAME* /= *LOAD-VERBOSE* 1+
    *MACROEXPAND-HOOK* 1- *MODULES* < *PACKAGE* <= *PRINT-ARRAY* =
    *PRINT-BASE* > *PRINT-CASE* >= *PRINT-CIRCLE* ABORT *PRINT-ESCAPE* ABS
    *PRINT-GENSYM* ACONS *PRINT-LENGTH* ACOS *PRINT-LEVEL* ACOSH
    *PRINT-LINES* ADD-METHOD ADJOIN ATOM BOUNDP ADJUST-ARRAY BASE-CHAR
    BREAK ADJUSTABLE-ARRAY-P BASE-STRING BROADCAST-STREAM
    ALLOCATE-INSTANCE BIGNUM BROADCAST-STREAM-STREAMS ALPHA-CHAR-P BIT
    BUILT-IN-CLASS ALPHANUMERICP BIT-AND BUTLAST AND BIT-ANDC1 BYTE APPEND
    BIT-ANDC2 BYTE-POSITION APPLY BIT-EQV BYTE-SIZE APROPOS BIT-IOR CAAAAR
    APROPOS-LIST BIT-NAND CAAADR AREF BIT-NOR CAAAR ARITHMETIC-ERROR
    BIT-NOT CAADAR ARITHMETIC-ERROR-OPERANDS BIT-ORC1 CAADDR
    ARITHMETIC-ERROR-OPERATION BIT-ORC2 CAADR ARRAY BIT-VECTOR CAAR
    ARRAY-DIMENSION BIT-VECTOR-P CADAAR ARRAY-DIMENSION-LIMIT BIT-XOR
    CADADR ARRAY-DIMENSIONS BLOCK CADAR ARRAY-DISPLACEMENT BOOLE CADDAR
    ARRAY-ELEMENT-TYPE BOOLE-1 CADDDR ARRAY-HAS-FILL-POINTER-P BOOLE-2
    CADDR ARRAY-IN-BOUNDS-P BOOLE-AND CADR ARRAY-RANK BOOLE-ANDC1
    CALL-ARGUMENTS-LIMIT ARRAY-RANK-LIMIT BOOLE-ANDC2 CALL-METHOD
    ARRAY-ROW-MAJOR-INDEX BOOLE-C1 CALL-NEXT-METHOD ARRAY-TOTAL-SIZE
    BOOLE-C2 CAR ARRAY-TOTAL-SIZE-LIMIT BOOLE-CLR CASE ARRAYP BOOLE-EQV
    CATCH ASH BOOLE-IOR CCASE ASIN BOOLE-NAND CDAAAR ASINH BOOLE-NOR
    CDAADR ASSERT BOOLE-ORC1 CDAAR ASSOC BOOLE-ORC2 CDADAR ASSOC-IF
    BOOLE-SET CDADDR ASSOC-IF-NOT BOOLE-XOR CDADR ATAN BOOLEAN CDAR ATANH
    BOTH-CASE-P CDDAAR CDDADR CLEAR-INPUT COPY-TREE CDDAR CLEAR-OUTPUT COS
    CDDDAR CLOSE COSH CDDDDR CLRHASH COUNT CDDDR CODE-CHAR COUNT-IF CDDR
    COERCE COUNT-IF-NOT CDR COMPILATION-SPEED CTYPECASE CEILING COMPILE
    DEBUG CELL-ERROR COMPILE-FILE DECF CELL-ERROR-NAME
    COMPILE-FILE-PATHNAME DECLAIM CERROR COMPILED-FUNCTION DECLARATION
    CHANGE-CLASS COMPILED-FUNCTION-P DECLARE CHAR COMPILER-MACRO
    DECODE-FLOAT CHAR-CODE COMPILER-MACRO-FUNCTION DECODE-UNIVERSAL-TIME
    CHAR-CODE-LIMIT COMPLEMENT DEFCLASS CHAR-DOWNCASE COMPLEX DEFCONSTANT
    CHAR-EQUAL COMPLEXP DEFGENERIC CHAR-GREATERP
    COMPUTE-APPLICABLE-METHODS DEFINE-COMPILER-MACRO CHAR-INT
    COMPUTE-RESTARTS DEFINE-CONDITION CHAR-LESSP CONCATENATE
    DEFINE-METHOD-COMBINATION CHAR-NAME CONCATENATED-STREAM
    DEFINE-MODIFY-MACRO CHAR-NOT-EQUAL CONCATENATED-STREAM-STREAMS
    DEFINE-SETF-EXPANDER CHAR-NOT-GREATERP COND DEFINE-SYMBOL-MACRO
    CHAR-NOT-LESSP CONDITION DEFMACRO CHAR-UPCASE CONJUGATE DEFMETHOD
    CHAR/= CONS DEFPACKAGE CHAR< CONSP DEFPARAMETER CHAR<= CONSTANTLY
    DEFSETF CHAR= CONSTANTP DEFSTRUCT CHAR> CONTINUE DEFTYPE CHAR>=
    CONTROL-ERROR DEFUN CHARACTER COPY-ALIST DEFVAR CHARACTERP COPY-LIST
    DELETE CHECK-TYPE COPY-PPRINT-DISPATCH DELETE-DUPLICATES CIS
    COPY-READTABLE DELETE-FILE CLASS COPY-SEQ DELETE-IF CLASS-NAME
    COPY-STRUCTURE DELETE-IF-NOT CLASS-OF COPY-SYMBOL DELETE-PACKAGE
    DENOMINATOR EQ DEPOSIT-FIELD EQL DESCRIBE EQUAL DESCRIBE-OBJECT EQUALP
    DESTRUCTURING-BIND ERROR DIGIT-CHAR ETYPECASE DIGIT-CHAR-P EVAL
    DIRECTORY EVAL-WHEN DIRECTORY-NAMESTRING EVENP DISASSEMBLE EVERY
    DIVISION-BY-ZERO EXP DO EXPORT DO* EXPT DO-ALL-SYMBOLS EXTENDED-CHAR
    DO-EXTERNAL-SYMBOLS FBOUNDP DO-SYMBOLS FCEILING DOCUMENTATION
    FDEFINITION DOLIST FFLOOR DOTIMES FIFTH DOUBLE-FLOAT FILE-AUTHOR
    DOUBLE-FLOAT-EPSILON FILE-ERROR DOUBLE-FLOAT-NEGATIVE-EPSILON
    FILE-ERROR-PATHNAME DPB FILE-LENGTH DRIBBLE FILE-NAMESTRING
    DYNAMIC-EXTENT FILE-POSITION ECASE FILE-STREAM ECHO-STREAM
    FILE-STRING-LENGTH ECHO-STREAM-INPUT-STREAM FILE-WRITE-DATE
    ECHO-STREAM-OUTPUT-STREAM FILL ED FILL-POINTER EIGHTH FIND ELT
    FIND-ALL-SYMBOLS ENCODE-UNIVERSAL-TIME FIND-CLASS END-OF-FILE FIND-IF
    ENDP FIND-IF-NOT ENOUGH-NAMESTRING FIND-METHOD
    ENSURE-DIRECTORIES-EXIST FIND-PACKAGE ENSURE-GENERIC-FUNCTION
    FIND-RESTART FIND-SYMBOL GET-INTERNAL-RUN-TIME FINISH-OUTPUT
    GET-MACRO-CHARACTER FIRST GET-OUTPUT-STREAM-STRING FIXNUM
    GET-PROPERTIES FLET GET-SETF-EXPANSION FLOAT GET-UNIVERSAL-TIME
    FLOAT-DIGITS GETF FLOAT-PRECISION GETHASH FLOAT-RADIX GO FLOAT-SIGN
    GRAPHIC-CHAR-P FLOATING-POINT-INEXACT HANDLER-BIND
    FLOATING-POINT-INVALID-OPERATION HANDLER-CASE FLOATING-POINT-OVERFLOW
    HASH-TABLE FLOATING-POINT-UNDERFLOW HASH-TABLE-COUNT FLOATP
    HASH-TABLE-P FLOOR HASH-TABLE-REHASH-SIZE FMAKUNBOUND
    HASH-TABLE-REHASH-THRESHOLD FORCE-OUTPUT HASH-TABLE-SIZE FORMAT
    HASH-TABLE-TEST FORMATTER HOST-NAMESTRING FOURTH IDENTITY FRESH-LINE
    IF FROUND IGNORABLE FTRUNCATE IGNORE FTYPE IGNORE-ERRORS FUNCALL
    IMAGPART FUNCTION IMPORT FUNCTION-KEYWORDS IN-PACKAGE
    FUNCTION-LAMBDA-EXPRESSION INCF FUNCTIONP INITIALIZE-INSTANCE GCD
    INLINE GENERIC-FUNCTION INPUT-STREAM-P GENSYM INSPECT GENTEMP INTEGER
    GET INTEGER-DECODE-FLOAT GET-DECODED-TIME INTEGER-LENGTH
    GET-DISPATCH-MACRO-CHARACTER INTEGERP GET-INTERNAL-REAL-TIME
    INTERACTIVE-STREAM-P INTERN LISP-IMPLEMENTATION-TYPE
    INTERNAL-TIME-UNITS-PER-SECOND LISP-IMPLEMENTATION-VERSION
    INTERSECTION LIST INVALID-METHOD-ERROR LIST* INVOKE-DEBUGGER
    LIST-ALL-PACKAGES INVOKE-RESTART LIST-LENGTH
    INVOKE-RESTART-INTERACTIVELY LISTEN ISQRT LISTP KEYWORD LOAD KEYWORDP
    LOAD-LOGICAL-PATHNAME-TRANSLATIONS LABELS LOAD-TIME-VALUE LAMBDA
    LOCALLY LAMBDA-LIST-KEYWORDS LOG LAMBDA-PARAMETERS-LIMIT LOGAND LAST
    LOGANDC1 LCM LOGANDC2 LDB LOGBITP LDB-TEST LOGCOUNT LDIFF LOGEQV
    LEAST-NEGATIVE-DOUBLE-FLOAT LOGICAL-PATHNAME LEAST-NEGATIVE-LONG-FLOAT
    LOGICAL-PATHNAME-TRANSLATIONS LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
    LOGIOR LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT LOGNAND
    LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT LOGNOR
    LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT LOGNOT
    LEAST-NEGATIVE-SHORT-FLOAT LOGORC1 LEAST-NEGATIVE-SINGLE-FLOAT LOGORC2
    LEAST-POSITIVE-DOUBLE-FLOAT LOGTEST LEAST-POSITIVE-LONG-FLOAT LOGXOR
    LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT LONG-FLOAT
    LEAST-POSITIVE-NORMALIZED-LONG-FLOAT LONG-FLOAT-EPSILON
    LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT LONG-FLOAT-NEGATIVE-EPSILON
    LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT LONG-SITE-NAME
    LEAST-POSITIVE-SHORT-FLOAT LOOP LEAST-POSITIVE-SINGLE-FLOAT
    LOOP-FINISH LENGTH LOWER-CASE-P LET MACHINE-INSTANCE LET* MACHINE-TYPE
    MACHINE-VERSION MASK-FIELD MACRO-FUNCTION MAX MACROEXPAND MEMBER
    MACROEXPAND-1 MEMBER-IF MACROLET MEMBER-IF-NOT MAKE-ARRAY MERGE
    MAKE-BROADCAST-STREAM MERGE-PATHNAMES MAKE-CONCATENATED-STREAM METHOD
    MAKE-CONDITION METHOD-COMBINATION MAKE-DISPATCH-MACRO-CHARACTER
    METHOD-COMBINATION-ERROR MAKE-ECHO-STREAM METHOD-QUALIFIERS
    MAKE-HASH-TABLE MIN MAKE-INSTANCE MINUSP MAKE-INSTANCES-OBSOLETE
    MISMATCH MAKE-LIST MOD MAKE-LOAD-FORM MOST-NEGATIVE-DOUBLE-FLOAT
    MAKE-LOAD-FORM-SAVING-SLOTS MOST-NEGATIVE-FIXNUM MAKE-METHOD
    MOST-NEGATIVE-LONG-FLOAT MAKE-PACKAGE MOST-NEGATIVE-SHORT-FLOAT
    MAKE-PATHNAME MOST-NEGATIVE-SINGLE-FLOAT MAKE-RANDOM-STATE
    MOST-POSITIVE-DOUBLE-FLOAT MAKE-SEQUENCE MOST-POSITIVE-FIXNUM
    MAKE-STRING MOST-POSITIVE-LONG-FLOAT MAKE-STRING-INPUT-STREAM
    MOST-POSITIVE-SHORT-FLOAT MAKE-STRING-OUTPUT-STREAM
    MOST-POSITIVE-SINGLE-FLOAT MAKE-SYMBOL MUFFLE-WARNING
    MAKE-SYNONYM-STREAM MULTIPLE-VALUE-BIND MAKE-TWO-WAY-STREAM
    MULTIPLE-VALUE-CALL MAKUNBOUND MULTIPLE-VALUE-LIST MAP
    MULTIPLE-VALUE-PROG1 MAP-INTO MULTIPLE-VALUE-SETQ MAPC
    MULTIPLE-VALUES-LIMIT MAPCAN NAME-CHAR MAPCAR NAMESTRING MAPCON
    NBUTLAST MAPHASH NCONC MAPL NEXT-METHOD-P MAPLIST NIL NINTERSECTION
    PACKAGE-ERROR NINTH PACKAGE-ERROR-PACKAGE NO-APPLICABLE-METHOD
    PACKAGE-NAME NO-NEXT-METHOD PACKAGE-NICKNAMES NOT
    PACKAGE-SHADOWING-SYMBOLS NOTANY PACKAGE-USE-LIST NOTEVERY
    PACKAGE-USED-BY-LIST NOTINLINE PACKAGEP NRECONC PAIRLIS NREVERSE
    PARSE-ERROR NSET-DIFFERENCE PARSE-INTEGER NSET-EXCLUSIVE-OR
    PARSE-NAMESTRING NSTRING-CAPITALIZE PATHNAME NSTRING-DOWNCASE
    PATHNAME-DEVICE NSTRING-UPCASE PATHNAME-DIRECTORY NSUBLIS
    PATHNAME-HOST NSUBST PATHNAME-MATCH-P NSUBST-IF PATHNAME-NAME
    NSUBST-IF-NOT PATHNAME-TYPE NSUBSTITUTE PATHNAME-VERSION
    NSUBSTITUTE-IF PATHNAMEP NSUBSTITUTE-IF-NOT PEEK-CHAR NTH PHASE
    NTH-VALUE PI NTHCDR PLUSP NULL POP NUMBER POSITION NUMBERP POSITION-IF
    NUMERATOR POSITION-IF-NOT NUNION PPRINT ODDP PPRINT-DISPATCH OPEN
    PPRINT-EXIT-IF-LIST-EXHAUSTED OPEN-STREAM-P PPRINT-FILL OPTIMIZE
    PPRINT-INDENT OR PPRINT-LINEAR OTHERWISE PPRINT-LOGICAL-BLOCK
    OUTPUT-STREAM-P PPRINT-NEWLINE PACKAGE PPRINT-POP PPRINT-TAB READ-CHAR
    PPRINT-TABULAR READ-CHAR-NO-HANG PRIN1 READ-DELIMITED-LIST
    PRIN1-TO-STRING READ-FROM-STRING PRINC READ-LINE PRINC-TO-STRING
    READ-PRESERVING-WHITESPACE PRINT READ-SEQUENCE PRINT-NOT-READABLE
    READER-ERROR PRINT-NOT-READABLE-OBJECT READTABLE PRINT-OBJECT
    READTABLE-CASE PRINT-UNREADABLE-OBJECT READTABLEP PROBE-FILE REAL
    PROCLAIM REALP PROG REALPART PROG* REDUCE PROG1 REINITIALIZE-INSTANCE
    PROG2 REM PROGN REMF PROGRAM-ERROR REMHASH PROGV REMOVE PROVIDE
    REMOVE-DUPLICATES PSETF REMOVE-IF PSETQ REMOVE-IF-NOT PUSH
    REMOVE-METHOD PUSHNEW REMPROP QUOTE RENAME-FILE RANDOM RENAME-PACKAGE
    RANDOM-STATE REPLACE RANDOM-STATE-P REQUIRE RASSOC REST RASSOC-IF
    RESTART RASSOC-IF-NOT RESTART-BIND RATIO RESTART-CASE RATIONAL
    RESTART-NAME RATIONALIZE RETURN RATIONALP RETURN-FROM READ REVAPPEND
    READ-BYTE REVERSE ROOM SIMPLE-BIT-VECTOR ROTATEF SIMPLE-BIT-VECTOR-P
    ROUND SIMPLE-CONDITION ROW-MAJOR-AREF
    SIMPLE-CONDITION-FORMAT-ARGUMENTS RPLACA
    SIMPLE-CONDITION-FORMAT-CONTROL RPLACD SIMPLE-ERROR SAFETY
    SIMPLE-STRING SATISFIES SIMPLE-STRING-P SBIT SIMPLE-TYPE-ERROR
    SCALE-FLOAT SIMPLE-VECTOR SCHAR SIMPLE-VECTOR-P SEARCH SIMPLE-WARNING
    SECOND SIN SEQUENCE SINGLE-FLOAT SERIOUS-CONDITION
    SINGLE-FLOAT-EPSILON SET SINGLE-FLOAT-NEGATIVE-EPSILON SET-DIFFERENCE
    SINH SET-DISPATCH-MACRO-CHARACTER SIXTH SET-EXCLUSIVE-OR SLEEP
    SET-MACRO-CHARACTER SLOT-BOUNDP SET-PPRINT-DISPATCH SLOT-EXISTS-P
    SET-SYNTAX-FROM-CHAR SLOT-MAKUNBOUND SETF SLOT-MISSING SETQ
    SLOT-UNBOUND SEVENTH SLOT-VALUE SHADOW SOFTWARE-TYPE SHADOWING-IMPORT
    SOFTWARE-VERSION SHARED-INITIALIZE SOME SHIFTF SORT SHORT-FLOAT SPACE
    SHORT-FLOAT-EPSILON SPECIAL SHORT-FLOAT-NEGATIVE-EPSILON
    SPECIAL-OPERATOR-P SHORT-SITE-NAME SPEED SIGNAL SQRT SIGNED-BYTE
    STABLE-SORT SIGNUM STANDARD SIMPLE-ARRAY STANDARD-CHAR
    SIMPLE-BASE-STRING STANDARD-CHAR-P STANDARD-CLASS SUBLIS
    STANDARD-GENERIC-FUNCTION SUBSEQ STANDARD-METHOD SUBSETP
    STANDARD-OBJECT SUBST STEP SUBST-IF STORAGE-CONDITION SUBST-IF-NOT
    STORE-VALUE SUBSTITUTE STREAM SUBSTITUTE-IF STREAM-ELEMENT-TYPE
    SUBSTITUTE-IF-NOT STREAM-ERROR SUBTYPEP STREAM-ERROR-STREAM SVREF
    STREAM-EXTERNAL-FORMAT SXHASH STREAMP SYMBOL STRING SYMBOL-FUNCTION
    STRING-CAPITALIZE SYMBOL-MACROLET STRING-DOWNCASE SYMBOL-NAME
    STRING-EQUAL SYMBOL-PACKAGE STRING-GREATERP SYMBOL-PLIST
    STRING-LEFT-TRIM SYMBOL-VALUE STRING-LESSP SYMBOLP STRING-NOT-EQUAL
    SYNONYM-STREAM STRING-NOT-GREATERP SYNONYM-STREAM-SYMBOL
    STRING-NOT-LESSP T STRING-RIGHT-TRIM TAGBODY STRING-STREAM TAILP
    STRING-TRIM TAN STRING-UPCASE TANH STRING/= TENTH STRING< TERPRI
    STRING<= THE STRING= THIRD STRING> THROW STRING>= TIME STRINGP TRACE
    STRUCTURE TRANSLATE-LOGICAL-PATHNAME STRUCTURE-CLASS
    TRANSLATE-PATHNAME STRUCTURE-OBJECT TREE-EQUAL STYLE-WARNING TRUENAME 
    TRUNCATE VALUES-LIST TWO-WAY-STREAM VARIABLE
    TWO-WAY-STREAM-INPUT-STREAM VECTOR TWO-WAY-STREAM-OUTPUT-STREAM
    VECTOR-POP TYPE VECTOR-PUSH TYPE-ERROR VECTOR-PUSH-EXTEND
    TYPE-ERROR-DATUM VECTORP TYPE-ERROR-EXPECTED-TYPE WARN TYPE-OF WARNING
    TYPECASE WHEN TYPEP WILD-PATHNAME-P UNBOUND-SLOT WITH-ACCESSORS
    UNBOUND-SLOT-INSTANCE WITH-COMPILATION-UNIT UNBOUND-VARIABLE
    WITH-CONDITION-RESTARTS UNDEFINED-FUNCTION WITH-HASH-TABLE-ITERATOR
    UNEXPORT WITH-INPUT-FROM-STRING UNINTERN WITH-OPEN-FILE UNION
    WITH-OPEN-STREAM UNLESS WITH-OUTPUT-TO-STRING UNREAD-CHAR
    WITH-PACKAGE-ITERATOR UNSIGNED-BYTE WITH-SIMPLE-RESTART UNTRACE
    WITH-SLOTS UNUSE-PACKAGE WITH-STANDARD-IO-SYNTAX UNWIND-PROTECT WRITE
    UPDATE-INSTANCE-FOR-DIFFERENT-CLASS WRITE-BYTE
    UPDATE-INSTANCE-FOR-REDEFINED-CLASS WRITE-CHAR
    UPGRADED-ARRAY-ELEMENT-TYPE WRITE-LINE UPGRADED-COMPLEX-PART-TYPE
    WRITE-SEQUENCE UPPER-CASE-P WRITE-STRING USE-PACKAGE WRITE-TO-STRING
    USE-VALUE Y-OR-N-P USER-HOMEDIR-PATHNAME YES-OR-NO-P VALUES ZEROP
    ))


(provide 'pjb-cl)
;;;; THE END ;;;;

