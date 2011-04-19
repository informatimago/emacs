;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-utilities.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports various utility functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2005-07-28 <PJB> Added compiletime-cond
;;;;    2002-02-17 <PJB> Added stderr and stdout parameters to printf.
;;;;    2001-11-30 <PJB> Added process-with-id.
;;;;    2001-11-02 <PJB> Added foreach, commented-out macros.
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

(provide 'pjb-utilities)
(require 'pjb-strings)
(require 'pjb-cl)
(require 'forms)
(require 'comint)
(require 'calendar)


(defmacro compiletime-cond (&rest clauses)
  (if (eval (first (first clauses)))
      `(progn ,@(rest (first clauses)))
      `(compiletime-cond ,@(rest clauses))))



(defun recursive-apply (atom-func a-list b-list)

  "Applies recursively the function  atom-func on each and every pairs
that can  be found recursively  in the two parallel  structures a-list
and b-list. Only the elements from a-list must be an atom to be passed
to atom-func. Examples: 

  (recursive-apply '+ '((1 2) (3 4)) '((1 0) (0 1))) 
        ---> ((2 2) (3 5))

  (recursive-apply (lambda (atom other) (cons atom other))
         '(apple orange peach)
         '((red yellow green) (orange) (yellow white)))

        ---> ((apple red yellow green) (orange orange) (peach yellow white))
"
  (cond ((null a-list) nil)
        ((atom a-list) (apply atom-func (list a-list b-list)))
        (t (cons (recursive-apply atom-func (car a-list) (car b-list)) 
                 (recursive-apply atom-func (cdr a-list) (cdr b-list))))))

(defun padd (a b)  (recursive-apply '+ a b))
(defun psub (a b)  (recursive-apply '- a b))
(defun pmul (a b)  (recursive-apply '* a b))
(defun pdiv (a b)  (recursive-apply '/ a b))
(defun pmod (a b)  (recursive-apply '% a b))




(defun pjb-or (&rest args)
  "or is not a true function..."
  (while (and (consp args) (not (car args)))
    (setq args (cdr args)))
  (car args))


(defun pjb-equal (a b)
  "An implementation of equal that prints out the atoms."
  (cond
   ((and (atom a) (atom b))
    (printf :stdout "%S\n%S\n\n" a b)
    (equal a b))
   ((and (consp a) (consp b))
    (and (pjb-equal (car a) (car b))
         (pjb-equal (cdr a) (cdr b))))
   (t
    (printf :stdout "%S\n%S\n\n" a b)
    nil)))


(defun pjb-diff (a b)
  "
DO:      Show deep differences between a and b. a and b should be  
         lists  of same structure (recursively).
RETURN:  Whether there are differences between a and b.
"
  (cond
   ((and (atom a) (atom b))
    (if (equal a b)
        nil
      (printf :stdout "%S\n%S\n\n" a b)
      t))
   ((and (consp a) (consp b))
    (let ((t1 (pjb-diff (car a) (car b)))
          (t2 (pjb-diff (cdr a) (cdr b))))
      (or t1 t2)))
   (t
    (printf :stdout "%S\n%S\n\n" a b)
    t)))

(defun pjb-struct-diff (a b &optional cmp)
  "
DO:      Compare two structures and return the differences.
RETURN:  A list structured like a and b containing nil where elements 
         from a and b match or (cons elt-a elt-b) when elt-a and elt-b differ.
NOTE:    cmp is a function used to compare atoms ('eq, 'equal or whatever).
         Default is 'eq.
"
  (unless cmp (setq cmp 'eq))
  (cond
   ((and (atom a) (atom b))
    (if (funcall cmp a b)
        nil
      (cons a b))
    )
   ((and (consp a) (consp b))
    (cons (pjb-struct-diff (car a) (car b))
          (pjb-struct-diff (cdr a) (cdr b))))
   (t
    (cons a b))))


(defun ^ (x exp)
  "Computes x^exp = x to the power of exp."
  (cond ((< exp 0)       (/ 1.0 (^ x (- exp))))
        ((= exp 0)       1)
        ((= exp 1)       x)
        ((integerp exp)  (if (= (% exp 2) 0)
                             (let ((x2 (^ x (/ exp 2))))
                               (* x2 x2))
                             (let ((x2 (^ x (/ (- exp 1) 2))))
                               (* x x2 x2))))
        (t               (exp (* (log x) exp)))))



(defun float-precision (&optional base)
  "RETURN: the number of base digit available in floating point numbers.
        Default is base Ten."
  (setq base (+ 0.0 (or base 10.0)))
  (let ((number 1.0) (number+1 (1+ 1.0)) (precision 0))
    (while (/= number number+1)
      (setq number    (* base number)
            number+1  (1+ number)
            precision (1+ precision)))
    precision))
  


(defun float-to-base (number base)
  "DO:      Convert  a number  value into  a string  contening 
            the  same value expressed into the given base. 1<base<37.
SEE-ALSO:decimal-to-base."
  (cond
   ((not (integerp base))         (error "Invalid base (%S)." base))
   ((or (< base 2) (< 36 base))   (error "Invalid base (%d)." base))
   )
  (let ((digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (buffer (make-string 32 0))
        (b 31)
        (sign "")
        )
    (if (< number 0.0)
        (setq sign   "-"
              number (abs number)))
    (unless (/= (1+ number) number)
      (error "Number to big."))
    (setq number (- number (mod number 1.0)))
    (while (< 0.0 number)
      (let ((digit   (truncate (mod number base))))
        (aset buffer b (aref digits digit))
        (setq number (/ (- number digit) base)
              b       (- b 1))))
    (if (= b 31)
        "0"
      (concat sign (substring buffer (+ 1 b))))))


(defun float-from-base (string-number-in-base base)
  "DO:      Convert the value encoded as a string containing a number 
         expressed in base into a float. 1<base<37. Anycase
         letter are accepted as  digits  values from  10  to  36  for
         A  to  Z.  The value  may  be signed.  The string  sould
         only contain  an  optional sign  (+ or  -) followed by one or
         more alphadigits in the range valid for base.
SEE-ALSO:float-from-base."

  (cond
   ((not (integerp base))         (error "Invalid base (%S)." base))
   ((or (< base 2) (< 36 base))   (error "Invalid base (%d)." base))
   ((not (stringp string-number-in-base))      
    (error "For now, I only convert from string values.")))
  (let (
        (number  0.0)
        (sign    1.0)
        (b       0)
        (min     1)
        (len     (length string-number-in-base))
        (digit   (aref string-number-in-base 0))
        (max-nine)
        (max-biga)
        (max-smalla)
        )
    (cond
     ((= digit ?+) (setq min 2 b 1))
     ((= digit ?-) (setq min 2 b 1 sign -1)))
    (if (<= len min)
        (error 
         "The input string must contain at least one digit (consider \"0\")."))
    (if (< base 10) 
         (setq max-nine   (- (+ ?0 base) 1)
               max-biga   0
               max-smalla 0)
         (setq max-nine   ?9
               max-biga   (- (+ ?A base) 11)
               max-smalla (- (+ ?a base) 11)))
    (while (< b len)
      (setq digit (aref string-number-in-base b))
      (cond 
       ((and (<= ?0 digit) (<= digit max-nine))
        (setq number (+ (* number base) (- digit ?0))))
       ((and (<= ?A digit) (<= digit max-biga))
        (setq number (+ (* number base) 10 (- digit ?A))))
       ((and (<= ?a digit) (<= digit max-smalla))
        (setq number (+ (* number base) 10 (- digit ?a))))
       (t (error "Input string contained a digit (%c) not in the base (%d)."
                 digit base)))
      (if (< number 0)
          (error (concat "Overflow in integer computation. "
                         "(Sorry only %d-bit integers are available here).")
                 (let ((p 3)) (while (< 0 (^ 2 p)) (setq p (+ 1 p))) (- p 1))))
      (setq b (+ 1 b)))
    (* sign number)))



(defun integer-to-base (decimal base &optional width padchar
                                commachar comma-interval)
  "
DO:      Convert  a decimal  value into  a string  contening the 
         same value expressed into the given base. 1<base<37.
         The optional WIDTH specifies the minimum length of the returned 
         string (0-left-filled), not counting a '-' sign.
SEE-ALSO:float-to-base."
    ;;TODO: Implement commachar, comma-interval
  (cond
   ((not (integerp base))         (error "Invalid base (%S)." base))
   ((or (< base 2) (< 36 base))   (error "Invalid base (%d)." base))
   ((not (integerp decimal))      
    (error "For now, I only convert integer values.")))
  (let ((digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (buffer (make-string 32 ?0))
        (b 31)
        (sign "")
        )
    (if (< decimal 0)
        (setq sign "-"
              decimal (abs decimal)))
    (while (< 0 decimal)
      (let ((digit   (% decimal base)))
        (aset buffer b (aref digits digit))
        (setq decimal (/ (- decimal digit) base)
              b       (- b 1))))
    (if width (if (< 32 width)
                  (setq sign (concat sign (make-string (- width 32) padchar))
                        width 32)))
    (concat sign 
            (if (and width (< (- 32 width) (+ 1 b) ))
                (substring buffer (- 32 width))
              (if (= b 31) "0" (substring buffer (+ 1 b)))))))



(defun integer-from-base (string-number-in-base base)
  "
DO:      Convert the value encoded as a string containing a number 
         expressed in base into an integer. 1<base<37. Anycase
         letter are accepted as  digits  values from  10  to  36  for
         A  to  Z.  The value  may  be signed.  The string  sould
         only contain  an  optional sign  (+ or  -) followed by one or
         more alphadigits in the range valid for base.
SEE-ALSO:float-from-base."
  (cond

   ((not (integerp base))         (error "Invalid base (%S)." base))
   ((or (< base 2) (< 36 base))   (error "Invalid base (%d)." base))
   ((not (stringp string-number-in-base))      
    (error "For now, I only convert from string values.")))
  (let (
        (decimal 0)
        (sign    1)
        (b       0)
        (min     1)
        (len     (length string-number-in-base))
        (digit   (aref string-number-in-base 0))
        (max-nine)
        (max-biga)
        (max-smalla)
        )
    (cond
     ((= digit ?+) (setq min 2 b 1))
     ((= digit ?-) (setq min 2 b 1 sign -1)))
    (if (<= len min)
        (error 
         "The input string must contain at least one digit (consider \"0\")."))
    (if (< base 10) 
         (setq max-nine   (- (+ ?0 base) 1)
               max-biga   0
               max-smalla 0)
         (setq max-nine   ?9
               max-biga   (- (+ ?A base) 11)
               max-smalla (- (+ ?a base) 11)))
    (while (< b len)
      (setq digit (aref string-number-in-base b))
      (cond 
       ((and (<= ?0 digit) (<= digit max-nine))
        (setq decimal (+ (* decimal base) (- digit ?0))))
       ((and (<= ?A digit) (<= digit max-biga))
        (setq decimal (+ (* decimal base) 10 (- digit ?A))))
       ((and (<= ?a digit) (<= digit max-smalla))
        (setq decimal (+ (* decimal base) 10 (- digit ?a))))
       (t (error "Input string contained a digit (%c) not in the base (%d)."
                 digit base)))
      (if (< decimal 0)
          (error (concat "Overflow in integer computation. "
                         "(Sorry only %d-bit integers are available here).")
                 (let ((p 3)) (while (< 0 (^ 2 p)) (setq p (+ 1 p))) (- p 1))))
      (setq b (+ 1 b)))
    (* sign decimal)))



(defun pjb-seconds-to-time (secs)
  "Converts a value givent as 3600th, into an integer and 60th and 3600th 
encoded as a \"HH:MM:SS.sss\" string."
  (let ((s (% (truncate secs) 60)))
	(let ((m (% (truncate (/ secs 60)) 60)))
	  (let ((h (truncate (/ secs 3600))))
		(format "%02d:%02d:%02d = %d seconds" h m s (truncate secs))))))


(defun d-dms (d)
  "Convert a decimal value into an integer and 60th and 3600th 
encoded as a \"HH:MM:SS.sss\" string."
  (let ((epsilon 0.000001)
        (hou) (min*) (min) (sec*) (sec))
    (setq hou  (floor d))
    (setq min* (* 60.0 (- d hou)))
    (setq min  (floor min*))
    (setq sec* (* 60.0 (- min* min)))
    (if (< (- 60.0 epsilon) sec*)
        (progn
          (setq min (+ min 1.0))
          (if (<= 60.0 min)
              (setq min 0.0
                    hou (+ 1.0 hou)))
          (setq sec* (- (+ epsilon sec*) (floor (+ epsilon sec*))))))
    (setq sec sec*)
    (if (< (- sec (floor sec)) 0.000001)
        (format "%d:%02d:%02d" hou min  sec)
      (if (< (- 1.0 0.000001) (- sec (floor sec)))
        (format "%d:%02d:%02d" hou min  (+ 0.000001 sec))
        (format "%d:%02d:%09.6f" hou min sec)))))



(defun dms-d (arg &rest rest)
  "Convert value given as an integer and 60th and 3600th,
encoded as a  \"HH:MM:SS.sss\" string, into a decimal value."
  (if (stringp arg)
      (let* (
             (one (string-match ":" arg))
             (hou (string-to-number (substring arg 0 one)))
             (two (if (null one) nil (string-match ":" arg (+ one 1))))
             (min (if (null one) 0 
                    (string-to-number (substring arg (+ one 1) two))))
             (tre (if (null two) nil  (string-match ":" arg (+ two 1))))
             (sec (if (null two) 0 
                    (string-to-number (substring arg (+ two 1)))))
             )
        (if (null rest)
            (+ hou (/ min 60.0) (/ sec 3600.0))
          (error "dms-d expects either one string or one to three numbers.")))
    (let ((hou arg)
          (min (if (null (car rest))  0 (car rest)))
          (sec (if (null (cadr rest)) 0 (cadr rest))))
      (+ hou (/ min 60.0) (/ sec 3600.0)))))


 
(defmacro commented-out (&rest stuff)
  "Use this macro to 'comment-out' some lisp. 
Note: the ignore function evaluates its arguments, 
      while the commented-out macro doesn't."
  nil)


(defmacro foreach (var list &rest body)
  `(mapcar (lambda (,var) ,@body) ,list))



(defun split-cl-format (format-string)
  "
PRE:    format-string  is a string containing Common-Lisp format specifiers.
RETURN: a list of strings, each either a specifier-less string 
        or containing only one specifier (thus beginning with ~ ; 
        ~~ is considered a specifier).
NOTE:   The clisp reader does not know anything about backslash escapes 
        such as \n.   Only \" and \\ are recognized as expected.
"
  (let ((positions '())
        (len (length format-string)))
    (dotimes (pos len)
      )))



(defconst stderr :stderr)
(defconst stdout :stdout)

(defun printf (&rest args)
  "
The first argument is optional. When present, it may be either:
  - a function of one character argument,
  - a buffer (output is inserted before point),
  - a marker (output is inserted and the marker is advanced),
  - the symbol t (output appears in the echo area 
    that is, *Message* buffer when interactive, stdout when in batch),
  - the symbol :stderr (output appears in *Message* or on /dev/stderr),
  - the symbol :stdout (output appears in the current buffer or on /dev/stdout).

  ------------  ------------------------  ------------------------
  output        interactive               non-interactive
  ------------  ------------------------  ------------------------
  functionp     (apply fun one-char)      (apply fun one-char)
  bufferp       inserted before point     inserted before point 
  markerp       inserted and advanced     inserted and advanced
  nil           (current-buffer)          /dev/stdout
  t             *STANDARD-OUTPUT*         *STANDARD-OUTPUT*
  :stdout       (current-buffer)          /dev/stdout
  :stderr       *Message*                 /dev/stderr
  ------------  ------------------------  ------------------------


When the first argument is absent, the resulting formated string is
inserted into the current buffer, at the current point, or to stdout.

The second argument is a format string (with % specifiers  like C printf).

The optional remaining arguments are formated. 

"
  (block :print
    (let ( (kind (car args))
           format-args write out)
            
      (cond 
       ;; no optional first argument -> out is current buffer
       ;; TODO: This should be the same as :stdout !
       ((stringp kind)
        (setq out (current-buffer)
              format-args args
              write (lambda (s) (write-string s out)) 
              ))

       ;; printf stderr -> (message          in any case
       ;;                                    but when interactive, message is small !
       ((eq :stderr kind)
        (setq format-args (cdr args)
              write (lambda (s) (if noninteractive
                                    (write-string s 'external-debugging-output)
                                  (message s)))
              ;;    (lambda (s) (message "%s" s))
              ))

       ;; printf stdout -> (printf (current-buffer) ...)  if interactive
       ;;               -> (printf t ...)                 if batch
       ((eq :stdout kind)
        (setq format-args (cdr args)
              write (lambda (s) 
                      (write-string s (if noninteractive t (current-buffer))))
              ))

       (t
        (apply 'printf *STANDARD-OUTPUT* (cdr args))
        (return-from :print))
;;;       (setq out (car args)
;;;             format-args (cdr args)
;;;             write (lambda (s) (write-string s out))
;;;             ))
       ) ;;cond
      (funcall write (apply 'format  format-args)))))


(defun write-string (string  &optional out)
  "
SEE-ALSO: `write-char'.
"
  (mapc (lambda (char) (write-char char out)) string)
  nil)


(defun show (&rest x)
  "Insert the formated value X."
  (unless (= (point) (progn (beginning-of-line) (point))) 
    (end-of-line)
    (insert "\n"))
  (insert (format ";; --> %S\n"
      (if (= 1 (length x)) (car x) x)))
  (if (= 1 (length x)) (car x) x))



(defmacro for (var  init  final  &rest body)
  "Execute a simple for loop: (for i  1  10  (print i))."
  (let ((tempvar (make-symbol "max")))
    `(let ((,var ,init)
           (,tempvar ,final))
       (if (< ,var ,tempvar)
           (while (<= ,var ,tempvar)
             ,@body
             (setq ,var (+ ,var 1)))
           (while (>= ,var ,tempvar)
             ,@body
             (setq ,var (- ,var 1)))))))
   

(defun today ()
  "Returns the date of today in YYYY-MM-DD format."
  (let* ((date (calendar-current-date))
         (month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date)))
    (format "%04d-%02d-%02d" year month day)))


(defun remove-parity-from-region ()
  "Replace the region by the same 8-bit text with the parity bit set to 0 (7-bit)."
  (interactive)
  (let* ((text-8 (buffer-substring-no-properties (region-beginning) 
                                                 (region-end)))
         (len    (length text-8))
         (text-7 (make-string len ??))
         (i      0))
    (while (< i len)
      (aset text-7 i (% (aref text-8 i) 128))
      (setq i (+ 1 i)))
    (delete-region  (region-beginning) (region-end))
    (insert text-7)))


(defun reverse-lines (start end)
  "Reverse the order of the characters in each lines in the region.
  (Useful for hebrew)."
  (interactive "r")
  (let* ((text)(lines)(first))
    (setq text (buffer-substring-no-properties start end))
    (setq lines  (split-string text "[\n]"))
    (setq first lines)
    (while lines
      (if (< 0 (length (car lines)))
        (setcar lines (apply 'string (reverse (string-to-list (car lines))))))
      (setq lines (cdr lines)))
    (delete-region  start end)
    (insert (unsplit-string first "\n"))))





(defun kill-all-empty-buffers (&optional invisibles-too)
  "DO:     Kills all empty buffers.
        In addition, when invisibles-too, kills the invisible buffers too."
  (interactive "P")
  (let ((buffers (buffer-list))
        (buf))
    (while buffers 
      (setq buf (car buffers)
            buffers (cdr buffers))
      (set-buffer buf)
      (when (or
             (= 0 (buffer-size))
             (and invisibles-too (= 32 (string-to-char (buffer-name buf)))))
        (set-buffer buf)    
        (set-buffer-modified-p nil)
        (kill-buffer buf)
        ))
    (sleep 0)))


(defun process-with-id (pid)
  "RETURN:  The process whose process-id is pid 
         (or nil of none is found in (process-list))."
  (let ( (pl (process-list))
         (pr) )
    (while pl
      (if (= pid (process-id (car pl)))
          (setq pr (car pl)
                pl nil)
        (setq pl (cdr pl))))
    pr))


(defun plist-remove (plist key)
  "RETURN:  A new plist with the elements in plist but the one with key.
NOTE:    A suffix in result may be a suffix of plist too."
  (if (eq (car plist) key)
      (cdr (cdr plist))
    (cons (car plist) (cons (cadr plist) (plist-remove (cddr plist) key)))))



(defun seconds-to-emacs-time (secs)
  "
PRE:     secs is a number of seconds.
RETURN:  The time represented by secs in emacs time format, ie.
         a list ( h l us ) with h=secs/2^16, l=secs%2^16, us=(secs*1e6)%1e6.
"
  (let* ( (h  (truncate (/ secs 65536)))
          (lf (- secs (* h 65536.0)))
          (l  (truncate lf))
          (us (truncate (* 1000000.0 (- lf l)))) )
    (list h l us)))


(defun emacs-time-to-seconds (et)
  "
PRE:     et is a time in emacs time format, ie. a list ( h l us)
         with h and l being in [0..65535] and us in [0..999999].
RETURN:  et expressed as a scalar."
  (+ (let ((h (nth 0 et))) (if (< h 1024) (* h 65536) (* h 65536.0)))
     (nth 1 et)
     (let ((us (nth 2 et))) (if (= 0 us) 0 (/ us 1000000.0)))))


(defun color-mix     (color-a color-b &optional factor)
  "
PRE:     factor in [0.0 1.0], default is 0.5.
RETURN:  A triplet (red green blue)
         = color-a + ( color-b - color-a ) * factor
"
  (setq factor (or factor 0.5))
  (when (or (<= factor 0.0) (<= 1.0 factor))
    (error "Factor %f is out of range [0.0,1.0]." factor))
  (mapcar* (lambda (a b) (truncate (+ a (* (- b a)  factor)))) color-a color-b))


(defun color-48-lighter (color-values &optional factor)
  "
PRE:     color-value is triplet (Red Green Blue) with each value in [0..65535].
RETURN:  a color triplet lighter than color-value (by 0.5 or by factor).
"
  (setq factor (or factor 0.5))
  (when (or (<= factor 0.0) (<= 1.0 factor))
    (error "Factor %f is out of range [0.0,1.0]." factor))
  (color-mix color-values '( 65535 65535 65535 ) factor))


(defun color-48-darker (color-values &optional factor)
  "
PRE:     color-value is triplet (Red Green Blue) with each value in [0..65535].
RETURN:  a color triplet darker than color-value (by 0.5 or by factor).
"
  (setq factor (or factor 0.5))
  (when (or (<= factor 0.0) (<= 1.0 factor))
    (error "Factor %f is out of range [0.0,1.0]." factor))
  (color-mix color-values '( 0 0 0 ) factor))


(defun color-48-value-to-name (color-value)
  "
PRE:     color-value is triplet (Red Green Blue) with each value in [0..65535].
RETURN:  a color name in the form \"#rrrrggggbbbb\" each letter being 
         a hexadecimal digit.
"
  (format "#%04x%04x%04x" 
          (nth 0 color-value)
          (nth 1 color-value)
          (nth 2 color-value)))


(defalias 'lighter 'color-48-lighter)
(defalias 'darker  'color-48-darker)
(defalias 'color-value-to-name 'color-48-value-to-name)


(defun color-24-value-to-name (color-value)
  "
PRE:     color-value is triplet (Red Green Blue) with each value in [0..255].
RETURN:  a color name in the form \"#rrggbb\" each letter being 
         a hexadecimal digit.
"
  (format "#%02x%02x%02x" 
          (nth 0 color-value)
          (nth 1 color-value)
          (nth 2 color-value)))


(defun color-24-to-48 (color-24-values)
  "
PRE:     color-value is triplet (Red Green Blue) with each value in [0..255].
RETURN:  a (Red Green Blue) with each value in [0..65535].
"
  (mapcar (lambda (x) (* 256 x)) color-24-values))


(defun color-48-to-24 (color-48-values)
  "
PRE:     color-value is triplet (Red Green Blue) with each value in [0..65535].
RETURN:  a (Red Green Blue) with each value in [0..255].
"
  (mapcar (lambda (x) (truncate x 256)) color-48-values))


(defun get-ip-interfaces ()
  "
RETURN: A list of list of strings (interface-name ip-address up-or-down)
        ip-address and up-or-down (\"UP\" or \"DOWN\") may be absent.
"
  (let ((l 
         (mapcar 
          (lambda (line)
            (cond
             ((string-match "^\\([^ ]+\\) .*" line)
              (match-string 1 line))
             ((string-match "^ .* inet addr:\\([0-9\\.]+\\) .*" line)
              (match-string 1 line))
             ((string-match "^ .* \\(UP\\|DOWN\\) .*" line)
              (match-string 1 line))
             ((string-equal line "")
              :next)
             (t nil)))
          (split-string (shell-command-to-string "ifconfig -a") "\n"))
         )
        (r nil) (i nil))

    (while l
      (cond 
       ((null (car l))
        ;; nop
        )
       ((eq :next (car l))
        (setq r (cons (nreverse i) r))
        (setq i nil))
       (t (setq i (cons (car l) i))))
      (setq l (cdr l)))
    r))



(commented-out
 (list-colors-display 
  (mapcar (lambda (c) 
            (color-value-to-name (darker (x-color-values c) 0.8)))
          x-colors))
 (show (color-value-to-name (lighter (x-color-values "MediumSpringGreen") 0.8)))




 (defun letter-index (string)
   (mapcar (lambda (l) (- l ?@)) string ))

 (defun letter-incr (string inc)
   (concat (mapcar (lambda (l) (+ inc l ?@)) (letter-index string))))

 (letter-incr "VMS" 1)

 (defun make-list (length init)
   (cond 
     ((>= 0 length) nil)
     ((= 1 length) (list init))
     (t  (cons init (make-list (1- length) init)))))

 (defun make-list (length init)
   (let ((res nil))
     (while (< 0 length)
       (setq res (cons init res)
             length (1- length)))
     res))


 (defun letter-diff (a b)
   (recursive-apply 'mod
                    (psub (string-to-list a) (string-to-list b))
                    (make-list (length a) 26)))

 ) ;;commented-out




(defun dichotomy (vector value compare &optional start end key)
  "
PRE:	entry is the element to be searched in the table.
        (<= start end)
RETURN: (values found index order)
POST:	(<= start index end)
        +-------------------+----------+-------+----------+----------------+
        | Case              |  found   | index |  order   |     Error      |
        +-------------------+----------+-------+----------+----------------+
        | x < a[min]        |   FALSE  |  min  |  less    |      0         |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
        | x = a[i]          |   TRUE   |   i   |  equal   |      0         |
        | a[max] < x        |   FALSE  |  max  |  greater |      0         |
        +-------------------+----------+-------+----------+----------------+
"
  (setf start (or start 0)
        end   (or end (length vector))
        key   (or key (function identity)))
  (let* ((curmin start)
         (curmax end)
         (index  (truncate (+ curmin curmax) 2))
         (order  (funcall compare value (funcall key (aref vector index)))) )
    (while (and (/= 0 order) (/= curmin index))
      (if (< order 0)
        (setf curmax index)
        (setf curmin index))
      (setf index (truncate (+ curmin curmax) 2))
      (setf order (funcall compare value (funcall key (aref vector index)))))
    (when (and (< start index) (< order 0))
      (setf order 1)
      (decf index))
    (assert
     (or (< (funcall compare value (aref vector start)) 0)
         (and (< (funcall compare (aref vector index) value) 0)
              (or (>= (1+ index) end)
                  (< (funcall compare value (aref vector (1+ index))) 0)))
         (= (funcall compare value (aref vector index)) 0)))
    (values (= order 0) index order)))


;;;; pjb-utilities.el                 --                     --          ;;;;
