;; From: Eli Zaretskii <eliz@gnu.org>
;; Subject: Re: Converting an Integer into Human Readable String
;; Newsgroups: gnu.emacs.help
;; To: help-gnu-emacs@gnu.org
;; Date: Sat, 09 Apr 2011 11:46:34 +0300
;; Message-ID: <mailman.1.1302338931.20547.help-gnu-emacs@gnu.org>
;;
;; >  . It cannot be evaluated in Emacs Lisp without commenting out the
;; >    part between "#|" and "|#".
;; >
;; >  . It cannot be evaluated on a 32-bit machine without commenting out
;; >    some parts of the integer test values in the test harness, due to
;; >    integer overflows.
;; >
;; >  . When the last argument is :binary, it produces wrong results for
;; >    numbers between 1000*2^N and 1023*2^N.  E.g.,
;; >
;; >     (format-human-readable-big-number 1023  "%.1f" "%13.3e" "B" t :binary)
;; >       => "   1.023e+003 B"
;; >
;; >    whereas I'd expect "1023 B" instead.
;; >
;; >  . It always produces results with a fixed number of digits after the
;; >    decimal, determined by the value of *normal-format*.  Thus, with a
;; >    format of "%.1f" it will always produce 1 digit after the decimal,
;; >    even if that digit is zero:
;; >
;; >     (format-human-readable-big-number 900  "%.1f" "%13.3e" "B" t :binary)
;; >       => "900.0 B"
;; >
;; >    which is IMO ugly; "ls -lh" produces just "900" in this case.  This
;; >    cannot be remedied by using "%.0f" as the normal format, because
;; >    then it will always round to the nearest integral value, and the
;; >    fractions will never be shown; again, this is different from "ls -lh".



;; Note: since in emacs lisp, there are no bignums, we keep the
;; exponents in symbolic form.

;; #-emacs ;; Too bad emacs lisp doesn't implement #+/#-
(require 'cl)


(defun dichotomy (vector value compare &optional start end key)
  "
PRE:    entry is the element to be searched in the table.
        (<= start end)
RETURN: (list found index order)
POST:   (<= start index end)
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
    (loop while (and (/= 0 order) (/= curmin index)) do
         (if (< order 0)
             (setf curmax index)
             (setf curmin index))
         (setf index (truncate (+ curmin curmax) 2))
         (setf order (funcall compare value (funcall key (aref vector index)))))
    (when (and (< start index) (< order 0))
      (setf order 1)
      (decf index))
    (assert
     (or (< (funcall compare value (funcall key (aref vector start))) 0)
         (and (< (funcall compare (funcall key (aref vector index)) value) 0)
              (or (>= (1+ index) end)
                  (< (funcall compare value (funcall key (aref vector (1+ index)))) 0)))
         (= (funcall compare value (funcall key (aref vector index))) 0)))
    (list (= order 0) index order)))


(defun filter-prefixes (prefixes value)
  "
PREFIXES is a list of (long short (expt base exponent))
VALUE    is either an integer or a floating point value.

DO:      Filters out prefixes that are out of range for type of the
         given value, and compute the values of the symbolic exponents
         in prefixes.

RETURN:  a list of (long short base^exponent-value)
"
  (etypecase value
    (float
     (mapcar (lambda (prefix)
               (destructuring-bind (long short (expt base exponent)) prefix
                 (list long short (expt (float base) exponent))))
             prefixes))
    (integer
     (mapcan (lambda (prefix)
               (destructuring-bind (long short (expt base exponent)) prefix
                 (when (< (expt (float base) exponent) most-positive-fixnum)
                   (list (list long short (expt (float base) exponent))))))
             prefixes))))


(defun compute-prefixes ()
  "
RETURN: A hash-table mapping lists of prefix-code and type to a
        filtered and sorted vector of prefixes.
PREFIX-CODE either :si or :binary
TYPE        either float or integer
"
  (let ((table (make-hash-table :test (function equal))))
    (loop
       for value in '(0 0.0)
       for type in '(integer float)
       do (loop
             for prefix-code in '(:si :binary)
             for prefixes     in (list *si-prefixes* *binary-prefixes*)
             do (setf (gethash (list prefix-code type) table)
                      (coerce (sort  (filter-prefixes prefixes value)
                                     (lambda (a b) (< (third a) (third b))))
                              'vector))))
    table))


(defvar *si-prefixes*
  '(("yotta" "Y" (expt 10 24))
    ("zetta" "Z" (expt 10 21))
    ("exa"   "E" (expt 10 18))
    ("peta"  "P" (expt 10 15))
    ("tera"  "T" (expt 10 12))
    ("giga"  "G" (expt 10 9))
    ("mega"  "M" (expt 10 6))
    ("kilo"  "k" (expt 10 3))
    (""      ""  (expt 10 0))
    ("milli" "m" (expt 10 -3))
    ("micro" "µ" (expt 10 -6))
    ("nano"  "n" (expt 10 -9))
    ("pico"  "p" (expt 10 -12))
    ("femto" "f" (expt 10 -15))
    ("atto"  "a" (expt 10 -18))
    ("zepto" "z" (expt 10 -21))
    ("yocto" "y" (expt 10 -24))))


(defvar *binary-prefixes*
  '(("yobi"  "Yi" (expt 2 80))
    ("zebi"  "Zi" (expt 2 70))
    ("exbi"  "Ei" (expt 2 60))
    ("pebi"  "Pi" (expt 2 50))
    ("tebi"  "Ti" (expt 2 40))
    ("gibi"  "Gi" (expt 2 30))
    ("mebi"  "Mi" (expt 2 20))
    ("kibi"  "Ki" (expt 2 10))
    (""      ""   (expt 2 0))))

(defvar *prefixes* (compute-prefixes))


(defun find-scale (num prefix-code)
  "
Find from the *prefixes* the scale of the number NUM with the given
PREFIX-CODE.
"
  (let ((prefixes  (gethash (list prefix-code (etypecase num
                                                (integer 'integer)
                                                (float   'float)))
                            *prefixes*)))
    (destructuring-bind (foundp index order)
        (dichotomy prefixes num (lambda (a b)
                                  (cond ((< a b) -1)
                                        ((< b a) +1)
                                        (t        0)))
                   0 (length prefixes) (function third))
      (cond
        ((minusp order) ; too small
         '("" "" 1))
        ((< (/ num 1000.0) (third (aref prefixes index))) ; ok
         (aref prefixes index))
        (t ; too big
         '("" "" 1))))))




#|
;; For Common Lisp.  But too bad, emacs lisp doesn't implement #+/#-.

#+common-lisp
(defun format-human-readable-big-number (num format exceptional-format
                                         base-unit short-form prefixes)
  (destructuring-bind (long short scale) (find-scale num prefixes)
    (format nil "~? ~A~A"
            (if (and (= 1 scale)
                     (or (and (< 0 (abs num)) (< (abs num) 1))
                         (<= 1000 (abs num))))
                exceptional-format
                format)
            (list (/ num scale))
            (if short-form short long)
            base-unit)))

#+common-lisp
(defvar *normal-format*       "~9,3F")

#+common-lisp
(defvar *exceptional-format*  "~13,3E")

|#

(defun format-human-readable-big-number (num format exceptional-format
                                         base-unit short-form prefixes)
  (destructuring-bind (long short scale) (find-scale num prefixes)

    (format "%s %s%s" (format (if (and (= 1 scale)
                                       (or (and (< 0 (abs num)) (< (abs num) 1))
                                           (<= 1000 (abs num))))
                                  exceptional-format
                                  format)
                              (/ num scale))
            (if short-form short long)
            base-unit)))

(defvar *normal-format*       "%9.3f")
(defvar *exceptional-format*  "%13.3e")

(defun test/format-human-readable-big-number ()
  (dolist (prefixes '(:si :binary))
    (dolist (short-form '(nil t))
      (dolist (num '(4 45 456 4567 45678 456789 467890 45678901
                     456789012 4567890123 45678901234 456789012345
                     4567890123456 45678901234567 456789012345678
                     4567890123456789 45678901234567890
                     456789012345678901
                     0.04333
                     0.4333
                     4.333 45.333 456.333 4567.333
                     45678.333 456789.333 467890.333 45678901.333
                     456789012.333 4567890123.333 45678901234.333
                     456789012345.333 4567890123456.333
                     45678901234567.333 456789012345678.333
                     4567890123456789.333 45678901234567890.333
                     456789012345678901.333 4567890123456789012.333
                     45678901234567890123.333 456789012345678901234.333
                     4567890123456789012345.333
                     45678901234567890123456.333
                     456789012345678901234567.333
                     4567890123456789012345678.333
                     45678901234567890123456789.333
                     456789012345678901234567890.333
                     4567890123456789012345678901.333
                     45678901234567890123456789012.333
                     456789012345678901234567890123.333
                     ))
        (princ (format-human-readable-big-number num
                                                 *normal-format*
                                                 *exceptional-format*
                                                 (if short-form
                                                     "B"
                                                     "bit")
                                                 short-form prefixes))
        (terpri)))))

(test/format-human-readable-big-number)
    4.000 bit
   45.000 bit
  456.000 bit
    4.567 kilobit
   45.678 kilobit
  456.789 kilobit
  467.890 kilobit
   45.679 megabit
  456.789 megabit
    4.568 gigabit
   45.679 gigabit
  456.789 gigabit
    4.568 terabit
   45.679 terabit
  456.789 terabit
    4.568 petabit
   45.679 petabit
  456.789 petabit
   43.330 millibit
  433.300 millibit
    4.333 bit
   45.333 bit
  456.333 bit
    4.567 kilobit
   45.678 kilobit
  456.789 kilobit
  467.890 kilobit
   45.679 megabit
  456.789 megabit
    4.568 gigabit
   45.679 gigabit
  456.789 gigabit
    4.568 terabit
   45.679 terabit
  456.789 terabit
    4.568 petabit
   45.679 petabit
  456.789 petabit
    4.568 exabit
   45.679 exabit
  456.789 exabit
    4.568 zettabit
   45.679 zettabit
  456.789 zettabit
    4.568 yottabit
   45.679 yottabit
  456.789 yottabit
    4.568e+27 bit
    4.568e+28 bit
    4.568e+29 bit
    4.000 B
   45.000 B
  456.000 B
    4.567 kB
   45.678 kB
  456.789 kB
  467.890 kB
   45.679 MB
  456.789 MB
    4.568 GB
   45.679 GB
  456.789 GB
    4.568 TB
   45.679 TB
  456.789 TB
    4.568 PB
   45.679 PB
  456.789 PB
   43.330 mB
  433.300 mB
    4.333 B
   45.333 B
  456.333 B
    4.567 kB
   45.678 kB
  456.789 kB
  467.890 kB
   45.679 MB
  456.789 MB
    4.568 GB
   45.679 GB
  456.789 GB
    4.568 TB
   45.679 TB
  456.789 TB
    4.568 PB
   45.679 PB
  456.789 PB
    4.568 EB
   45.679 EB
  456.789 EB
    4.568 ZB
   45.679 ZB
  456.789 ZB
    4.568 YB
   45.679 YB
  456.789 YB
    4.568e+27 B
    4.568e+28 B
    4.568e+29 B
    4.000 bit
   45.000 bit
  456.000 bit
    4.460 kibibit
   44.607 kibibit
  446.083 kibibit
  456.924 kibibit
   43.563 mebibit
  435.628 mebibit
    4.254 gibibit
   42.542 gibibit
  425.418 gibibit
    4.154 tebibit
   41.545 tebibit
  415.447 tebibit
    4.057 pebibit
   40.571 pebibit
  405.710 pebibit
    4.333e-02 bit
    4.333e-01 bit
    4.333 bit
   45.333 bit
  456.333 bit
    4.460 kibibit
   44.608 kibibit
  446.083 kibibit
  456.924 kibibit
   43.563 mebibit
  435.628 mebibit
    4.254 gibibit
   42.542 gibibit
  425.418 gibibit
    4.154 tebibit
   41.545 tebibit
  415.447 tebibit
    4.057 pebibit
   40.571 pebibit
  405.710 pebibit
    3.962 exbibit
   39.620 exbibit
  396.201 exbibit
    3.869 zebibit
   38.692 zebibit
  386.915 zebibit
    3.778 yobibit
   37.785 yobibit
  377.847 yobibit
    4.568e+27 bit
    4.568e+28 bit
    4.568e+29 bit
    4.000 B
   45.000 B
  456.000 B
    4.460 KiB
   44.607 KiB
  446.083 KiB
  456.924 KiB
   43.563 MiB
  435.628 MiB
    4.254 GiB
   42.542 GiB
  425.418 GiB
    4.154 TiB
   41.545 TiB
  415.447 TiB
    4.057 PiB
   40.571 PiB
  405.710 PiB
    4.333e-02 B
    4.333e-01 B
    4.333 B
   45.333 B
  456.333 B
    4.460 KiB
   44.608 KiB
  446.083 KiB
  456.924 KiB
   43.563 MiB
  435.628 MiB
    4.254 GiB
   42.542 GiB
  425.418 GiB
    4.154 TiB
   41.545 TiB
  415.447 TiB
    4.057 PiB
   40.571 PiB
  405.710 PiB
    3.962 EiB
   39.620 EiB
  396.201 EiB
    3.869 ZiB
   38.692 ZiB
  386.915 ZiB
    3.778 YiB
   37.785 YiB
  377.847 YiB
    4.568e+27 B
    4.568e+28 B
    4.568e+29 B
nil


