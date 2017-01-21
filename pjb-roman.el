;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-roman.el
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
;;;;    199?/??/?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2011
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
(provide 'pjb-roman)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; triple point of water is 273.16 K
;; ice point is 273.15 K
;; Celcius and Fahrenheit are defined in terms of ice point.

(defun cefak (k)
  "Insert at point a string with degree Kelvin, Celcius and Fahrenheit.
SEE ALSO: fahrenheit (F), celcius (C) and kelvin (K)."
  (funcall (if buffer-file-read-only
               (function insert)
               (function message))
           (format "%10.2f K  %10.2f °C  %10.2f °F   %s"
                  k
                  (- k 273.15)
                  (+ 32.0 (* 1.8 (- k 273.15)))
                  (let ((c (- k 273.15)))
                   (cond
                     ((<= k 0) "absolutely cold")
                     ((< c -30) "extremely cold")
                     ((< c 0) "very cold")
                     ((< c 10) "cold")
                     ((< c 15) "supportable")
                     ((< c 20) "not so bad")
                     ((< c 25) "good")
                     ((< c 35) "warm")
                     ((< c 45) "hot")
                     ((< c 55) "rather hot")
                     ((< c 65) "very hot")
                     ((< c 100) "quite really hot")
                     ((< c 10000) "definitely hot")
                     ((< c 100000) "extremely hot")
                     ((< c 1000000) "you won't believe how hot this is hot"))))))

(defun fahrenheit (f)
  "Convert degrees Fahrenheit to Celcius and Kelvin."
  (interactive "XDegrees Fahrenheit: ")
  (cefak (+ 273.15 (/ (- f 32) 1.8))))

(defun celcius (c)
   "Convert degrees Celcius to Fahrenheit and Kelvin."
   (interactive "XDegrees Celcius: ")
   (cefak (+ 273.15 c)))

(defun kelvin (k)
  "Convert degrees Kelvin to Fahrenheit and Celcius."
  (interactive "XDegrees Kelvin: ")
  (cefak k))

(defalias 'C 'celcius)
(defalias 'F 'fahrenheit)
(defalias 'K 'kelvin)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Règles d'écriture des nombres romains :
;; Les chiffres M, C, X, I ne peuvent pas être répétés plus de quatre fois.
;; Les chiffres D, L, V ne peuvent pas être répétés.
;; Les chiffres doivent être écris dans l'ordre : M, D, C, L, X, V, I ;
;; sauf que un I peut précéder V, X, L, ou C, et un X peut précéder L ou C.


;;  romain : milliers centaines .
;;
;;  milliers : | M | M M | M M M | M M M M .
;;
;;  centaines : centaine4 dizaines | D centaine4 dizaines .
;;  centaine4 : | C | C C | C C C | C C C C .
;;
;;  dizaines : dizaine8 | X C unite8 | I C .
;;  dizaine8 : dizaine4 unites | I L | L dizaine3 unites
;;  dizaine4 : dizaine3 | X L .
;;  dizaine3 : | X | X X | X X X .
;;
;;  unites : unite8 | I X .
;;  unite8 : unite4 | V unite3 .
;;  unite4 : unite3 | I V .
;;  unite3 : | I | I I | I I I .


;;----------------------------------------------------------------------
;; Parser
;; ( remaining_char_list value )
;;

(defun roman-parser-make (string)
  (list (string-to-list string) 0));;roman-parser-make


(defun roman-parser-curr-char (parser)
  (car (car parser)));;roman-parser-curr-char


(defun roman-parser-next-char (parser)
  (car (cdr (car parser))));;roman-parser-next-char


(defun roman-parser-fetch-char (parser)
  (setcar parser (cdr (car parser))));;roman-parser-fetch-char


(defun roman-parser-string (parser)
  (if (roman-parser-curr-char parser)
      (apply 'string (car parser))
    ""));;roman-parser-string


(defun roman-parser-value (parser)
  (car (cdr parser)));;roman-parser-value


(defun roman-parser-value-add (parser increment)
  (setcar (cdr parser) (+ (car (cdr parser)) increment)));;roman-parser-value-add


;;----------------------------------------------------------------------
;; Roman parser
;;

(defun from-roman (r)
  ;;  romain : milliers centaines .
  (let ((p (roman-parser-make r)))
     (from-roman-thousands p)
     (from-roman-hundreds  p)
     (if (roman-parser-curr-char p)
         (error (format "Superfluous characters '%s'." (roman-parser-string p))))
     (roman-parser-value p)));;from-roman


(defun from-roman-thousands (p)
  ;;  milliers : | M | M M | M M M | M M M M .
  (let ((i 0))
      (while (and (< i 4) (equal ?M (roman-parser-curr-char p)))
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 1000)
        (setq i (1+ i))))
  (if (equal ?M (roman-parser-curr-char p))
      (error "Too many Ms.")));;from-roman-thousands


(defun from-roman-hundreds (p)
  ;;  centaines : centaine4 dizaines | D centaine4 dizaines .
  (if (equal ?D (roman-parser-curr-char p))
      (progn
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 500)))
  (from-roman-hundred4 p)
  (from-roman-tens p));;from-roman-hundreds


(defun from-roman-hundred4 (p)
  ;;  centaine4 : | C | C C | C C C | C C C C .
  (let ((i 0))
      (while (and (< i 4) (equal ?C (roman-parser-curr-char p)))
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 100)
        (setq i (1+ i))))
  (if (equal ?C (roman-parser-curr-char p))
      (error "Too many Cs.")));;from-roman-hundred4


(defun from-roman-tens (p)
  ;;  dizaines : dizaine8 | X C unite8 | I C .
  (if (equal ?C (roman-parser-next-char p))
      (cond
       ((equal ?I (roman-parser-curr-char p))
        (roman-parser-fetch-char p)
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 99))
       ((equal ?X (roman-parser-curr-char p))
        (roman-parser-fetch-char p)
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 90)
        (from-roman-unit8 p))
       (t (from-roman-ten8 p)))
    (from-roman-ten8 p)));;from-roman-tens


(defun from-roman-ten8 (p)
  ;;  dizaine8 : dizaine4 unites | I L | L dizaine3 unites
  (cond
   ((equal ?I (roman-parser-curr-char p))
    (if (equal ?L (roman-parser-next-char p))
        (progn
          (roman-parser-fetch-char p)
          (roman-parser-fetch-char p)
          (roman-parser-value-add p 49))
      (from-roman-ten4  p)
      (from-roman-units p)))
   ((equal ?L (roman-parser-curr-char p))
    (roman-parser-fetch-char p)
    (roman-parser-value-add p 50)
    (from-roman-ten3  p)
    (from-roman-units p))
   (t
    (from-roman-ten4  p)
    (from-roman-units p))));;from-roman-ten8


(defun from-roman-ten4 (p)
  ;;  dizaine4 : dizaine3 | X L .
  (if (and
       (equal ?X (roman-parser-curr-char p))
       (equal ?L (roman-parser-next-char p)))
      (progn
        (roman-parser-fetch-char p)
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 40))
    (from-roman-ten3 p)));;from-roman-ten4


(defun from-roman-ten3 (p)
  ;;  dizaine3 : | X | X X | X X X .
  (let ((i 0))
      (while (and (< i 3) (equal ?X (roman-parser-curr-char p)))
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 10)
        (setq i (1+ i))))
  (if (equal ?X (roman-parser-curr-char p))
      (error "Too many Xs.")));;from-roman-ten3


(defun from-roman-units (p)
  ;;  unites : unite8 | I X .
  (if (and
       (equal ?I (roman-parser-curr-char p))
       (equal ?X (roman-parser-next-char p)))
      (progn
        (roman-parser-fetch-char p)
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 9))
    (from-roman-unit8 p)));;from-roman-units


(defun from-roman-unit8 (p)
  ;;  unite8 : unite4 | V unite3 .
  (if (equal ?V (roman-parser-curr-char p))
      (progn
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 5)
        (from-roman-unit3 p))
    (from-roman-unit4 p)));;from-roman-unit8


(defun from-roman-unit4 (p)
  ;;  unite4 : unite3 | I V .
  (if (and
       (equal ?I (roman-parser-curr-char p))
       (equal ?V (roman-parser-next-char p)))
      (progn
        (roman-parser-fetch-char p)
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 4))
    (from-roman-unit3 p)));;from-roman-unit4


(defun from-roman-unit3 (p)
  ;;  unite3 : | I | I I | I I I .
  (let ((i 0))
      (while (and (< i 3) (equal ?I (roman-parser-curr-char p)))
        (roman-parser-fetch-char p)
        (roman-parser-value-add p 1)
        (setq i (1+ i))))
  (if (equal ?I (roman-parser-curr-char p))
      (error "Too many Is.")));;from-roman-unit3



;;----------------------------------------------------------------------

(defun to-roman-100 (m c r)
  (concat (nth m '("" "M" "MM" "MMM" "MMMM"))
          (nth c '("" "C" "CC" "CCC" "CCCC" "D"
                   "DC" "DCC" "DCCC" "DCCCC"))
          r));;to-roman-100

(defun to-roman-10 (m c d r)
  (to-roman-100 m c (concat (nth d '("" "X" "XX" "XXX" "XL"
                                     "L" "LX" "LXX" "LXXX" "XC")) r)));;to-roman-10

(defun to-roman (n)
  "
RETURN: the number N expressed in roman number notation.
PRE:    1<=N<=4999
"
  (cond
   ((not (integerp n))
    (error "The parameter N must be an integer."))
   ((or (< n 1) (< 4999 n))
    (error "The parameter N must be between 1 and 4999 included.")))

  (let* ((u (% n 10))
         (d (% (/ n 10) 10))
         (c (% (/ n 100) 10))
         (m (% (/ n 1000) 10))
         (r ""))
    (cond
     ((= (+ (* d 10) u) 40) (to-roman-100 m c "XL"))
     ((= (+ (* d 10) u) 49) (to-roman-100 m c "IL"))
     ((= (+ (* d 10) u) 99) (to-roman-100 m c "IC"))
     (t   (to-roman-10 m c d (nth u '("" "I" "II" "III" "IV" "V"
                                      "VI" "VII" "VIII" "IX")))))));;to-roman

;;----------------------------------------------------------------------

(defun region-to-roman ()
  "Replace the region containing a decimal integer by the equivalent roman number."
  (interactive)
  (let ((r (to-roman (string-to-number
                     (buffer-substring (region-beginning) (region-end))))))
    (delete-region (region-beginning) (region-end))
    (insert r)));;region-to-roman

(defun region-from-roman ()
  "Replace the region containing a roman number by the equivalent decimal integer."
  (interactive)
  (let ((n (from-roman  (buffer-substring (region-beginning) (region-end)))))
    (delete-region (region-beginning) (region-end))
    (insert (format "%d" n))));;region-from-roman

;;----------------------------------------------------------------------



;     (for
;      n 1 4999
;      (condition-case cc
;          (let* ((r (to-roman n))
;                 (m (from-roman r)))
;            (or (= n m)
;                (insert (format  "%4d -> %-20s -> %4d %s\n"
;                                 n r m (if (= n m) "" "***")))))
;        ('error
;         (insert (format "ERROR for %4d [%-20s] : %s\n" n (to-roman n)
;                         (car (cdr cc)))))))
;     (for
;      n 1 4999
;      (insert (format "%4d -> %s\n" n (to-roman n))))




;;;; pjb-roman.el                     -- 2003-09-04 04:41:25 -- pascal   ;;;;
