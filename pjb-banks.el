;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-banks.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports functions to format back movement listings
;;;;    fetched from the web.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2004-10-18 <PJB> Updated. Added generation of journal-entries 
;;;;                     in from-cajamar-mozilla.
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
(require 'pjb-euro)
(require 'pjb-strings)
(provide 'pjb-banks)


(defun clean-number-string (number-string)
  "
RETURN: A cleaned string where the comas are replaced by dots, 
        and spaces and dashes are removed.
"
  (if number-string
    (dolist (p-r '( ("[ -][ -]*"               .  "")
                    ("[.,]"                    .  "@")
                    ("\\(.*\\)@\\([^@]*\\)$"   .  "\\1.\\2")
                    ("@"                       .  "") ))
      (setq number-string (replace-regexp-in-string
                           (car p-r) (cdr p-r) number-string t nil)))
    (setq number-string ""))
  number-string);;clean-number-string


(defun pjb-bank-clean-description (description)
  (dolist (p-r '( ("[ \f\t\n\r\v]+"   . " ")
                  ("^ "               . "")
                  (" $"               . "") ))
    (setq description (replace-regexp-in-string
                       (car p-r) (cdr p-r) description t t)))
  description);;pjb-bank-clean-description


(defun pjb-string-amount-list (list)
  (do* ((items list (cdr items))
        (item (car items) (car items))
        (i 0)
        (result '()))
      ((null items) (apply (function concatenate) 'string (nreverse result)))
    (unless (zerop (length (string-trim " " item)))
      (incf i)
      (push (format "%10s" item) result)
      (when (zerop (mod i 6))
        (push "\n;           " result)))));;pjb-string-amount-list


(defun pjb-bank-format-line (year month day credit debit description)
  (format "%04d-%02d-%02d %10s %10s %s\n\n"
    year month day credit debit 
    (if (< (length description) 48)
      description
      (substring (string-justify-left description 79 33) 33)))
  );;pjb-bank-format-line

   
;; ---------------------------------------------------------------------

(defun from-cajamar-mozilla ()
  "Convert the lines following the point as from CajaMar
This is the most up-to-date implementation.
Use Mozilla, and copy-and-paste to emacs."
  (interactive)
  (unless (re-search-forward (concat "Fecha\\(.\\|\n\\)*Moneda\n"
                                     "\\(\\(.\\|\n\\)*\\)\n"
                                     ".*NO HAY MAS MOVIMIENTOS") nil t)
    (message 
     "Nothing found! (Needs from 'Fecha' to 'NO HAY MAS MOVIMIENTOS').")
    (return))
  (let* ((source (match-string 2))
         (lines (split-string (replace-regexp-in-string  "\n" " " source t t) 
                              " *eur\\. *"))
         credits debits solde 
         (journal '()))
    (printf "\n\n") ;;; ###
    (dolist (line lines)
      (when 
          (string-match
           (concat 
            "^"
            "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) *\t"
            "[0-9][0-9]-[0-9][0-9] *\t"
            "\\(.*[^ ]\\) *\t"
            "\\( \\|[-0-9][0-9,.]*\\) *\t"
            "\\( \\|[-0-9][0-9,.]*\\) *\t"
            "\\([-0-9][0-9,.]*\\) *\t"
            "$"
            ) line)
        (let* ((d   (string-to-number (match-string-no-properties 1 line)))
               (m   (string-to-number (match-string-no-properties 2 line)))
               (y   (string-to-number (match-string-no-properties 3 line)))
               (mv m) ;; should get them from input lines...
               (dv d) ;; should get them from input lines...
               (lab (match-string-no-properties 4 line))
               (deb (match-string-no-properties 5 line))
               (cre (match-string-no-properties 6 line))
               (sol (match-string-no-properties 7 line))
               ;; (mat (match-lines))
               (lin) (ori))
          (setf deb (string-trim " " deb)
                cre (string-trim " " cre)
                y (+ y (cond ((and (= mv 12) (= m 1)) 1999)
                             ((and (= mv 1) (= m 12)) 2001)
                             (t                       2000)))
                cre (clean-number-string cre)
                deb (clean-number-string deb)
                lab (pjb-bank-clean-description lab)
                lin (pjb-bank-format-line  y m d cre deb lab)
                credits (append credits (list cre))
                debits  (append debits  (list deb))
                solde   sol)
          (push `(journal-entry
                  ,(format "\"%04d-%02d-%02d\"" y m d)
                  ,(format "#m%.2f" (if (zerop (length cre))
                                      (- (car (read-from-string deb)))
                                      (car (read-from-string cre))))
                  ,(cond
                    ((string-match "HACIENDA\\|SEGURIDAD" lab) "0/100")
                    ((zerop (length deb)) "0/100")
                    (t "16/100"))
                  "\"FISCALID\"" 
                  ,(if (or (string-match "FRA.N.: \\(.*\\)" lab) 
                          (string-match "RECIBO REF:\\([^ ]*\\)" lab)
                          (string-match "JUSTIFICANTE: \\([^ ]*\\)" lab))
                    (format "\"%s\"" (match-string-no-properties 1 lab))
                    "\"FACTURA\"")
                  ,(format "\"%s\""
                     (if (< (length lab) 48) lab  
                         (replace-regexp-in-string
                          "\n" "\n                "
                          (string-trim " " (string-justify-left lab 48)))))
                  ,(cond
                    ((string-match "HACIENDA\\|SEGURIDAD" lab)
                     ":IMPUESTO")
                    ((zerop (length deb)) ":PRESTACION-INTRACOMUNITARIA")
                    (t ":GASTO-CORRIENTE")))
                journal)
          (insert lin))))
    (printf "; crédits : %s\n" (pjb-string-amount-list credits))
    (printf "; débits  : %s\n" (pjb-string-amount-list debits))
    (printf "; solde   : %s\n\n\n" solde)
    (dolist (entry (reverse journal))
      (printf "(%s %s %s %s\n  %s %s\n  %s\n  %s)\n\n"
              (elt entry 0)(elt entry 1)(elt entry 2)
              (elt entry 3)(elt entry 4)(elt entry 5)
              (elt entry 6)(elt entry 7)))));;from-cajamar-mozilla



(defun from-cajamar-2003-mozilla ()
  "Convert the lines following the point as from CajaMar
This is the most up-to-date implementation.
Use Mozilla, and copy-and-paste to emacs."
  (interactive)
  (unless (re-search-forward (concat "Fecha\\(.\\|\n\\)*Moneda\n"
                                     "\\(\\(.\\|\n\\)*\\)\n"
                                     ".*NO HAY MAS MOVIMIENTOS") nil t)
    (message 
     "Nothing found! (Needs from 'Fecha' to 'NO HAY MAS MOVIMIENTOS').")
    (return))
  (let* ((source (match-string 2))
         (lines (split-string (replace-regexp-in-string  "\n" " " source t t) 
                              " *eur\\. *"))
         credits debits solde)
    (printf "\n\n") ;;; ###
    (dolist (line lines)
      (when 
          (string-match
           (concat 
            "^"
            "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) "
            "[0-9][0-9]-[0-9][0-9] "
            "\\(.*[^ ]\\) "
            "\\( \\|[-0-9][0-9,.]*\\) "
            "\\( \\|[-0-9][0-9,.]*\\) "
            "\\( \\|[-0-9][0-9,.]*\\)"
            "$"
            ) line)
        (let* ((d   (string-to-number (match-string-no-properties 1 line)))
               (m   (string-to-number (match-string-no-properties 2 line)))
               (y   (string-to-number (match-string-no-properties 3 line)))
               (mv m) ;; should get them from input lines...
               (dv d) ;; should get them from input lines...
               (lab (match-string-no-properties 4 line))
               (deb (match-string-no-properties 5 line))
               (cre (match-string-no-properties 6 line))
               (sol (match-string-no-properties 7 line))
               ;; (mat (match-lines))
               (lin) (ori))
          (message "%S %S %S" deb cre sol)
          (setf deb (string-trim " " deb)
                cre (string-trim " " cre)
                y (+ y (cond ((and (= mv 12) (= m 1)) 1999)
                             ((and (= mv 1) (= m 12)) 2001)
                             (t                       2000)))
                cre (clean-number-string cre)
                deb (clean-number-string deb)
                lab (pjb-bank-clean-description lab)
                lin (pjb-bank-format-line  y m d cre deb lab)
                credits (append credits (list cre))
                debits  (append debits  (list deb))
                solde   sol)
          (insert lin))))
    (printf "; crédits : %s\n" (pjb-string-amount-list credits))
    (printf "; débits  : %s\n" (pjb-string-amount-list debits))
    (printf "; solde   : %s\n" solde)));;from-cajamar-2003-mozilla


(defun from-cajamar-table ()
  "Convert the lines following the point as from Caja Rural
See: from-cajamar-mozilla"
  (interactive)
  (let ((credits '())
        (debits  '())
        (movements '()))
    (while (re-search-forward 
            (concat 
             "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)|"
             "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)|"
             "\\([^|]*\\)|"
             "\\([-0-9][0-9,.]*\\)|"
             "\\([-0-9][0-9,.]*\\)|"
             "\\([-0-9][0-9,.]*\\)|\n"
             ) nil t)
      (let* ((d   (string-to-number (match-string-no-properties 1)))
             (m   (string-to-number (match-string-no-properties 2)))
             (y   (string-to-number (match-string-no-properties 3)))
             (dv  (string-to-number (match-string-no-properties 4)))
             (mv  (string-to-number (match-string-no-properties 5)))
             yv
             (lab (match-string-no-properties 6))
             (deb (match-string-no-properties 7))
             (cre (match-string-no-properties 8))
             (sol (match-string-no-properties 9))
             (mat (match-data))
             lin ori oridev)
        ;; y is given as YY.
        (cond ((and (= mv 12) (= m 1)) (setq yv (+ y 1999) y (+ y 2000)))
              ((and (= mv 1) (= m 12)) (setq yv (+ y 2001) y (+ y 2000)))
              (t                       (setq yv (+ y 2000) y (+ y 2000))))
        (setq cre (clean-number-string cre))
        (setq deb (clean-number-string deb))
        ;; (insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
        (setq lab (replace-regexp-in-string "^ " "" (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\\.\\.\\.*" "." (replace-regexp-in-string "\n" " " lab t t) t t) t t) t t))
        (setq ori (replace-regexp-in-string ".*ORIGEN: *\\([0-9,.][0-9,.]*\\)-? \\(PTA\\|EUR\\).*\\**" "\\1 \\2" lab t nil))
        (setq lab (replace-regexp-in-string " \\*\\*\\*.*ORIGEN:.*" "" lab t t))
        (if (string-equal ori lab)
          (setq ori "") ;; When there's no ORIGEN!
          (setq oridev (substring ori -3))
          (setq ori    (substring ori 0 -4))
          (if (string-equal "PTA" oridev)
            (setq ori (concat (replace-regexp-in-string "\\.00$" "" (clean-number-string ori) t t) " ESP"))
            (setq ori (concat (clean-number-string ori) " EUR"))))
        (setq movements (nconc movements 
                               (nconc (list
                                       :operation-date (list 0 0 0 d m y)
                                       :value-date     (list 0 0 0 dv mv yv)
                                       :description    lab
                                       :balance        sol
                                       :devise         :EUR
                                       )
                                      (if (/= 0 cre)
                                        (list :credit cre)
                                        (list :debit deb))
                                      )))
        (setq lin (pjb-bank-format-line y m d cre deb lab))
        (setq credits (nconc credits  cre))
        (setq debits  (nconc debits   deb))
        (store-match-data mat)
        (replace-match lin nil nil)))
    (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
    (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
    (insert (format "(setq movements '%S)\n" movements))
    ));;from-cajamar-table


(defun from-cajamar ()
  "Convert the lines following the point as from CajaMar
See: from-cajamar-mozilla"
  (interactive)
  (if (re-search-forward "\\(\\(Fecha\\(.\\|\n\\)*\\)?Saldo.*Moneda *\\(\\(.\\|\n\\)*\\)NO HAY MAS MOVIMIENTOS\\)" nil t)
    (let ((data (split-string (replace-regexp-in-string "\n" " " (match-string 4) t t)
                              " *eur\\. *"))
          credits debits solde)
      (printf "\n\n") ;;; ###
      (while data
        ;; (printf "%S\n" (car data))
        (when  (string-match
                (concat 
                 "^"
                 "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)  "
                 "[-0-9]*  "
                 "\\(.*[^ ]\\)  "
                 "\\(\\([-0-9][0-9,.]*\\)     \\|   \\([-0-9][0-9,.]*\\)  \\)"
                 "\\([-0-9][0-9,.]*\\)"
                 "$"
                 ) (car data))
          (let* ((d   (string-to-number 
                       (match-string-no-properties 1 (car data))))
                 (m   (string-to-number 
                       (match-string-no-properties 2 (car data))))
                 (y   (string-to-number 
                       (match-string-no-properties 3 (car data))))
                 (mv m) ;; should get them from input data...
                 (dv d) ;; should get them from input data...
                 (lab (match-string-no-properties 4 (car data)))
                 (deb (match-string-no-properties 6 (car data)))
                 (cre (match-string-no-properties 7 (car data)))
                 (sol (match-string-no-properties 8 (car data)))
                 ;; (mat (match-data))
                 (lin) (ori))
            (if deb (setq cre "") (setq deb ""))
            ;; (printf "%S\n%S\n%S\n" deb cre sol)
            (setq y (+ y (cond ((and (= mv 12) (= m 1)) 1999)
                               ((and (= mv 1) (= m 12)) 2001)
                               (t                       2000))))
            (setq cre (clean-number-string cre))
            (setq deb (clean-number-string deb))
            ;; (insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
            (message "lib=%S" lab)
            (setq lab (pjb-bank-clean-description lab))
            (message "lab=%S" lab)
            (setq lin (pjb-bank-format-line  y m d cre deb lab))
            (setq credits (append credits (list cre)))
            (setq debits  (append debits  (list deb)))
            (setq solde   sol)
            (insert lin)))
        (setq data (cdr data)))
      (printf "; crédits : %s\n" (pjb-string-amount-list credits))
      (printf "; débits  : %s\n" (pjb-string-amount-list debits))
      (printf "; solde   : %s\n" solde))
    (message "Nothing found! (Need from 'Fecha' to 'NO HAY MAS MOVIMIENTOS').")))


(defun from-cajamar-old ()
  "Convert the lines following the point as from Caja Rural"
  (interactive)
  (let (credits debits)
    (while  (re-search-forward 
             (concat 
              "  *\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]*\\) *\n"
              "       *\\([0-9][0-9]\\)-\\([0-9][0-9]\\) *\n"
              "\\(\\(        .*\n\\)+\\)"
              "\\(                                *[-0-9][0-9,.]*\\)? *\n"
              "\\(                                *[-0-9][0-9,.]*\\)? *\n"
              "\\(                                *[-0-9][0-9,.]*\\)? *\n"
              "                                *eur. *\n"
              ) nil t)
      (let* ((d   (string-to-number (match-string-no-properties 1)))
             (m   (string-to-number (match-string-no-properties 2)))
             (y   (string-to-number (match-string-no-properties 3)))
             (dv  (string-to-number (match-string-no-properties 4)))
             (mv  (string-to-number (match-string-no-properties 5)))
             (lab (match-string-no-properties 6))
             (deb (match-string-no-properties 8))
             (cre (match-string-no-properties 9))
             (sol (match-string-no-properties 10))
             (mat (match-data))
             (lin) (ori))
        ;;(show d m y dv mv lab deb cre sol)
        (when (< y 100)
          (setq y (+ y (cond ((and (= mv 12) (= m 1)) 1999)
                             ((and (= mv 1) (= m 12)) 2001)
                             (t                       2000)))))
        (setq cre (clean-number-string cre))
        (setq deb (clean-number-string deb))
        ;; (insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
        (setq lab (pjb-bank-clean-description lab))
        (setq lin (pjb-bank-format-line  y m d cre deb lab))
        (setq credits (append credits (list cre)))
        (setq debits  (append debits  (list deb)))
        (store-match-data mat)
        (replace-match lin nil nil)))
    (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
    (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
    ));;from-cajamar-old


;; ---------------------------------------------------------------------


(defun from-patagon-table ()
  "Convert the lines following the point as from Patagon"
  (interactive)
  (let (credits debits)
    (while 
        (re-search-forward 
         (concat
          "||"
          "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9][0-9][0-9]\\)|"
          "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)|"
          "\\([^|]*\\)|"
          "\\([-,.0-9][,.0-9]*\\)|"
          "\\([-,.0-9][,.0-9]*\\)|\n") nil t)
      (let ((d   (match-string-no-properties 1))
            (m   (match-string-no-properties 2))
            (y   (match-string-no-properties 3))
            (dv  (match-string-no-properties 4))
            (mv  (match-string-no-properties 5))
            (lab (match-string-no-properties 6))
            (imp (match-string-no-properties 7))
            (sol (match-string-no-properties 8))
            (mat (match-data))
            (lin)
            (cre) (deb))
                                        ; (insert  (format "D: %s %s %s  V: %s %s  [%s]  I: %s  S:%s\n" d m y dv mv lab imp sol )))))
        (if (= ?- (string-to-char imp))
          (progn 
            (setq deb (clean-number-string imp))
            (setq cre ""))
          (progn
            (setq deb "")
            (setq cre (clean-number-string imp))))
        (setq lab (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\n" " " lab t t) t t))
        (setq lab (substring (string-justify-left lab 66 41) 41))
        (setq lin (format "%4s-%2s-%2s %10s %10s         %s\n\n"
                    y m d cre deb lab))
        (setq credits (append credits (list cre)))
        (setq debits  (append debits  (list deb)))
        (store-match-data mat)
        (replace-match lin nil nil)
        (printf "%s\n" lin)
        )) ;;while
    (printf "; crédits : %s\n" (pjb-string-amount-list credits))
    (printf "; débits  : %s\n" (pjb-string-amount-list debits))
    ));;from-patagon-table




;;; ;; ---------------------------------------------------------------------
;;; ;; old functions not used anymore.
;;; 
;;; (defun from-evolvebank-table ()
;;;   "Convert the lines following the point as from Evolve Bank"
;;;   (interactive)
;;;   (let (credits debits)
;;;     (while (re-search-forward 
;;;             (concat 
;;;              "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)|"
;;;              "[/0-9]*|"
;;;              "[^|]*|"
;;;              "\\([^|]*\\)|"
;;;              "\\([-0-9]?[0-9,.]*\\)\\( Pta.\\)?|"
;;;              "\\([-0-9]?[0-9,.]*\\)\\( Pta.\\)?|"
;;;              "\\([-0-9]?[0-9,.]*\\)\\( Pta.\\)?|"
;;;              "\n"
;;;              ) nil t)
;;;       (let ((d   (match-string-no-properties 1))
;;;             (m   (match-string-no-properties 2))
;;;             (y   (match-string-no-properties 3))
;;;             (lab (match-string-no-properties 4))
;;;             (deb (match-string-no-properties 5))
;;;             (cre (match-string-no-properties 7))
;;;             (sol (match-string-no-properties 9))
;;;             (mat (match-data))
;;;             (lin) (ori))
;;;         (setq cre (clean-number-string (replace-regexp-in-string "\\." "" cre t t)))
;;;         (setq deb (clean-number-string (replace-regexp-in-string "\\." "" deb t t)))
;;; ;;(insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
;;;         (setq lab (replace-regexp-in-string "^ " "" (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\\.\\.\\.*" "." (replace-regexp-in-string "\n" " " lab t t) t t) t t) t t))
;;;         (setq ori (concat (if (string-equal "" cre) deb cre) " ESP"))
;;;         (if (not (string-equal "" cre))
;;;             (setq cre (format "%.2f" 
;;;                               (euro-from-value (string-to-number cre) 'ESP))))
;;;         (if (not (string-equal "" deb))
;;;             (setq deb (format "%.2f" 
;;;                               (euro-from-value (string-to-number deb) 'ESP))))
;;;         (if (> (length lab) 24)
;;;             (progn
;;;               (setq lab (substring (string-justify-left lab 66 41) 41))
;;;               (setq lin (format "20%2s-%2s-%2s %10s %10s         %-s%14s\n\n"
;;;                             y m d cre deb lab ori)))
;;; 
;;;             (setq lin (format "20%2s-%2s-%2s %10s %10s         %-25s%14s\n\n"
;;;                               y m d cre deb lab ori)))
;;;         (setq credits (append credits (list cre)))
;;;         (setq debits  (append debits  (list deb)))
;;;         (store-match-data mat)
;;;         (replace-match lin nil nil)))
;;; 
;;;     (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
;;;     (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
;;;     ));;from-evolvebank-table
;;; 
;;; 
;;; (defun from-evolvebank-xls-esp ()
;;;   "Convert the lines following the point as from Evolve Bank (text)."
;;;   (interactive)
;;;   (let (credits debits)
;;;     (while (re-search-forward 
;;;             (concat 
;;;              "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)	"
;;;              "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)	"
;;;              "\\([^	]*\\)	"
;;;              "\\([-0-9]?[0-9,.]*\\)	"
;;;              "\\([-0-9]?[0-9,.]*\\)	"
;;;              "\\([-0-9]?[0-9,.]*\\)\n"
;;;              ) nil t)
;;;       (let ((d   (match-string-no-properties 1))
;;;             (m   (match-string-no-properties 2))
;;;             (y   (match-string-no-properties 3))
;;;             (lab (match-string-no-properties 7))
;;;             (deb (match-string-no-properties 8))
;;;             (cre (match-string-no-properties 9))
;;;             (sol (match-string-no-properties 10))
;;;             (mat (match-data))
;;;             (lin) (ori))
;;;         (setq cre (clean-number-string (replace-regexp-in-string "\\." "" cre t t)))
;;;         (setq deb (clean-number-string (replace-regexp-in-string "\\." "" deb t t)))
;;; ;;(insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
;;;         (setq lab (replace-regexp-in-string "^ " "" (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\\.\\.\\.*" "." (replace-regexp-in-string "\n" " " lab t t) t t) t t) t t))
;;;         (setq ori (concat (if (string-equal "" cre) deb cre) " ESP"))
;;;         (if (not (string-equal "" cre))
;;;             (setq cre (format "%.2f" 
;;;                               (euro-from-value (string-to-number cre) 'ESP))))
;;;         (if (not (string-equal "" deb))
;;;             (setq deb (format "%.2f" 
;;;                               (euro-from-value (string-to-number deb) 'ESP))))
;;;         (if (> (length lab) 24)
;;;             (progn
;;;               (setq lab (substring (string-justify-left lab 66 41) 41))
;;;               (setq lin (format "20%2s-%2s-%2s %10s %10s         %-s%14s\n\n"
;;;                             y m d cre deb lab ori)))
;;; 
;;;             (setq lin (format "20%2s-%2s-%2s %10s %10s         %-25s%14s\n\n"
;;;                               y m d cre deb lab ori)))
;;;         (setq credits (append credits (list cre)))
;;;         (setq debits  (append debits  (list deb)))
;;;         (store-match-data mat)
;;;         (replace-match lin nil nil)))
;;; 
;;;     (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
;;;     (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
;;;     ));;from-evolvebank-xls-esp
;;; 
;;; 
;;; (defun from-openbank ()
;;;   "Convert the lines following the point as from OpenBank"
;;;   (interactive)
;;; 
;;;   (while 
;;;       (re-search-forward 
;;;        (concat
;;;         " *\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9][0-9][0-9]\\) *\n"
;;;         " *\\([0-9][0-9]\\)-\\([0-9][0-9]\\) *\n"
;;;         "\\(\\( *[^ ].*\n\\)*\\)"
;;;         " *\\([-,.0-9][,.0-9]*\\) *\n"
;;;         " *\\([-,.0-9][-,.0-9]*\\) *\n") nil t)
;;;       (let ((d   (match-string-no-properties 1))
;;;             (m   (match-string-no-properties 2))
;;;             (y   (match-string-no-properties 3))
;;;             (dv  (match-string-no-properties 4))
;;;             (mv  (match-string-no-properties 5))
;;;             (lab (match-string-no-properties 6))
;;;             (imp (match-string-no-properties 8))
;;;             (sol (match-string-no-properties 9))
;;;             (mat (match-data))
;;;             (lin)
;;;             (cre) (deb))
;;; 
;;; ; (insert  (format "D: %s %s %s  V: %s %s  [%s]  I: %s  S:%s\n" d m y dv mv lab imp sol )))))
;;;         
;;;         (if (= ?- (string-to-char imp))
;;;             (progn 
;;;               (setq deb (clean-number-string imp))
;;;               (setq cre ""))
;;;             (progn
;;;               (setq deb "")
;;;               (setq cre (clean-number-string imp))))
;;; 
;;;         (setq lab (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\n" " " lab t t) t t))
;;;         (setq lab (substring (string-justify-left lab 66 41) 41))
;;;         (setq lin (format "%4s-%2s-%2s %10s %10s         %s\n"
;;;                         y m d cre deb lab))
;;;         (store-match-data mat)
;;;         (replace-match lin nil nil))));;from-openbank
;;; 
;;; 
;;; (defun from-paritate ()
;;;   "Convert the lines following the point as from Paritate Bank"
;;;   (interactive)
;;;   
;;;   (while 
;;;       (re-search-forward 
;;; "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)   \\(................................\\)\\* *\\([0-9][0-9.,]*[CD]\\) *\n\\(\\( [A-Z].*\n\\)*\\) *\\([0-9][0-9]*\\) *\\(#[0-9][0-9]* [0-9][0-9]* [0-9][0-9]*\\) *\\([0-9]*\\) *\\* *\\([0-9][0-9.,]*\\) *\n" nil t)
;;;       (let ((m   (match-string-no-properties 1))
;;;             (d   (match-string-no-properties 2))
;;;             (y   (match-string-no-properties 3))
;;;             (lab (match-string-no-properties 4))
;;;             (inc (match-string-no-properties 5))
;;;             (bal (match-string-no-properties 6))
;;;             (ref (match-string-no-properties 8))
;;;             (cnt (match-string-no-properties 9))
;;;             (doc (match-string-no-properties 10))
;;;             (sol (match-string-no-properties 11))
;;;             (mat (match-data))
;;;             (cre "")
;;;             (deb "")
;;;             (lin))
;;; 
;;;         (if (= 67 (car (last (string-to-list inc))))
;;;             (setq cre (substring inc 0 (- (length inc) 1)))
;;;             (setq deb (substring inc 0 (- (length inc) 1))))
;;;         (setq cre (clean-number-string cre))
;;;         (setq deb (clean-number-string deb))
;;;         (setq lab (replace-regexp-in-string " *$" "" lab t t))
;;;         (setq lab (substring (string-justify-left lab 66 41) 41))
;;;         (setq bal (replace-regexp-in-string " *$" "" bal t t))
;;;         (if (STRING= "" bal)
;;;             (setq lin (format "20%2s-%2s-%2s %10s %10s %s\n"
;;;                               y m d cre deb lab))
;;;             (setq lin (format "20%2s-%2s-%2s %10s %10s %s\n%s\n"
;;;                               y m d cre deb lab bal))
;;;             )
;;;         (store-match-data mat)
;;;         (replace-match lin nil nil))));;from-paritate
;;;
;;; (defun from-evolvebank-xls-eur ()
;;;   "Convert the lines following the point as from Evolve Bank (text)."
;;;   (interactive)
;;;   (let (credits debits)
;;;     (while (re-search-forward 
;;;             (concat 
;;;              "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)	"
;;;              "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)	"
;;;              "\\([^	]*\\)	"
;;;              "\\([-0-9]?[0-9,.]*\\)	"
;;;              "\\([-0-9]?[0-9,.]*\\)	"
;;;              "\\([-0-9]?[0-9,.]*\\)\n"
;;;              ) nil t)
;;;       (let ((d   (match-string-no-properties 1))
;;;             (m   (match-string-no-properties 2))
;;;             (y   (match-string-no-properties 3))
;;;             (lab (match-string-no-properties 7))
;;;             (deb (match-string-no-properties 8))
;;;             (cre (match-string-no-properties 9))
;;;             (sol (match-string-no-properties 10))
;;;             (mat (match-data))
;;;             lin ori oridev)
;;;         (setq cre (clean-number-string cre))
;;;         (setq deb (clean-number-string deb))
;;; ;;(insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
;;;         (setq lab (replace-regexp-in-string "^ " "" (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\\.\\.\\.*" "." (replace-regexp-in-string "\n" " " lab t t) t t) t t) t t))
;;;         (setq ori (concat (if (string-equal "" cre) deb cre) " ESP"))
;;;         (if (not (string-equal "" cre))
;;;             (setq cre (format "%.2f" (string-to-number cre)
;;;                               )))
;;;         (if (not (string-equal "" deb))
;;;             (setq deb (format "%.2f" (string-to-number deb)
;;;                               )))
;;;         (if (> (length lab) 24)
;;;             (progn
;;;               (setq lab (substring (string-justify-left lab 66 41) 41))
;;;               (setq lin (format "20%2s-%2s-%2s %10s %10s         %-s%14s\n\n"
;;;                             y m d cre deb lab ori)))
;;;             (setq lin (format "20%2s-%2s-%2s %10s %10s         %-25s%14s\n\n"
;;;                               y m d cre deb lab ori)))
;;;         (setq credits (append credits (list cre)))
;;;         (setq debits  (append debits  (list deb)))
;;;         (store-match-data mat)
;;;         (replace-match lin nil nil)))
;;;     (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
;;;     (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
;;;     ));;from-evolvebank-xls-eur


;;;; pjb-banks.el                     --                     --          ;;;;
