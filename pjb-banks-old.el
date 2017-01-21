;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-banks.el
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
(require 'pjb-euro)
(require 'pjb-strings)
(provide 'pjb-banks)


(defun clean-number-string (number-string)
  "
RETURN: A cleaned string where the comas are replaced by dots,
        and spaces and dashes are removed.
"
  (dolist (p-r '( ("[ -][ -]*"               .  "")
                  ("[.,]"                    .  "@")
                  ("\\(.*\\)@\\([^@]*\\)$"   .  "\\1.\\2")
                  ("@"                       .  "") ))
    (setq number-string (replace-regexp-in-string (car p-r) (cdr p-r) number-string t nil))
    );;dolist
  number-string
  );;clean-number-string


(defun pjb-bank-clean-description (description)

  (dolist (p-r '( ("[ \f\t\n\r\v]+"   . " ")
                  ("^ "               . "")
                  (" $"               . "") ))
    (setq description (replace-regexp-in-string (car p-r) (cdr p-r) description t t))
    );;dolist
  description
  );;pjb-bank-clean-description


(defun pjb-string-amount-list (list)
  (let ((result ""))
    (while list
      (if (not (string-equal (car list) ""))
          (setq result (concat result (format "%10s" (car list)))))
      (setq list (cdr list)))
    result));;pjb-string-amount-list

(defun pjb-bank-format-line (year month day credit debit description)
  (format "%04d-%02d-%02d %10s %10s %s\n\n"
          year month day credit debit
          (if (< (length description) 48)
              description
            (substring (string-justify-left description 79 33) 33)))
  );;pjb-bank-format-line


;; ---------------------------------------------------------------------



(defun from-cajamar-table ()
  "Convert the lines following the point as from Caja Rural"
  (interactive)
  (let (credits debits)
    (while (re-search-forward
            (concat
             "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)|"
             "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)|"
             "\\([^|]*\\)|"
             "\\([-0-9][0-9,.]*\\)|"
             "\\([-0-9][0-9,.]*\\)|"
             "\\([-0-9][0-9,.]*\\)|\n"
             ) nil t)
      (let ((d   (string-to-number (match-string-no-properties 1)))
            (m   (string-to-number (match-string-no-properties 2)))
            (y   (string-to-number (match-string-no-properties 3)))
            (dv  (string-to-number (match-string-no-properties 4)))
            (mv  (string-to-number (match-string-no-properties 5)))
            (lab (match-string-no-properties 6))
            (deb (match-string-no-properties 7))
            (cre (match-string-no-properties 8))
            (sol (match-string-no-properties 9))
            (mat (match-data))
            lin ori oridev)

        (setq y (+ y (cond ((and (= mv 12) (= m 1)) 1999)
                           ((and (= mv 1) (= m 12)) 2001)
                           (t                       2000))))
        (setq m mv)
        (setq d dv)
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
        (setq lin (pjb-bank-format-line y m d cre deb lab))
        (setq credits (append credits (list cre)))
        (setq debits  (append debits  (list deb)))
        (store-match-data mat)
        (replace-match lin nil nil)))

    (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
    (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
    ));;from-cajamar-table

;; ---------------------------------------------------------------------
(defun from-cajamar ()
  "Convert the lines following the point as from CajaMar"
  (interactive)
  (if (re-search-forward "\\(\\(Fecha\\(.\\|\n\\)*\\)?Saldo Moneda *\\(\\(.\\|\n\\)*\\)NO HAY MAS MOVIMIENTOS\\)" nil t)
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
                   (mv m);; should get them from input data...
                   (dv d);; should get them from input data...
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
              (insert lin)
              ));;when
          (setq data (cdr data))
          );;while
        (printf "; crédits : %s\n" (pjb-string-amount-list credits))
        (printf "; débits  : %s\n" (pjb-string-amount-list debits))
        (printf "; solde   : %s\n" solde)
        );;let
    (message "Not found!"))
  );;from-cajamar


(defun from-cajamar-old ()
  "Convert the lines following the point as from Caja Rural"
  (interactive)
  (let (credits debits)
    (while  (re-search-forward
             (concat
              "  \\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) *\n"
              "       *[-0-9]* *\n"
              "\\(\\(        .*\n\\)+\\)"
              "                                *\\([-0-9][0-9,.]*\\)? *\n"
              "                                *\\([-0-9][0-9,.]*\\)? *\n"
              "                                *\\([-0-9][0-9,.]*\\)? *\n"
              "                                *eur. *\n"
              ) nil t)

      (let* ((d   (string-to-number (match-string-no-properties 1)))
             (m   (string-to-number (match-string-no-properties 2)))
             (y   (string-to-number (match-string-no-properties 3)))
             (mv m);; should get them from input data...
             (dv d);; should get them from input data...
             (lab (match-string-no-properties 4))
             (deb (match-string-no-properties 6))
             (cre (match-string-no-properties 7))
             (sol (match-string-no-properties 8))
             (mat (match-data))
             (lin) (ori))

        (setq y (+ y (cond ((and (= mv 12) (= m 1)) 1999)
                           ((and (= mv 1) (= m 12)) 2001)
                           (t                       2000))))

        (setq cre (clean-number-string cre))
        (setq deb (clean-number-string deb))
        ;; (insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
        (message "lib=%S" lab)
        (setq lab (pjb-bank-clean-description lab))
        (message "lab=%S" lab)
;;;        (setq ori (string-replace lab ".*\\*\\*\\*ORIGEN: *\\([0-9,.][0-9,.]*\\)-? \\(PTA\\|EUR\\).*" "\\1 \\2" t nil))
;;;        (setq lab (string-replace lab " *\\*\\*\\*ORIGEN:.*" "" t t))
;;;        (if (string-equal ori lab)
;;;            (setq ori "") ;; When there's no ORIGEN!
;;;          (setq oridev (substring ori -3))
;;;          (setq ori    (substring ori 0 -4))
;;;          (if (string-equal "PTA" oridev)
;;;              (setq ori (concat (string-replace
;;;                                 (clean-number-string ori)
;;;                                 "\\.00$" "" t t) " ESP"))
;;;            (setq ori (concat (clean-number-string ori) " EUR"))))
;;;
        (setq lin (pjb-bank-format-line  y m d cre deb lab))
        (setq credits (append credits (list cre)))
        (setq debits  (append debits  (list deb)))
        (store-match-data mat)
        (replace-match lin nil nil)))

    (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
    (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
    ));;from-cajamar-old

;; ------------------------------------------------------------------------

(defun from-evolvebank-xls-eur ()
  "Convert the lines following the point as from Evolve Bank (text)."
  (interactive)
  (let (credits debits)
    (while (re-search-forward
            (concat
             "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)    "
             "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)    "
             "\\([^     ]*\\)   "
             "\\([-0-9]?[0-9,.]*\\)     "
             "\\([-0-9]?[0-9,.]*\\)     "
             "\\([-0-9]?[0-9,.]*\\)\n"
             ) nil t)
      (let ((d   (match-string-no-properties 1))
            (m   (match-string-no-properties 2))
            (y   (match-string-no-properties 3))
            (lab (match-string-no-properties 7))
            (deb (match-string-no-properties 8))
            (cre (match-string-no-properties 9))
            (sol (match-string-no-properties 10))
            (mat (match-data))
            lin ori oridev)
        (setq cre (clean-number-string cre))
        (setq deb (clean-number-string deb))
;;(insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
        (setq lab (replace-regexp-in-string "^ " "" (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\\.\\.\\.*" "." (replace-regexp-in-string "\n" " " lab t t) t t) t t) t t))
        (setq ori (concat (if (string-equal "" cre) deb cre) " ESP"))
        (if (not (string-equal "" cre))
            (setq cre (format "%.2f" (string-to-number cre)
                              )))
        (if (not (string-equal "" deb))
            (setq deb (format "%.2f" (string-to-number deb)
                              )))
        (if (> (length lab) 24)
            (progn
              (setq lab (substring (string-justify-left lab 66 41) 41))
              (setq lin (format "20%2s-%2s-%2s %10s %10s         %-s%14s\n\n"
                            y m d cre deb lab ori)))

            (setq lin (format "20%2s-%2s-%2s %10s %10s         %-25s%14s\n\n"
                              y m d cre deb lab ori)))
        (setq credits (append credits (list cre)))
        (setq debits  (append debits  (list deb)))
        (store-match-data mat)
        (replace-match lin nil nil)))

    (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
    (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
    ));;from-evolvebank-xls-eur

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
        ));;while
  (printf "; crédits : %s\n" (pjb-string-amount-list credits))
  (printf "; débits  : %s\n" (pjb-string-amount-list debits))
    ));;from-patagon-table




;; ---------------------------------------------------------------------
(if nil (progn
;; old functions not used anymore.

(defun from-evolvebank-table ()
  "Convert the lines following the point as from Evolve Bank"
  (interactive)
  (let (credits debits)
    (while (re-search-forward
            (concat
             "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)|"
             "[/0-9]*|"
             "[^|]*|"
             "\\([^|]*\\)|"
             "\\([-0-9]?[0-9,.]*\\)\\( Pta.\\)?|"
             "\\([-0-9]?[0-9,.]*\\)\\( Pta.\\)?|"
             "\\([-0-9]?[0-9,.]*\\)\\( Pta.\\)?|"
             "\n"
             ) nil t)
      (let ((d   (match-string-no-properties 1))
            (m   (match-string-no-properties 2))
            (y   (match-string-no-properties 3))
            (lab (match-string-no-properties 4))
            (deb (match-string-no-properties 5))
            (cre (match-string-no-properties 7))
            (sol (match-string-no-properties 9))
            (mat (match-data))
            (lin) (ori))
        (setq cre (clean-number-string (replace-regexp-in-string "\\." "" cre t t)))
        (setq deb (clean-number-string (replace-regexp-in-string "\\." "" deb t t)))
;;(insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
        (setq lab (replace-regexp-in-string "^ " "" (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\\.\\.\\.*" "." (replace-regexp-in-string "\n" " " lab t t) t t) t t) t t))
        (setq ori (concat (if (string-equal "" cre) deb cre) " ESP"))
        (if (not (string-equal "" cre))
            (setq cre (format "%.2f"
                              (euro-from-value (string-to-number cre) 'ESP))))
        (if (not (string-equal "" deb))
            (setq deb (format "%.2f"
                              (euro-from-value (string-to-number deb) 'ESP))))
        (if (> (length lab) 24)
            (progn
              (setq lab (substring (string-justify-left lab 66 41) 41))
              (setq lin (format "20%2s-%2s-%2s %10s %10s         %-s%14s\n\n"
                            y m d cre deb lab ori)))

            (setq lin (format "20%2s-%2s-%2s %10s %10s         %-25s%14s\n\n"
                              y m d cre deb lab ori)))
        (setq credits (append credits (list cre)))
        (setq debits  (append debits  (list deb)))
        (store-match-data mat)
        (replace-match lin nil nil)))

    (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
    (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
    ));;from-evolvebank-table

(defun from-evolvebank-xls-esp ()
  "Convert the lines following the point as from Evolve Bank (text)."
  (interactive)
  (let (credits debits)
    (while (re-search-forward
            (concat
             "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)    "
             "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)    "
             "\\([^     ]*\\)   "
             "\\([-0-9]?[0-9,.]*\\)     "
             "\\([-0-9]?[0-9,.]*\\)     "
             "\\([-0-9]?[0-9,.]*\\)\n"
             ) nil t)
      (let ((d   (match-string-no-properties 1))
            (m   (match-string-no-properties 2))
            (y   (match-string-no-properties 3))
            (lab (match-string-no-properties 7))
            (deb (match-string-no-properties 8))
            (cre (match-string-no-properties 9))
            (sol (match-string-no-properties 10))
            (mat (match-data))
            (lin) (ori))
        (setq cre (clean-number-string (replace-regexp-in-string "\\." "" cre t t)))
        (setq deb (clean-number-string (replace-regexp-in-string "\\." "" deb t t)))
;;(insert (format "d=%S\nm=%S\ny=%S\nlab=%S\ndeb=%S\ncre=%S\nsol=%S\n" d m y lab deb cre sol))
        (setq lab (replace-regexp-in-string "^ " "" (replace-regexp-in-string "  *" " " (replace-regexp-in-string "\\.\\.\\.*" "." (replace-regexp-in-string "\n" " " lab t t) t t) t t) t t))
        (setq ori (concat (if (string-equal "" cre) deb cre) " ESP"))
        (if (not (string-equal "" cre))
            (setq cre (format "%.2f"
                              (euro-from-value (string-to-number cre) 'ESP))))
        (if (not (string-equal "" deb))
            (setq deb (format "%.2f"
                              (euro-from-value (string-to-number deb) 'ESP))))
        (if (> (length lab) 24)
            (progn
              (setq lab (substring (string-justify-left lab 66 41) 41))
              (setq lin (format "20%2s-%2s-%2s %10s %10s         %-s%14s\n\n"
                            y m d cre deb lab ori)))

            (setq lin (format "20%2s-%2s-%2s %10s %10s         %-25s%14s\n\n"
                              y m d cre deb lab ori)))
        (setq credits (append credits (list cre)))
        (setq debits  (append debits  (list deb)))
        (store-match-data mat)
        (replace-match lin nil nil)))

    (insert (format "; crédits : %s\n" (pjb-string-amount-list credits)))
    (insert (format "; débits  : %s\n" (pjb-string-amount-list debits)))
    ));;from-evolvebank-xls-esp


(defun from-openbank ()
  "Convert the lines following the point as from OpenBank"
  (interactive)

  (while
      (re-search-forward
       (concat
        " *\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9][0-9][0-9]\\) *\n"
        " *\\([0-9][0-9]\\)-\\([0-9][0-9]\\) *\n"
        "\\(\\( *[^ ].*\n\\)*\\)"
        " *\\([-,.0-9][,.0-9]*\\) *\n"
        " *\\([-,.0-9][-,.0-9]*\\) *\n") nil t)
      (let ((d   (match-string-no-properties 1))
            (m   (match-string-no-properties 2))
            (y   (match-string-no-properties 3))
            (dv  (match-string-no-properties 4))
            (mv  (match-string-no-properties 5))
            (lab (match-string-no-properties 6))
            (imp (match-string-no-properties 8))
            (sol (match-string-no-properties 9))
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
        (setq lin (format "%4s-%2s-%2s %10s %10s         %s\n"
                        y m d cre deb lab))
        (store-match-data mat)
        (replace-match lin nil nil)))
  );;from-openbank

(defun from-paritate ()
  "Convert the lines following the point as from Paritate Bank"
  (interactive)

  (while
      (re-search-forward
"^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)   \\(................................\\)\\* *\\([0-9][0-9.,]*[CD]\\) *\n\\(\\( [A-Z].*\n\\)*\\) *\\([0-9][0-9]*\\) *\\(#[0-9][0-9]* [0-9][0-9]* [0-9][0-9]*\\) *\\([0-9]*\\) *\\* *\\([0-9][0-9.,]*\\) *\n" nil t)
      (let ((m   (match-string-no-properties 1))
            (d   (match-string-no-properties 2))
            (y   (match-string-no-properties 3))
            (lab (match-string-no-properties 4))
            (inc (match-string-no-properties 5))
            (bal (match-string-no-properties 6))
            (ref (match-string-no-properties 8))
            (cnt (match-string-no-properties 9))
            (doc (match-string-no-properties 10))
            (sol (match-string-no-properties 11))
            (mat (match-data))
            (cre "")
            (deb "")
            (lin))

        (if (= 67 (car (last (string-to-list inc))))
            (setq cre (substring inc 0 (- (length inc) 1)))
            (setq deb (substring inc 0 (- (length inc) 1))))
        (setq cre (clean-number-string cre))
        (setq deb (clean-number-string deb))
        (setq lab (replace-regexp-in-string " *$" "" lab t t))
        (setq lab (substring (string-justify-left lab 66 41) 41))
        (setq bal (replace-regexp-in-string " *$" "" bal t t))
        (if (STRING= "" bal)
            (setq lin (format "20%2s-%2s-%2s %10s %10s %s\n"
                              y m d cre deb lab))
            (setq lin (format "20%2s-%2s-%2s %10s %10s %s\n%s\n"
                              y m d cre deb lab bal))
            )
        (store-match-data mat)
        (replace-match lin nil nil)))
  );;from-paritate

))
;; ---------------------------------------------------------------------
;;;; pjb-banks-old.el                 --                     --          ;;;;
