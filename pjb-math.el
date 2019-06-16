;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-math.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines bindings for maths symols.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-12-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************


(setf lexical-binding t)
(defun set-math-bindings (prefix)
  (dolist (item '(
                  "`~∞∝" "1!₁¹" "2@₂²" "3#₃³" "4$₄⁴" "5%₅⁵" "6^₆⁶" "7&₇⁷" "8*₈⁸" "9(₉⁹" "0)₀⁰" "-_⊣⊥" "=+⊢⊤"
;; Math chars:
;; ℂℍℕℙℚℝℤℼℽℾℿ⅀ⅅⅆⅇⅈⅉ
;; ℬℰℱℋℐℒℳ℘ℛ

                  "qQωΩ" "wW€¢" "eEεΕ" "rRρΡ" "tTτΤ" "yYψΨ" "uUυΥ" "iIιΙ" "oOοΟ" "pPπΠ" "[{«∀" "]}»∃"  "\\|\∖∫"
                  "aAαΑ" "sSσΣ" "dDδΔ" "fFφΦ" "gGγΓ" "hHθΘ" "jJηΗ" "kKκΚ" "lLλΛ" ";:⊆⎕" "'\"⊇○"
                  "zZζΖ" "xXξΞ" "cCχΧ" "vV∇√" "bBβΒ" "nNνΝ" "mMμΜ" ",<≤∧" ".>≥∨" "/?≠¬"
                  (pause  "∂")
                  (insert "∈∉")   (home "⇒⇔")    (prior "≡≢")
                  (delete "∅≣")   (end "∴∵")     (next "≈≇")
                  (up "↑∩")
                  (left "←⊂")    (down "↓∪")    (right "→⊃")))
    (cond
      ((stringp item)
       (let ((l (aref item 0)) (lm (string (aref item 2)))
             (s (aref item 1)) (sm (string (aref item 3))))
         (local-set-key (kbd (format "%s %c" prefix l))
                        (lambda (n)
                          (interactive "p")
                          (dotimes (i n) (insert lm))))
         (local-set-key (kbd (format "%s %c" prefix s))
                        (lambda (n)
                          (interactive "p")
                          (dotimes (i n) (insert sm))))))
      ((listp item)
       (let ((y (elt item 0))
             (lm (string (aref (elt item 1) 0)))
             (sm (when (< 1 (length (elt item 1)))
                   (string (aref (elt item 1) 1)))))
         (local-set-key (kbd (format "%s <%s>" prefix y))
                        (lambda (n)
                          (interactive "p")
                          (dotimes (i n) (insert lm))))
         (when sm
           (local-set-key (kbd (format "%s S-<%s>" prefix y))
                          (lambda (n)
                            (interactive "p")
                            (dotimes (i n) (insert sm))))))))))

(defun set-greek-bindings (prefix)
  (dolist (item '(
                  "qQωΩ" "wW€¢" "eEεΕ" "rRρΡ" "tTτΤ" "yYψΨ" "uUυΥ" "iIιΙ" "oOοΟ" "pPπΠ"
                  "aAαΑ" "sSσΣ" "dDδΔ" "fFφΦ" "gGγΓ" "hHθΘ" "jJηΗ" "kKκΚ" "lLλΛ"
                  "zZζΖ" "xXξΞ" "cCχΧ" "vV∇√" "bBβΒ" "nNνΝ" "mMμΜ"))
    (let ((l (aref item 0)) (lm (string (aref item 2)))
          (s (aref item 1)) (sm (string (aref item 3))))
      (local-set-key (kbd (format "%s %c" prefix l))
                     (lambda (n)
                       (interactive "p")
                       (dotimes (i n) (insert lm))))
      (local-set-key (kbd (format "%s %c" prefix s))
                     (lambda (n)
                       (interactive "p")
                       (dotimes (i n) (insert sm)))))))


(set-greek-bindings "C-c g")
(set-math-bindings "C-c m")

(provide 'pjb-math)
