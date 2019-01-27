;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-unicode.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements functions binding locally keys to unicode alphabets.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-01-25 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
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
(require 'cl)

(defun pjb-unicode-ascii ()
  (interactive)
  (loop for ch from ?0 to ?9
        do (local-set-key (kbd (string ch)) nil))
  (loop for ch from ?A to ?Z
        do (local-set-key (kbd (string ch)) nil)
           (local-set-key (kbd (string (+ #x20 ch))) nil)))


(defun pjb-unicode-parenthesized-latin ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from #x1d7f6
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?a to ?z
        for mf from 9372
        do (local-set-key (kbd (string ch))          (string mf))))


(defun pjb-unicode-circled-latin ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from 9450
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from 9398
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))


(defun pjb-unicode-fullwidth-latin ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from 65296
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from 65313
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 32 mf)))))

(defun pjb-unicode-mathematical-bold ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from #x1d7f6
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from 119808
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-bold-italic ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from #x1d7f6
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from 119912
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-script ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from #x1d7f6
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from 119964
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-script-bold ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from 120782
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from 120016
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-double-struck ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from 120792
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from #x1d538
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf))))
  (local-set-key (kbd "C") "ℂ")
  (local-set-key (kbd "H") "ℍ")
  (local-set-key (kbd "N") "ℕ")
  (local-set-key (kbd "P") "ℙ")
  (local-set-key (kbd "Q") "ℚ")
  (local-set-key (kbd "R") "ℝ")
  (local-set-key (kbd "Z") "ℤ"))

(defun pjb-unicode-mathematical-fraktur ()
  (interactive)
  (loop for ch from ?1 to ?9
        for mf from #x02170
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from #x1d504
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf))))
  (local-set-key (kbd "C") "ℭ")
  (local-set-key (kbd "H") "ℌ")
  (local-set-key (kbd "I") "ℑ")
  (local-set-key (kbd "R") "ℜ")
  (local-set-key (kbd "Z") "ℨ"))

(defun pjb-unicode-mathematical-fraktur-bold ()
  (interactive)
  (loop for ch from ?1 to ?9
        for mf from #x02160
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from #x1d56c
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-sans-serif ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from 120802
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from #x1d5A0
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-sans-serif-bold ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from 120812
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from #x1d5D4
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-sans-serif-italic ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from #x1d7ce
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from #x1d608
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-sans-serif-bold-italic ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from #x1d7ce
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from 120380
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(defun pjb-unicode-mathematical-monospace ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from #x1d7f6
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?A to ?Z
        for mf from #x1d670
        do (local-set-key (kbd (string ch))          (string mf))
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf)))))

(provide 'pjb-unicode)
(local-set-key (kbd "A-(")
               (lambda (rep)
                 (interactive "p")
                 (insert (make-string rep ?⸨))))

(local-set-key (kbd "A-)")
               (lambda (rep)
                 (interactive "p")
                 (insert (make-string rep ?⸩))))

