;;;;******************************************************************************
;;;;FILE:               pjb-selftrade.el
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
(require 'pjb-strings)
(provide 'pjb-selftrade)


(defun find-matching-pattern (string pattern-list)
  "RETURN: The index of the first pattern that matches STRING, 
        or nil if none matches."
  (let ((n -1)
        pattern
        (found nil))
    (while pattern-list
      (setq n (1+ n)
           pattern (car pattern-list)
           pattern-list (cdr pattern-list))
      (if (string-match pattern string)
          (setq pattern-list nil
                found t)))
    (if found n nil)))


(if nil
    (let ((n 0))
      (mapc (lambda (x)
                (insert (format "((= p %d)\n(setq %-12s (fsce-append %-12s (chop-spaces line))))\n" n x x))
                (setq n (1+ n)))
              '(code-valeur emetteur parite style strike echeance delta nombre))))

(defun fsce-append (a b)
  (if a
      (concat a " " b)
    b))

(defun from-selftrade-cash-extraction ()
  (interactive)
  (let ((patterns '(
"^                                                                    .....*$"
"^                                                            ....*$"
"^                                                 ..........*$"
"^                                         .....*$"
"^                             ........*$"
"^                       .....*$"
"^       .........*$"
"^......*$"
))
        code-valeur emetteur parite style strike echeance delta nombre
        line beg end p
        )

    (while (not (= (point) (point-max)))
      (if (= (line-beginning-position) (line-end-position))
          (progn
            (insert (format "%s|%s|%s|%s|%s|%s|%s|%s\n"
                            code-valeur emetteur parite style 
                            strike echeance delta nombre))
            (setq code-valeur nil
                  emetteur nil 
                  parite nil
                  style nil
                  strike nil
                  echeance nil
                  delta nil
                  nombre nil)
            )

        (setq line (buffer-substring (line-beginning-position)
                                     (line-end-position)))
        (setq p (find-matching-pattern line patterns))
        (cond
         ((= p 7)
          (setq code-valeur  (fsce-append code-valeur  (chop-spaces line))))
         ((= p 6)
          (setq emetteur     (fsce-append emetteur     (chop-spaces line))))
         ((= p 5)
          (setq parite       (fsce-append parite       (chop-spaces line))))
         ((= p 4)
          (setq style        (fsce-append style        (chop-spaces line))))
         ((= p 3)
          (setq strike       (fsce-append strike       (chop-spaces line))))
         ((= p 2)
          (setq echeance     (fsce-append echeance     (chop-spaces line))))
         ((= p 1)
          (setq delta        (fsce-append delta        (chop-spaces line))))
         ((= p 0)
          (setq nombre       (fsce-append nombre       (chop-spaces line))))
         )
        
        )
      (beginning-of-line)
      (kill-line 1))
))

;;;; pjb-selftrade.el                 -- 2002-02-20 04:06:02 -- pascal   ;;;;
