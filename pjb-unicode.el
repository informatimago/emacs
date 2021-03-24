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

(defparameter *pjb-unicode-maps*
  '((mathematical-monospace ((?0 ?9 #x1d7c6)
                             (?A ?Z #x1d62f)
                             (?a ?z #x1d629)))))



(defun pjb-unicode-map-character-ranges (string ranges)
  (map 'string (lambda (ch)
                 (let ((range (find-if (lambda (range)  (<= (first range) ch (second range)))
                                       ranges)))
                   (if range
                       (+ ch (third range))
                       ch)))
       string))

(defun pjb-unicode-get-map (map-name)
  (second (assoc map-name *pjb-unicode-maps*)))

(defun pjb-unicode-map-region (start end map-name)
  (let* ((text (buffer-substring start end))
         (new  (pjb-unicode-map-character-ranges text (pjb-unicode-get-map map-name))))
    (delete-region start end)
    (insert new)))

(defun pjb-unicode-set-keys (map-name)
  (loop for (min max offset) in (pjb-unicode-get-map map-name)
        do (loop for ch from min to max
                 do (local-set-key (kbd (string ch)) (string (+ ch offset))))))


(defun pjb-unicode-ascii ()
  (interactive)
  (loop for ch from 32 to 126
        do (local-set-key (kbd (string ch)) nil)))


(defun pjb-unicode-small-caps ()
  (interactive)
  (loop for ch from ?0 to ?9
        for mf from 120802
        do  (local-set-key (kbd (string ch))        (string mf)))
  (loop for ch from ?a to ?z
        for mf across "ᴀʙᴄᴅᴇꜰɢʜɪᴊᴋʟᴍɴᴏᴘꞯʀꜱᴛᴜᴠᴡxʏᴢ"
        do (local-set-key (kbd (string ch))          (string mf)))
  (loop for ch from ?A to ?Z
        for mf across "ᴀʙᴄᴅᴇꜰɢʜɪᴊᴋʟᴍɴᴏᴘꞯʀꜱᴛᴜᴠᴡxʏᴢ"
        do (local-set-key (kbd (string ch))          (string mf))))


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
           (local-set-key (kbd (string (+ #x20 ch))) (string (+ 26 mf))))
  (local-set-key (kbd "e") (string #x212f))
  (local-set-key (kbd "g") (string #x210A))
  (local-set-key (kbd "o") (string #x2134)))

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
  (pjb-unicode-set-keys 'mathematical-monospace))

(defun pjb-unicode-mathematical-monospace-region (start end)
  (interactive "r")
  (pjb-unicode-map-region start end 'mathematical-monospace))

(provide 'pjb-unicode)
(local-set-key (kbd "A-(")
               (lambda (rep)
                 (interactive "p")
                 (insert (make-string rep ?⸨))))

(local-set-key (kbd "A-)")
               (lambda (rep)
                 (interactive "p")
                 (insert (make-string rep ?⸩))))



(defparameter *braille*
  '((capital "⠠")
    (number "⠼")
    (bracket "⠶")

    ("1" "⠼⠁")
    ("2" "⠼⠂")
    ("3" "⠼⠃")
    ("4" "⠼⠄")
    ("5" "⠼⠅")
    ("6" "⠼⠆")
    ("7" "⠼⠇")
    ("8" "⠼⠈")
    ("9" "⠼⠉")
    ("0" "⠼⠊")

    ("a" "⠁")
    ("b" "⠂")
    ("c" "⠃")
    ("d" "⠄")
    ("e" "⠅")
    ("f" "⠆")
    ("g" "⠇")
    ("h" "⠈")
    ("i" "⠉")
    ("j" "⠊")
    ("k" "⠋")
    ("l" "⠌")
    ("m" "⠍")
    ("n" "⠎")
    ("o" "⠏")
    ("p" "⠐")
    ("q" "⠑")
    ("r" "⠒")
    ("s" "⠓")
    ("t" "⠔")
    ("u" "⠥")
    ("v" "⠧")
    ("w" "⠺")
    ("x" "⠭")
    ("y" "⠽")
    ("z" "⠵")
    ("ç" "⠯")
    ("é" "⠿")
    ("à" "⠷")
    ("è" "⠮")
    ("ù" "⠾")
    ("â" "⠡")
    ("ê" "⠣")
    ("î" "⠩")
    ("ô" "⠹")
    ("û" "⠱")
    ("ë" "⠫")
    ("ï" "⠻")
    ("ü" "⠳")
    ("ö" "⠪")
    ("ì" "⠌")
    ("ä" "⠜")
    ("ò" "⠬")

    ("," "⠂")
    (";" "⠆")
    ("'" "⠄")
    (":" "⠒")
    ("-" "⠤")
    ("." "⠨")
    ("." "⠲")
    ("!" "⠖")
    ("?" "⠦")
    ("`" "⠦")
    ("‘" "⠦")
    ("’" "⠴")
    ("/" "⠌")
    ("(" "⠶")
    (")" "⠶")

    ("A" "⠠⠁")
    ("B" "⠠⠂")
    ("C" "⠠⠃")
    ("D" "⠠⠄")
    ("E" "⠠⠅")
    ("F" "⠠⠆")
    ("G" "⠠⠇")
    ("H" "⠠⠈")
    ("I" "⠠⠉")
    ("J" "⠠⠊")
    ("K" "⠠⠋")
    ("L" "⠠⠌")
    ("M" "⠠⠍")
    ("N" "⠠⠎")
    ("O" "⠠⠏")
    ("P" "⠠⠐")
    ("Q" "⠠⠑")
    ("R" "⠠⠒")
    ("S" "⠠⠓")
    ("T" "⠠⠔")
    ("U" "⠠⠥")
    ("V" "⠠⠧")
    ("W" "⠠⠺")
    ("X" "⠠⠭")
    ("Y" "⠠⠽")
    ("Z" "⠠⠵")
    ("Ç" "⠠⠯")
    ("É" "⠠⠿")
    ("À" "⠠⠷")
    ("È" "⠠⠮")
    ("Ù" "⠠⠾")
    ("Â" "⠠⠡")
    ("Ê" "⠠⠣")
    ("Î" "⠠⠩")
    ("Ô" "⠠⠹")
    ("Û" "⠠⠱")
    ("Ë" "⠠⠫")
    ("Ï" "⠠⠻")
    ("Ü" "⠠⠳")
    ("Ö" "⠠⠪")
    ("Ì" "⠠⠌")
    ("Ä" "⠠⠜")
    ("Ò" "⠠⠬")
    ))

(defun pjb-unicode-braille ()
  (interactive)
  (loop for (key braille) in *braille*
        when (stringp key)
          do (local-set-key (kbd key) braille)))

(defun ensure-list (x) (if (listp x) x (list x)))

(defun pjb-unicode-clock (clocks)
  "Return a string containing clock emojis from `clocks`
which is a designator for a list of numbers representing hours,
or hours and a half."
  (let ((clocks (ensure-list clocks)))
    (map 'string (lambda (clock)
                   (let ((hour (mod (truncate clock) 12))
                         (demi (round (- clock (truncate clock))
                                      0.5)))
                     (+ (if (zerop demi)
                            #x1f550
                            (+ #x1f550 12))
                        (if (zerop hour)
                            12
                            hour)
                        -1)))
         clocks)))

(defun pjb-unicode-clock-number (n)
  (pjb-unicode-clock (map 'list (lambda (digit)  (- digit ?0))
                          (format "%d" n))))

;; (pjb-unicode-clock-number 6.693648696890451, 149.30317099394867)
;; (pjb-unicode-clock-number 00641371491811)
;; "🕕🕓🕐🕒🕖🕐🕓🕘🕐🕗🕐🕐"
