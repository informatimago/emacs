;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-ansi-color.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Colorize a buffer containing ansi-color escape sequences.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-07-28 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(require 'pjb-cl)


;; http://www.strasis.com/documentation/limelight-xe/reference/ecma-48-sgr-codes

(defparameter *pjb-ansi-color-to-face*

  '(((normal)                 0    :weight :slant :underline :box :inverse-video :strike-through
                                   :foreground :background)

    ((bold-on)                1    (:weight            bold))
    ((bold-off)               2    :weight)
    ((italic-on)              3    (:slant             italic))
    ((underline-on)           4    (:underline         t))
    ((blink-on)               5    (:box               t))
    ((invert-on)              7    (:inverse-video     t))
    ((strike-on)              9    (:strike-through    t))

    ((bold-off)               21   :weight)
    ((italic-off)             23   :slant)
    ((underline-off)          24   :underline)
    ((blink-off)              25   :box)
    ((invert-off)             27   :inverse-video)
    ((strike-off)             29   :strike-throught)

    ((black-fore)             30   (:foreground        "#000000"))
    ((red-fore)               31   (:foreground        "#800000"))
    ((green-fore)             32   (:foreground        "#008000"))
    ((yellow-fore)            33   (:foreground        "#808000"))
    ((blue-fore)              34   (:foreground        "#000080"))
    ((magenta-fore)           35   (:foreground        "#800080"))
    ((cyan-fore)              36   (:foreground        "#008080"))
    ((white-fore)             37   (:foreground        "#c0c0c0"))

    ((black-back)             40   (:background        "#000000"))
    ((red-back)               41   (:background        "#800000"))
    ((green-back)             42   (:background        "#008000"))
    ((yellow-back)            43   (:background        "#808000"))
    ((blue-back)              44   (:background        "#000080"))
    ((magenta-back)           45   (:background        "#800080"))
    ((cyan-back)              46   (:background        "#008080"))
    ((white-back)             47   (:background        "#c0c0c0"))

    ((bright-black-fore)      90   (:foreground        "#808080"))
    ((bright-red-fore)        91   (:foreground        "#ff0000"))
    ((bright-green-fore)      92   (:foreground        "#00ff00"))
    ((bright-yellow-fore)     93   (:foreground        "#ffff00"))
    ((bright-blue-fore)       94   (:foreground        "#0000ff"))
    ((bright-magenta-fore)    95   (:foreground        "#ff00ff"))
    ((bright-cyan-fore)       96   (:foreground        "#00ffff"))
    ((bright-white-fore)      97   (:foreground        "#ffffff"))

    ((bright-black-back)      100  (:background        "#808080"))
    ((bright-red-back)        101  (:background        "#ff0000"))
    ((bright-green-back)      102  (:background        "#00ff00"))
    ((bright-yellow-back)     103  (:background        "#ffff00"))
    ((bright-blue-back)       104  (:background        "#0000ff"))
    ((bright-magenta-back)    105  (:background        "#ff00ff"))
    ((bright-cyan-back)       106  (:background        "#00ffff"))
    ((bright-white-back)      107  (:background        "#ffffff"))

    )
  "Maps CSI m codes to face properties.
A keyword alone would be removed from the current face properties,
A p-list would be added (replacing values) of the current face properties.")

(defparameter *pjb-ansi-properties* '(face)
  "A list of all the text properties used.")

(defun pjb-ansi-color-codes-to-face (face color-codes)
  "Return a face property list updated from `face' with the ANSI `color-codes'."
  (dolist (code (split-string color-codes ";" t) face)
    (let ((code (let ((value (car (read-from-string code))))
                  (if (integerp value)
                      value
                      (warn "Invalid color-code in escape sequence: %S -- ignored"
                            color-codes)))))
      (let ((entry (find code *pjb-ansi-color-to-face*
                         :key (function second))))
        (if entry
            (dolist (item (cddr entry))
              (if (atom item)
                  (remf face item)
                  (setf (getf face (first item)) (second item))))
            (warn "Unsupported ansi-code %S -- ignored" code))))))

(defun test/pjb-ansi-color-codes-to-face ()
  (assert (equal (pjb-ansi-color-codes-to-face '() "1")
                 '(:weight bold)))
  (assert (equal (pjb-ansi-color-codes-to-face '(:weight bold) "3;4")
                 '(:underline t :slant italic :weight bold)))
  (assert (equal (pjb-ansi-color-codes-to-face '(:weight bold) "2;3;4")
                 '(:underline t :slant italic)))
  (assert (equal (pjb-ansi-color-codes-to-face '(:underline t :slant italic :weight bold)
                                                     "21;23")
                 '(:underline t)))
  (assert (equal (pjb-ansi-color-codes-to-face '(:underline t :slant italic :weight bold)
                                                     "21;23;24")
                 nil))
  :success)

(test/pjb-ansi-color-codes-to-face)

(defun pjb-ansi-colorize-region (start end)
    "Interpret the CSI m ANSI sequences (7-bit) in the region.
All other CSI ANSi sequences are ignored and hidden."
  (interactive "r")
  (goto-char start)
  (let ((current-face '()))
    (while (re-search-forward "\\[\\([0-9;]*\\)\\([@-~]\\)" (point-max) t)
           (if (string= "m" (match-string 2))
               (let ((new-color-codes (match-string 1))
                     (old-end   (match-beginning 0))
                     (new-start (match-end 0)))
                 (when current-face
                   (set-text-properties start old-end (list 'face current-face)))
                 (compose-region old-end new-start "")
                 (setf current-face (pjb-ansi-color-codes-to-face
                                           current-face new-color-codes)
                       start new-start))
               ;; We ignore all other ANSI CSI sequences, and just hide them.
               (compose-region (match-beginning 0) (match-end 0) "")))
    (when current-face
      (set-text-properties start end (list 'face current-face)))))

(defun pjb-ansi-decolorize-region (start end)
  "Remove all face properties from the region, and decompose the CSI sequences."
  (interactive "r")
  (while (re-search-forward "\\[\\([0-9;]*\\)\\([@-~]\\)" (point-max) t)
         (decompose-region old-end new-start))
  (remove-text-properties start end *pjb-ansi-properties*))

(defun pjb-ansi-colorize-buffer ()
  "Interpret the CSI m ANSI sequences (7-bit) in the current buffer.
All other CSI ANSI sequences are ignored and hidden."
  (interactive)
  (pjb-ansi-colorize-region (point-min) (point-max)))

(defun pjb-ansi-decolorize-buffer ()
  "Remove all face properties from the region, decompose the CSI sequences, and refontify the buffer."
  (interactive)
  (pjb-ansi-decolorize-region (point-min) (point-max))
  (font-lock-fontify-buffer))

(provide 'pjb-ansi-color)
