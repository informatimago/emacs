;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-color.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Setting RGB colors with sliders.
;;;;    see: (pjb-color-picker--example)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-02-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
(setf lexical-binding t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some color utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun* color-mix     (color-a color-b &optional (factor 0.5) (gamma 2.0))
  "
FACTOR   a value in [0.0 1.0], default is 0.5.
GAMMA    the gamma value (usually, between 1.8 and 2.2; default 2.0).
RETURN:  A triplet (red green blue)
         = color-a + ( color-b - color-a ) * factor
"
  ;; averaging colors should be performed taking into account the fact
  ;; that RGB values are gamma-roots.
  (when (or (<= factor 0.0) (<= 1.0 factor))
    (error "Factor %f is out of range [0.0,1.0]." factor))
  (mapcar* (lambda (a b)
             (let ((a^g (expt a gamma))
                   (b^g (expt b gamma)))
               (expt (truncate (+ a^g (* (- b^g a^g) factor)))
                     (/ gamma))))
           color-a color-b))


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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; color picker
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pjb-color-picker--default-color)
(defvar pjb-color-picker--current-color)
(defvar pjb-color-picker--current-channel)
(defvar pjb-color-picker--width)
(defvar pjb-color-picker--update-color)
(defvar pjb-color-picker--quit)
(defvar pjb-color-picker--abort)

(defun pjb-color-picker--current-column (width level)
  (truncate (* width level)))

(defun pjb-color-picker--decode-color (color)
  (let ((rgb (color-name-to-rgb color)))
    (values-list (append rgb (list rgb)))))

(defun pjb-color-picker--rgb-red (rgb) (first rgb))
(defun pjb-color-picker--rgb-green (rgb) (second rgb))
(defun pjb-color-picker--rgb-blue (rgb) (third rgb))

(defun pjb-color-picker--clip (min max value)
  (cond
    ((< max value) max)
    ((< value min) min)
    (t value)))

(defun pjb-color-picker--red-color (intensity)
  (format "#%04x00000000"
          (pjb-color-picker--clip 0 65535 (truncate (* 65536 intensity)))))
(defun pjb-color-picker--green-color (intensity)
  (format "#0000%04x0000"
          (pjb-color-picker--clip 0 65535 (truncate (* 65536 intensity)))))
(defun pjb-color-picker--blue-color (intensity)
  (format "#00000000%04x"
          (pjb-color-picker--clip 0 65535 (truncate (* 65536 intensity)))))
(defun pjb-color-picker--rgb-color (rgb)
  (format "#%04x%04x%04x"
          (pjb-color-picker--clip 0 65535 (truncate (* 65536 (pjb-color-picker--rgb-red   rgb))))
          (pjb-color-picker--clip 0 65535 (truncate (* 65536 (pjb-color-picker--rgb-green rgb))))
          (pjb-color-picker--clip 0 65535 (truncate (* 65536 (pjb-color-picker--rgb-blue  rgb))))))

(defun pjb-color-picker--beginning-of-line (n)
  (save-excursion
   (goto-line n)
   (beginning-of-line)
   (point)))

(defun pjb-color-picker--end-of-line (n)
  (save-excursion
   (goto-line n)
   (end-of-line)
   (point)))

(defun pjb-color-picker--update-color ()
  ;; (message "%S" (list pjb-color-picker--default-color
  ;;                     pjb-color-picker--current-color
  ;;                     pjb-color-picker--current-channel
  ;;                     pjb-color-picker--width
  ;;                     pjb-color-picker--update-color
  ;;                     pjb-color-picker--quit
  ;;                     pjb-color-picker--abort))
  (let ((rgb pjb-color-picker--current-color))
    (multiple-value-bind (red green blue) rgb
      (add-text-properties (pjb-color-picker--beginning-of-line 1) (pjb-color-picker--end-of-line 1)
                           `(face (background-color . ,(pjb-color-picker--red-color   red))))
      (add-text-properties (pjb-color-picker--beginning-of-line 2) (pjb-color-picker--end-of-line 2)
                           `(face (background-color . ,(pjb-color-picker--green-color green))))
      (add-text-properties (pjb-color-picker--beginning-of-line 3) (pjb-color-picker--end-of-line 3)
                           `(face (background-color . ,(pjb-color-picker--blue-color  blue))))
      (add-text-properties (pjb-color-picker--beginning-of-line 4) (pjb-color-picker--end-of-line 4)
                           `(face (background-color . ,(pjb-color-picker--rgb-color   rgb))))
      (goto-line pjb-color-picker--current-channel)
      (beginning-of-line)
      (forward-char (pjb-color-picker--current-column
                     pjb-color-picker--width
                     (ecase pjb-color-picker--current-channel
                       ((1) red)
                       ((2) green)
                       ((3) blue))))
      (when pjb-color-picker--update-color
        (funcall pjb-color-picker--update-color pjb-color-picker--current-color)))))


(defun pjb-color-picker--mix-toward (target)
  (let ((step 0.02)
        (target-color (copy-list pjb-color-picker--current-color)))
    (setf (nth (1- pjb-color-picker--current-channel) target-color)
          (pjb-color-picker--clip 0.0 1.0
                                  (+ (nth (1- pjb-color-picker--current-channel) target-color)
                                     (if (plusp target) step (- step))))
          pjb-color-picker--current-color target-color)
    (pjb-color-picker--update-color)))

(defun pjb-color-picker--decrease-intensity ()
  (interactive)
  (pjb-color-picker--mix-toward 0.0))

(defun pjb-color-picker--increase-intensity ()
  (interactive)
  (pjb-color-picker--mix-toward 1.0))

(defun pjb-color-picker--previous-channel ()
  (interactive)
  (setf pjb-color-picker--current-channel (pjb-color-picker--clip 1 3 (1- pjb-color-picker--current-channel)))
  (pjb-color-picker--update-color))

(defun pjb-color-picker--next-channel ()
  (interactive)
  (setf pjb-color-picker--current-channel (pjb-color-picker--clip 1 3 (1+ pjb-color-picker--current-channel)))
  (pjb-color-picker--update-color))

(defun pjb-color-picker--quit ()
  (interactive)
  (when (or (null pjb-color-picker--quit)
            (funcall pjb-color-picker--quit pjb-color-picker--current-color))
    (switch-to-buffer nil)))

(defun pjb-color-picker--abort ()
  (interactive)
  (setf pjb-color-picker--current-color pjb-color-picker--default-color)
  (pjb-color-picker--update-color)
  (when (or (null pjb-color-picker--abort)
            (funcall pjb-color-picker--abort pjb-color-picker--current-color))
    (switch-to-buffer nil)))

(defun pjb-color-picker--initialize-keymap ()
  (let ((e (make-sparse-keymap)))
    (flet ((keys (command &rest keys)
             (dolist (k keys)
               (define-key e (read-kbd-macro k) command))))
      (keys 'pjb-color-picker--previous-channel         "<up>"    "C-p" "p")
      (keys 'pjb-color-picker--next-channel             "<down>"  "C-n" "n")
      (keys 'pjb-color-picker--decrease-intensity       "<left>"  "C-b" "b" "-" "-")
      (keys 'pjb-color-picker--increase-intensity       "<right>" "C-f" "f" "+" "=")
      (keys 'pjb-color-picker--quit                     "<RET>"   "q")
      (keys 'pjb-color-picker--abort                    "C-g"))
    (use-local-map e)))


(defun pjb-color-picker--initialize-buffer (default-rgb update-color quit abort)
  (mapc (function make-variable-buffer-local)
        '(pjb-color-picker--default-color
          pjb-color-picker--current-color
          pjb-color-picker--current-channel
          pjb-color-picker--width
          pjb-color-picker--update-color
          pjb-color-picker--quit
          pjb-color-picker--abort))
  (let* ((space 32)
         (width 50)
         (line (make-string width space)))
    (erase-buffer)
    (insert line "\n" line "\n" line "\n" line)
    (setf pjb-color-picker--default-color    default-rgb
          pjb-color-picker--current-color    default-rgb
          pjb-color-picker--update-color     update-color
          pjb-color-picker--quit             quit
          pjb-color-picker--abort            abort
          pjb-color-picker--width            width
          pjb-color-picker--current-channel  1)
    (pjb-color-picker--initialize-keymap)
    (pjb-color-picker--update-color)))


(defun* pjb-color-picker--create-color-picker (title default-rgb &key update quit abort)
  "
Create a buffer named `title' with three lines used to show the rgb
components of the color, and another showing the resulting color.

The user can move up and down those 'sliders' and increase or decrease
the level moving right and left.

Each time the `update' function is called to let it update the
color in some other part.

Default keys:

Up    C-p     move to the previou slider
Down  C-n     move to the next slinder
Left  C-b     decrease level
Right C-f     increase level
RET   q       hide the buffer, calling `quit' with the current color
      C-g     hide the buffer, calling `abort' with the initial color
"
  (with-current-buffer (get-buffer-create title)
    (switch-to-buffer (current-buffer))
    (pjb-color-picker--initialize-buffer (or (if (listp default-rgb)
                                                 default-rgb
                                                 (color-name-to-rgb default-rgb))
                                             '(0.5 0.5 0.5))
                                         update quit abort)))

(defun pjb-color-picker--example ()
  (interactive)
  (let ((old-back (background-color-at-point))
        (old-fore (foreground-color-at-point)))
    (set-foreground-color "white")
    (pjb-color-picker--create-color-picker
     "*test color picker*" "gray33"
     :update (lambda (color)
               (message "update %s" color))
     :quit   (lambda (color)
               (set-background-color (pjb-color-picker--rgb-color color))
               (message "quit   %s" color)
               t)
     :abort  (lambda (color)
               (message "abort  %s" color)
               (set-background-color old-back)
               (set-foreground-color old-fore)
               t))))

;;(pjb-color-picker--example)

