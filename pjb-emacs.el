;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-emacs.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports various functions usefull only in interactive
;;;;    emacs sessions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2010-10-30 <PJB> Renamed multifile-replace-string to recursive-replace-string,
;;;;                     Added recursive-replace-regexp and multifile-replace-regexp.
;;;;    2006-03-23 <PJB> Added fringe-width and scroll-bar-width for full-frame.
;;;;    2004-10-15 <PJB> Added maximize-window.
;;;;    2001-11-30 <PJB> Extracted from pjb-utilities.el.
;;;;
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2010
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
;;;;****************************************************************************
(require 'cl)
(require 'devices nil t)
(require 'font nil t)
(require 'browse-url)
(require 'picture) ;; (import picture-vertical-step picture-horizontal-step)
(require 'sgml-mode)

(require 'pjb-cl)
(require 'eieio)
(require 'pjb-euro)
(require 'pjb-font)
(provide 'pjb-emacs)


(defvar html-quick-keys t )
(defvar html-mode-map
  (let ((map (nconc (make-sparse-keymap) sgml-mode-map))
        (menu-map (make-sparse-keymap "HTML")))
    (define-key map "\C-c6" 'html-headline-6)
    (define-key map "\C-c5" 'html-headline-5)
    (define-key map "\C-c4" 'html-headline-4)
    (define-key map "\C-c3" 'html-headline-3)
    (define-key map "\C-c2" 'html-headline-2)
    (define-key map "\C-c1" 'html-headline-1)
    (define-key map "\C-c\r" 'html-paragraph)
    (define-key map "\C-c\n" 'html-line)
    ;;    (define-key map "\C-c\C-c-" 'html-horizontal-rule)
    ;;    (define-key map "\C-c\C-co" 'html-ordered-list)
    ;;    (define-key map "\C-c\C-cu" 'html-unordered-list)
    ;;    (define-key map "\C-c\C-cr" 'html-radio-buttons)
    ;;    (define-key map "\C-c\C-cc" 'html-checkboxes)
    ;;    (define-key map "\C-c\C-cl" 'html-list-item)
    ;;    (define-key map "\C-c\C-ch" 'html-href-anchor)
    ;;    (define-key map "\C-c\C-cn" 'html-name-anchor)
    ;;    (define-key map "\C-c\C-ci" 'html-image)
    (if html-quick-keys
        (progn
          (define-key map "\C-c-" 'html-horizontal-rule)
          (define-key map "\C-co" 'html-ordered-list)
          (define-key map "\C-cu" 'html-unordered-list)
          (define-key map "\C-cr" 'html-radio-buttons)
          (define-key map "\C-cc" 'html-checkboxes)
          (define-key map "\C-cl" 'html-list-item)
          (define-key map "\C-ch" 'html-href-anchor)
          (define-key map "\C-cn" 'html-name-anchor)
          (define-key map "\C-ci" 'html-image)))
    (define-key map "\C-c\C-s" 'html-autoview-mode)
    (define-key map "\C-c\C-v" 'browse-url-of-buffer)
    (define-key map [menu-bar html] (cons "HTML" menu-map))
    (define-key menu-map [html-autoview-mode]
      '("Toggle Autoviewing" . html-autoview-mode))
    (define-key menu-map [browse-url-of-buffer]
      '("View Buffer Contents" . browse-url-of-buffer))
    (define-key menu-map [nil] '("--"))
    ;;(define-key menu-map "6" '("Heading 6" . html-headline-6))
    ;;(define-key menu-map "5" '("Heading 5" . html-headline-5))
    ;;(define-key menu-map "4" '("Heading 4" . html-headline-4))
    (define-key menu-map "3" '("Heading 3" . html-headline-3))
    (define-key menu-map "2" '("Heading 2" . html-headline-2))
    (define-key menu-map "1" '("Heading 1" . html-headline-1))
    (define-key menu-map "l" '("Radio Buttons" . html-radio-buttons))
    (define-key menu-map "c" '("Checkboxes" . html-checkboxes))
    (define-key menu-map "l" '("List Item" . html-list-item))
    (define-key menu-map "u" '("Unordered List" . html-unordered-list))
    (define-key menu-map "o" '("Ordered List" . html-ordered-list))
    (define-key menu-map "-" '("Horizontal Rule" . html-horizontal-rule))
    (define-key menu-map "\n" '("Line Break" . html-line))
    (define-key menu-map "\r" '("Paragraph" . html-paragraph))
    (define-key menu-map "i" '("Image" . html-image))
    (define-key menu-map "h" '("Href Anchor" . html-href-anchor))
    (define-key menu-map "n" '("Name Anchor" . html-name-anchor))
    map)
  "Keymap for commands for use in HTML mode.") ;;html-mode-map


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random emacs specific elisp functions:


(defun recover-this-file ()
  (interactive)
  (let ((file-path  (buffer-file-name)))
    (if (and file-path (file-exists-p file-path) (file-regular-p file-path))
        (recover-file file-path)
        (message "This buffer has no associated file."))))


(defvar buffer-name-map   nil)
(defvar buffer-list-cache nil)


(defun buffer-named (name)
  "
RETURN: the buffer which has as name `name'.
"
  (let ((bl (buffer-list)))
    (unless (and buffer-list-cache buffer-name-map
                 (equal buffer-list-cache bl))
      (setf buffer-list-cache (copy-seq bl))
      (setf buffer-name-map (make-hash-table :test (function equal)))
      (dolist (buffer buffer-list-cache)
        (let ((name (buffer-name buffer)))
          (when name (setf (gethash name buffer-name-map) buffer)))
        (let ((name (buffer-file-name buffer)))
          (when name (setf (gethash name buffer-name-map) buffer))))))
  (or (gethash name buffer-name-map)
      (gethash (truename name) buffer-name-map)))

            
(defun old-buffer-named (name)
  "
RETURN: the buffer which has as name `name'.
"
  (let ((buffers (buffer-list)) (result))
    (while buffers
      (when (or (when (buffer-name      (car buffers))
                  (string-equal name (buffer-name      (car buffers))))
                (when (buffer-file-name (car buffers))
                  (string-equal name (buffer-file-name (car buffers))))
                (when (and (truename name) (buffer-name      (car buffers)))
                  (string-equal (truename name) 
                                (buffer-name      (car buffers))))
                (when (and (truename name) (buffer-file-name (car buffers)))
                  (string-equal (truename name) 
                                (buffer-file-name (car buffers)))))
        (setq result (car buffers))
        (setq buffers nil))
      (setq buffers (cdr buffers)))
    result)
  ) ;;old-buffer-named


(defun pjb-custom-set-variables (&rest l)
  (while l
    (custom-set-variables (append (car l) (list t)))
    (setq l (cdr l)))
  ) ;;pjb-custom-set-variables


(defun set-default-directory (path)
  (interactive "DDirectory for this buffer: ")
  (setf default-directory path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing functions:

(defun delete-region-and-yank (&optional arg)
  "Deletes region if mark is active and yanks the last kill.
Always replaces the region with the yank, whether the region was
selected via keyboard or mouse.  Also works for normal
yank even with ARGS (thus it can be mapped to \C-y)"
  (interactive "*P")                    ; raw, like yank.
  (message "arg=%S" arg)
  (cond
    (mark-active                        ; delete region
     (let ((str (buffer-substring (point) (mark))))
       (delete-region (point) (mark))
       (if (string=* str (current-kill 0 1))
           (let ((str2 (current-kill 1 1)))
             (kill-new str2 t))))
     (if arg
         (yank arg)
         (yank)))
    ;; else no region selected:
    ((consp arg)                        ; delete forward sexp
     (set-mark (point))
     (forward-sexp 1)
     (delete-region-and-yank))
    (arg (yank arg))
    (t   (yank))))


(defun exch-del-ctrl-h ()
  "Exchange \C-h and <DEL>."
  (interactive)
  ;; Translate `C-h' to <DEL>.
  (keyboard-translate ?\C-h ?\C-?)
  ;; Translate <DEL> to `C-h'.
  (keyboard-translate ?\C-? ?\C-h)
  ) ;;exch-del-ctrl-h


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; picture-mode functions:


(defun picture-draw-pixels (pix-list &optional pixel)
  "
DO:    Draws the pixels of pix-list (a list of (cons x y))
       from current position  as origin.
       Default pixel is '*'.
       Coordinate system is : increasing x to the right,
                              increasing y to the bottom.
"
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         ;;(pvs    picture-vertical-step)
         ;;(phs    picture-horizontal-step)
         ;;(c1     (progn (goto-char start) (current-column)))
         ;;(r1     (picture-current-line))
         ;;(c2     (progn (goto-char end) (current-column)))
         ;;(r2     (picture-current-line))
         ;;(right  (max c1 c2))
         ;;(left   (min c1 c2))
         ;;(top    (min r1 r2))
         ;;(bottom (max r1 r2))
         )
    (unless pixel (setq pixel (character "*")))

    (dolist (point pix-list)
      (goto-line      (+ sl (cdr point))) ;; goto-line first for
      (move-to-column (+ sc (car point)) t)
      (picture-update-desired-column t)
      (picture-insert pixel 1)
      ) ;;dolist

    (goto-line sl)
    (move-to-column sc t)
    ) ;;let*
  nil
  ) ;;picture-draw-pixels


(defun ellipse-quart (a b)
  "
RETURN: A list of integer coordinates approximating a quart (x>=0, y>=0) of
        an ellipse of half width a and half height b.
"
  (let ( (step  (/ pi 4 (sqrt (+ (* a a) (* b b)))))
        (limit (/ pi 2))
         (alpha 0.0)
         (result (list (cons 0 0)))
         x y )

    (while (<= alpha limit)
      (setq x (round (* a (cos alpha)))
            y (round (* b (sin alpha))) )
      (if (or  (/= y (cdar result)) (/= x (caar result)))
          (push (cons x y) result))
      (setq alpha (+ alpha step)))
    (cdr (nreverse result))
    ) ;;let
  )   ;;ellipse-quart


(defun ellipse-full (a b)
  "
RETURN: A list of integer coordinates approximating the whole ellipse
        of half width a and half height b.
"
  (let ( (quart (ellipse-quart a b)) )
    (append
     quart
     (mapcar (lambda (item) (cons (- 0 (car item)) (cdr item))) quart)
     (mapcar (lambda (item) (cons (car item) (- 0 (cdr item)))) quart)
     (mapcar (lambda (item) (cons (- 0 (car item)) (- 0 (cdr item)))) quart))))




(defun picture-draw-function (start end fun plot-char)
  "
DO:     Draw a function in the given rectangle region.
"
  (interactive "*r
xFunction f:[0,1]->[0,1]/x|-->f(x): 
cPlot character: ") ;; start <= end
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         (pvs    picture-vertical-step)
         (phs    picture-horizontal-step)
         (c1     (progn (goto-char start) (current-column)))
         (r1     (picture-current-line))
         (c2     (progn (goto-char end) (current-column)))
         (r2     (picture-current-line))
         (right  (max c1 c2))
         (left   (min c1 c2))
         (top    (min r1 r2))
         (bottom (max r1 r2))
         (width  (+ 0.0 (- right left)))
         (height (+ 0.0 (- bottom top)))
         )
    (goto-line            top)
    (move-to-column left t)
    (picture-update-desired-column t)
    (flet ((fun (x) nil))
      (fset 'fun `(function (lambda (x) ,fun)))
      (picture-draw-pixels 
       (do* ((xi 0 (1+ xi))
             (x) (y) (yi)
             (pixels nil)
             )
            ((> xi width) pixels)
         (setq x  (/ xi width))
         (setq y  (let ((y (unwind-protect (fun x))))
                    (if (< y 0.0) 0.0 (if (< 1.0 y) 1.0 y))))
         (setq yi (round (* height (- 1.0 y))))
         (push (cons xi yi) pixels)) 
       plot-char)
      ) ;;flet
    (goto-line sl)
    (move-to-column sc t))
  ) ;;picture-draw-function



(defun picture-draw-ellipse (start end)
  "
DO:     Draw an ellipse around region.
BUG:    Only draws ellipse of even width and height.
"
  (interactive "*r")                    ; start will be less than end
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         (pvs    picture-vertical-step)
         (phs    picture-horizontal-step)
         (c1     (progn (goto-char start) (current-column)))
         (r1     (picture-current-line))
         (c2     (progn (goto-char end) (current-column)))
         (r2     (picture-current-line))
         (right  (max c1 c2))
         (left   (min c1 c2))
         (top    (min r1 r2))
         (bottom (max r1 r2))
         (a      (/ (- right left) 2))
         (b      (/ (- bottom top) 2))
         )

    (goto-line            (+ top b))
    (move-to-column (+ left a) t)
    (picture-update-desired-column t)
    (picture-draw-pixels (ellipse-full a b) ?*)

    (goto-line sl)
    (move-to-column sc t))
  ) ;;picture-draw-ellipse


(defvar x-cell-size  7 "Width  in pixel of one cell.")
(defvar y-cell-size 14 "Height in pixel of one cell.")

(defun picture-draw-circle (start end)
  "Draw a circle centered on region."
  (interactive "*r")                    ; start will be less than end
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         (pvs    picture-vertical-step)
         (phs    picture-horizontal-step)
         (c1     (progn (goto-char start) (current-column)))
         (r1     (picture-current-line))
         (c2     (progn (goto-char end) (current-column)))
         (r2     (picture-current-line))
         (right  (max c1 c2))
         (left   (min c1 c2))
         (top    (min r1 r2))
         (bottom (max r1 r2))
         (a      (/ (- right left) 2))
         (b      (/ (- bottom top) 2))
         (r      (min (* a (float x-cell-size)) (* b (float y-cell-size))))
         )

    (goto-line            (+ top b))
    (move-to-column (+ left a) t)
    (picture-update-desired-column t)
    (picture-draw-pixels (ellipse-full (round (/ r x-cell-size))
                                       (round (/ r y-cell-size)))?*)

    (goto-line sl)
    (move-to-column sc t)
    ) ;;let*
  )   ;;picture-draw-circle



(defvar picture-fill-pixel ?* 
  "The default pixel used to fill forms.") ;;picture-fill-pixel


(defun picture-fill-rectangle (start end)
  "Fills a rectangle with `picture-fill-pixel', or when a prefix
  argument is given, with the character given in minibuf."
  (interactive "*rP")                   ; start will be less than end
    
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         (pvs    picture-vertical-step)
         (phs    picture-horizontal-step)
         (c1     (progn (goto-char start) (current-column)))
         (r1     (picture-current-line))
         (c2     (progn (goto-char end) (current-column)))
         (r2     (picture-current-line))
         (right  (max c1 c2))
         (left   (min c1 c2))
         (top    (min r1 r2))
         (bottom (max r1 r2))
         (fill-pixel picture-fill-pixel)
         (width  (- right left -1))
         )
    (when current-prefix-arg
      (setq fill-pixel  (character (read-from-minibuffer
                                    "What pixel: "  "*" nil nil nil "*"))))
    (picture-movement-right)
    (do ((line top (1+ line)))
        ((< bottom line))
      (goto-line            line)
      (move-to-column left t)
      (picture-update-desired-column t)
      (picture-insert fill-pixel width))
    (picture-set-motion  pvs phs)
    (goto-line sl)
    (move-to-column sc t)
    ) ;;let*
  )   ;;picture-fill-rectangle


(defun picture-horizontal-segment (line left right)
  (goto-line            line)
  (move-to-column right t)
  (picture-update-desired-column t)
  (buffer-substring (- (point) (- right left)) (1+ (point)))
  ) ;;picture-horizontal-segment


(defun picture-draw-text (line column text)
  "Draws given text from (line,column) toward the current picture-movement."
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         )
    (goto-line line)
    (move-to-column column t)
    (picture-update-desired-column t)
    (do* ((i 0 (1+ i)))
         ((<= (length text) i))
      (picture-insert (char text i) 1))
    (goto-line sl)
    (move-to-column sc t)
    )) ;;picture-draw-text


(defun picture-mirror-vertical (start end)
  "Replace the region by it's vertical mirror."
  (interactive "*r")
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         (pvs    picture-vertical-step)
         (phs    picture-horizontal-step)
         (c1     (progn (goto-char start) (current-column)))
         (r1     (picture-current-line))
         (c2     (progn (goto-char end) (current-column)))
         (r2     (picture-current-line))
         (right  (max c1 c2))
         (left   (min c1 c2))
         (top    (min r1 r2))
         (bottom (max r1 r2))
         )
    (picture-movement-left)
    (do ((line top (1+ line)))
        ((< bottom line))
      (do* ((segment (prog1 (picture-horizontal-segment line left right)
                       (move-to-column right t)
                       (picture-update-desired-column t)))
            (i 0 (1+ i)))
           ((<= (length segment) i))
        (picture-insert (char segment i) 1))
      )
    (picture-set-motion  pvs phs)
    (goto-line sl)
    (move-to-column sc t)
    )) ;;picture-mirror-vertical


(defun picture-mirror-horizontal (start end)
  "Replace the region by it's vertical mirror."
  (interactive "*r")
  (let* ((sl     (picture-current-line))
         (sc     (current-column))
         (pvs    picture-vertical-step)
         (phs    picture-horizontal-step)
         (c1     (progn (goto-char start) (current-column)))
         (r1     (picture-current-line))
         (c2     (progn (goto-char end) (current-column)))
         (r2     (picture-current-line))
         (right  (max c1 c2))
         (left   (min c1 c2))
         (top    (min r1 r2))
         (bottom (max r1 r2))
         )
    (picture-movement-right)
    (do* ((lines  (do ((line top (1+ line))
                       (result '()))
                      ((< bottom line) result)
                    (push (picture-horizontal-segment line left right) result))
                  (cdr lines))
          (line top (1+ line)))
         ((null lines))
      (picture-draw-text line left (car lines)))
    (picture-set-motion  pvs phs)
    (goto-line sl)
    (move-to-column sc t)
    )) ;;picture-mirror-horizontal


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various Editor functions:


(defun pjb-scratch ()
  "
DO:      Goes to the *scratch* buffer, creating it if it does not exists.
"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))


(defun pjb-wc ()
  "
DO:      Apply wc on the file visited in the current buffer.
"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (when file-name
      (shell-command (format "wc %s" (shell-quote-argument file-name))))))


(defun pjb-search-in-all-buffers (string)
  "

"
  (interactive "sString: ")
  (let ( (list (buffer-list)) 
        buffer )
    (save-excursion 
      (while list
        (setq buffer (car list)
              list   (cdr list))
        (set-buffer buffer)
        (goto-char (point-min))
        (if (search-forward string nil t nil)
            (setq list nil)
            (setq buffer nil))))
    (when buffer (switch-to-buffer buffer))))
    


(when nil
  (defun url-retrieve-as-string (url)
    "RETURN: A string containing the data found at the url."
    ;; This version uses w3.
    ;; An alternative could be:
    ;; (shell-command-to-string (format "lynx -source '%s'" url))
    (save-excursion
      (let* ((status (url-retrieve url))
             (cached (car status))
             (url-working-buffer (cdr status)))
        (set-buffer url-working-buffer)
        (buffer-string)))))


(defun url-retrieve-as-string (url)
  "RETURN: A string containing the data found at the url."
  (shell-command-to-string (format "lynx -source '%s'" url)))


(defun pjb-browse-url-lynx-xterm (url &optional new-window)
  ;; new-window ignored
  "Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  A new Lynx process is run
in an Xterm window using the Xterm program named by `browse-url-xterm-program'
with possible additional arguments `browse-url-xterm-args'."
  (interactive (browse-url-interactive-arg "Lynx URL: "))
  (apply #'start-process `(,(concat "lynx" url) nil 
                            "pjb-xterm" ; ,browse-url-xterm-program
                            ,@browse-url-xterm-args 
                            "-geometry" "80x40+10+0" "-bg" "#eeff99"
                            "-e" "lynx" ,url)))


(defun pjb-auto-scroll-up (speed)
  "DO:   Scroll down the current buffer until the end-of-buffer is visible,
      at the specified speed. Depending on the data, and your reading speed,
      speed values between 0.2 and 1 line/sec may be useful."
  (interactive "nSpeed: ")
  (let ((delay (/ 1.0  speed)))
    (message "Auto-scrolling...")
    (while (not (pos-visible-in-window-p (point-max)))
      (sit-for delay)
      (scroll-up 1)
      (force-mode-line-update t))
    (message "Done.")))


(defun pjb-regexp-nocase-region (start end)
  (interactive "r")
  (let* ( (s (string-to-vector (buffer-substring start end)))
         (l (length s))
          (r nil) 
          (i 0) 
          c C )
    (while (< i l) 
      (setq c (aref s i))
      (setq C (upcase   c))
      (setq c (downcase c))
      (if (eq c C)
          (setq r (cons (format "%c" c) r))
          (setq r (cons (format "[%c%c]" C c) r)))
      (setq i (1+ i))
      ) ;;while
    (delete-region start end)
    (insert (apply 'concat (nreverse r)))))


(defun pjb-animate (speed)
  (interactive "nSpeed: ")
  (let ((delay (/ 1.0  speed))
        (done  nil))
    (widen)
    (goto-char (point-min))
    (message "Animating...")
    (while (not done)
      (widen)
      (if (search-forward "\f" nil 'at-limit)
          nil
          (goto-char (point-max))
          (setq done t))
      (narrow-to-page)
      (sit-for delay)
      (force-mode-line-update t)
      ) ;;while
    (message "Done.")))




(defvar pjb-listing-light "LightBlue" 
  "Background color of light listing area.") ;;pjb-listing-light

(defvar pjb-listing-dark "LightSteelBlue" 
  "Background color of dark listing area.") ;;pjb-listing-dark

(defun pjb-colorize-listing-region (arg)
  "
DO:      Colorize the region with group of lines (normaly 1 by 1)
         with different background color).
"
  (interactive "pGroup size: ")
  (error "Sorry, it does not work yet.")
  (setq arg (prefix-numeric-value arg))
  (setq current-prefix-arg nil)
  (let ( (lines-forward (1+ (or arg 1)))
        (color (cons pjb-listing-light pjb-listing-dark)) 
         (start (region-beginning))
         (end   (region-end)) )
    ;; round the end to the beginning of next line.
    (goto-char end)
    (when (/= end (line-beginning-position))
      (beginning-of-line 2)
      (setq end (point)))

    ;; round the start to the beginning of first line.
    (goto-char start)
    (when (/= start (line-beginning-position))
      (beginning-of-line)
      (setq start (point)))

    (while (< start end)
      (goto-char start)
      ;; (message "avant %S" (point))
      (beginning-of-line lines-forward)
      ;; (message "apres %S" (point))
      (if (< end (point))
          (progn (goto-char end) (beginning-of-line 2)))
      ;;(message "%16s from %4d to %4d" (car color) start (point))
      (set-mark start)
      (goto-char (point))
      (facemenu-set-background (car color) start (point))
      (setq start (point))
      (setq color (cons (cdr color) (car color))))))


(defun pjb-old-look ()
  "
DO:      Set the background, foreground and font to look like an oldie tty.
"
  (interactive)
  (set-background-color "#000")
  (set-foreground-color "#0f2")
  (set-cursor-color     "#3f1")
  (mouse-set-font "-adobe-courier-bold-r-normal--*-120-*-*-m-*-iso8859-1")
  (setq global-font-lock-mode nil)
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (font-lock-mode -1)))
  ) ;;pjb-old-look


(defun pjb-address (pattern)
  "
DO:      Search an address in my address book (~/private/info/personnes.form)
"
  (interactive "MSearch address: ")
  (let ((personnes-forms (buffer-named "personnes.forms")))
    (if personnes-forms 
        (switch-to-buffer personnes-forms)
        (forms-find-file (format "%sprivate/info/personnes.forms"
                           (namestring (user-homedir-pathname))))))
  (forms-search-forward pattern))



(defvar pjb-cross-references-rejected-regexp
  "\\(^\\.?#\\|~$\\|\\.\\(elc\\|o\\|a\\)$\\)"
  "A regexp matching file names that should not be searched 
for cross references.") ;;pjb-cross-references-rejected-regexp


(defun pjb-cross-references ()
  "
DO:      Grep current directory for sources containing the current word.
"
  (interactive)
  (let ( (word  (current-word))
        (files (nremove-nil
                (mapcar (lambda (name) 
                          (cond
                            ((file-directory-p name) nil)
                            ((string-match pjb-cross-references-rejected-regexp
                                           name) nil)
                            (t (shell-quote-argument name))) )
                        (directory-files "." nil nil t)))) )
    (grep (format "grep -n -e %s %s" 
            (shell-quote-argument word) (unsplit-string files " ")))))




(defun pjb-eurotunnel ()
  "
DO:      get-devises and insert some eurotunnel data.
"
  (interactive)
  (let ((today (calendar-current-date)))
    (get-devises)
    (mapcar
     (lambda (line) 
       (let* ((fields   (split-string line ";"))
              (sym      (nth 0 fields))
              (quo      (string-to-number 
                         (replace-regexp-in-string "," "." (nth 1 fields) nil nil)))
              )
         (cond

           ((string-match "22457" sym)
            (printf
             "  | %4d-%02d-%02d    %8.6f   %4d %10s = %7.2f %11s |\n"
             (nth 2 today) (nth 0 today) (nth 1 today)
             quo 4400 sym (* quo 4400) " "))

           ((string-match "12537" sym)
            (printf
             "  | %4d-%02d-%02d    %8.6f        %10s    %18s |\n"
             (nth 2 today) (nth 0 today) (nth 1 today)
             quo  sym  " "))

           ((string-equal sym "GBP=X")
            (printf
             "  | %4d-%02d-%02d    %8.6f          %3s      ~ %7.4f %11s |\n"
             (nth 2 today) (nth 0 today) (nth 1 today)
             (/ (euro-from-value 10000 UKL) 10000.0) 'UKL
             (/ (+ (euro-from-value (* 1495 0.68) UKL) (* 1495 1.0214)) 1495)
             "EUR/12537"))

           (t))))

     (split-string 
      (url-retrieve-as-string 
       "http://fr.finance.yahoo.com/d/quos.csv?s=22456+22457+12537+GBP=X&m=PA&f=sl1d1t1c1ohgv&e=.csv")))))





(defun pjb-backcolors ()
  "
DO:     Insert in the current buffer a list of colors and 
        facemenu-set-background them.
"
  (interactive)
  (let ((f (lambda (x) (+ 255 (* 6 (- x 15))))) )
    (for
     r 10 12
     (for
      g 10 12
      (for
       b 10 12
       (let ((min (point)))
         (set-mark min)
         (printf " *   Color :  #%x%x%x   * \n" 
                 (funcall f r) (funcall f g) (funcall f b))
         (facemenu-set-background 
          (format "#%x%x%x"
            (funcall f r) (funcall f g) (funcall f b)) 
          min (point))))))))




(defun chronometre (lambda-body &optional outstream)
  "
DO:     Chronometre the execution of `lambda-body'. 
        Writes a message indicating the time it took.
RETURN: (cons seconds the result of `lambda-body').
"
  (let* ( (start  (current-time))
         (result (funcall lambda-body))
          (stop   (current-time)) 
          (time   (- (emacs-time-to-seconds stop) 
                     (emacs-time-to-seconds start))) )
    (printf outstream "Took %f seconds." time)
    (cons time result)
    ) ;;let*
  )   ;;chronometre




(defun fill-region (from to &optional justify nosqueeze to-eop)
  "Fill each of the paragraphs in the region.
A prefix arg means justify as well.
Ordinarily the variable `fill-column' controls the width.

Noninteractively, the third argument JUSTIFY specifies which
kind of justification to do: `full', `left', `right', `center',
or `none' (equivalent to nil).  t means handle each paragraph
as specified by its text properties.

The fourth arg NOSQUEEZE non-nil means to leave
whitespace other than line breaks untouched, and fifth arg TO-EOP
non-nil means to keep filling to the end of the paragraph (or next
hard newline, if `use-hard-newlines' is on).

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive (list (region-beginning) (region-end)
                     (if current-prefix-arg 'full)))
  (unless (memq justify '(t nil none full center left right))
    (setq justify 'full))
  (let (end beg)
    (save-restriction
      (goto-char (max from to))
      (if to-eop
          (progn (skip-chars-backward "\n")
                 (forward-paragraph)))
      (setq end (point))
      (goto-char (setq beg (min from to)))
      (beginning-of-line)
      (narrow-to-region (point) end)
      (while (not (eobp))
        (let ((initial (point))
              end)
          ;; If using hard newlines, break at every one for filling
          ;; purposes rather than using paragraph breaks. 
          (if use-hard-newlines
              (progn 
                (while (and (setq end (text-property-any (point) (point-max)
                                                         'hard t))
                            (not (= (character "\n") (char-after end)))
                            (not (= end (point-max))))
                  (goto-char (1+ end)))
                (setq end (if end (min (point-max) (1+ end)) (point-max)))
                (goto-char initial))
              (forward-paragraph 1)
              (setq end (point))
              (forward-paragraph -1))
          (if (< (point) beg)
              (goto-char beg))
          (if (>= (point) initial)
              (fill-region-as-paragraph (point) end justify nosqueeze)
              (goto-char end)))))))



(defun permutations (list)
  "Retourne une liste de toutes les permutations de list."
  (mapcan (lambda (item)
            (if (= 1 (length list))
                (list (list item))
                (mapcar (lambda (rest) (cons item rest))
                        (permutations (remove* item list :count 1)))))
          list))


(defun perm-words ()
  "Insère après la ligne courrante toutes les permutations des mots de la ligne courrante."
  (interactive)
  (let ((words (car (read-from-string
                     (format "(%s)" (buffer-substring-no-properties
                                     (progn (beginning-of-line) (point))
                                     (progn (end-of-line)       (point))))))))
    (end-of-line)
    (insert "\n")
    (dolist (line (permutations words))
      (dolist (word line)
        (insert (format "%s "
                  (if (and (listp word) (eq 'quote (car word))) 
                      (cadr word) word))))
      (insert "\n")))) ;;perm-words


(defvar *fortune-file* "/data/cookies/bopcs.cookies")

(defun add-fortune (start end)
  "
Add the selection to the local fortune file.
"
  (interactive "r")
  (let ((fortune (buffer-substring-no-properties
                  (min start end) (max start end))))
    (find-file *fortune-file*)
    (goto-char (point-max))
    (insert fortune)
    (insert "\n#\n")
    (save-buffer 1)
    (bury-buffer))
  ) ;;add-fortune
(defalias 'add-cookie 'add-fortune)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *window-manager-above*
  (if (boundp 'aquamacs-version)
    (+ 22)
    (+ 22 1))
  "The number of vertical pixels eaten by the window manager
   above the frame (window title).")

(defvar *window-manager-below*
  (if (boundp 'aquamacs-version)
      (+ 2)
      (+ 8))
  "The number of vertical pixels eaten by the window manager
   below the frame (grow bar).")

(defvar *window-manager-y-offset*
  (+ *window-manager-above* *window-manager-below*)
  "The number of vertical pixels eaten by the window manager
   (above and below).")

(defvar *window-manager-left* 1
  "The number of horizontal pixels eaten by the window manager
   on the left.")

(defvar *window-manager-right* 1
  "The number of horizontal pixels eaten by the window manager
   on the right.")

(defvar *window-manager-x-offset*
  (+  *window-manager-left*  *window-manager-right*)
  "The number of horizontal pixels eaten by the window manager.")


(defvar *current-frame* nil) 

(defun current-frame ()
  "
RETURN: The current frame.
"
  (selected-frame))


(defmacro define-frame-parameter (name)
  `(defun ,(intern (format "frame-%s" name)) (&optional frame)
     (frame-parameter (or frame (selected-frame)) ',name)))

;; (dolist (p (frame-parameters)) (insert (format "(define-frame-parameter %s)\n" (car p))))

(define-frame-parameter parent-id)
(define-frame-parameter display)
(define-frame-parameter visibility)
(define-frame-parameter icon-name)
(define-frame-parameter outer-window-id)
(define-frame-parameter window-id)
(define-frame-parameter top)
(define-frame-parameter left)
(define-frame-parameter buffer-list)
(define-frame-parameter unsplittable)
(define-frame-parameter minibuffer)
(define-frame-parameter modeline)
(define-frame-parameter width)
(define-frame-parameter height)
(define-frame-parameter name)
(define-frame-parameter background-mode)
(define-frame-parameter display-type)
(define-frame-parameter horizontal-scroll-bars)
(define-frame-parameter scroll-bar-width)
(define-frame-parameter cursor-type)
(define-frame-parameter auto-lower)
(define-frame-parameter auto-raise)
(define-frame-parameter icon-type)
(define-frame-parameter wait-for-wm)
(define-frame-parameter title)
(define-frame-parameter buffer-predicate)
(define-frame-parameter tool-bar-lines)
(define-frame-parameter menu-bar-lines)
(define-frame-parameter scroll-bar-background)
(define-frame-parameter scroll-bar-foreground)
(define-frame-parameter right-fringe)
(define-frame-parameter left-fringe)
(define-frame-parameter line-spacing)
(define-frame-parameter screen-gamma)
(define-frame-parameter border-color)
(define-frame-parameter cursor-color)
(define-frame-parameter mouse-color)
(define-frame-parameter background-color)
(define-frame-parameter foreground-color)
(define-frame-parameter vertical-scroll-bars)
(define-frame-parameter internal-border-width)
(define-frame-parameter border-width)
(define-frame-parameter font)


(defalias 'frame-pixel-top  'frame-top)
(defalias 'frame-pixel-left 'frame-left)


(defun set-default-frame-parameter (name value)
  (let ((acell (assoc name default-frame-alist)))
    (if acell
        (setf (cdr acell) value)
        (push (cons name value)  default-frame-alist))
    value))

(when (fboundp 'set-background-color)
  (defadvice set-background-color (after sbc-fringe last (color-name) activate)
    (when (facep 'fringe) (set-face-background 'fringe color-name))))



(defun max-frame-line-number (&optional frame)
  "
RETURN: The maximum number of line that can be displayed on this frame
        inside this screen.
"
  (truncate
   (/ (- (x-display-pixel-height frame) *window-manager-y-offset*)
      (frame-char-height frame))))


(defun max-frame-column-number (&optional frame margin)
  "
MARGIN: Number of pixel to substract from the display width.
RETURN: The maximum number of columns that can be displayed on this frame
        inside this screen.
"
  (setf margin (or margin 0))
  (truncate
   (/ (- (x-display-pixel-width frame) margin *window-manager-x-offset*)
      (frame-char-width frame))))


(defun maximize-frame ()
  "Enlarge the window to span the whole screen."
  (interactive)
  (let ((*current-frame* (current-frame)))
    (set-frame-width  *current-frame* 
                      (max-frame-column-number
                       *current-frame* (+ (if current-prefix-arg 64 0) 34)))
    (set-frame-height *current-frame* (max-frame-line-number *current-frame*))
    (set-frame-position *current-frame* 0 0)
    (delete-other-windows)))


(defun char-to-pixel-width (w &optional frame)
  (* w (frame-char-width (or frame (current-frame)))))

(defun pixel-to-char-width (w &optional frame)
  (truncate (/ w (frame-char-width (or frame (current-frame))))))

(defun char-to-pixel-height (h &optional frame)
  (* h (frame-char-height (or frame (current-frame)))))

(defun pixel-to-char-height (h &optional frame)
  (truncate (/ h (frame-char-height (or frame (current-frame))))))


(defun move-frame-right (offset)
  "Move the frame to the right (or the left with a negative offset)."
  (interactive "NOffset: ")
  (let ((*current-frame* (current-frame)))
    (set-frame-position *current-frame*
                        (+ offset (eval (frame-pixel-left *current-frame*)))
                        (eval (frame-pixel-top *current-frame*)))))


(defun move-frame-down (offset)
  "Move the frame down (or up with a negative offset)."
  (interactive "NOffset: ")
  (let ((*current-frame* (current-frame)))
    (set-frame-position *current-frame*
                        (eval (frame-pixel-left *current-frame*))
                        (+ offset (eval (frame-pixel-top *current-frame*))))))

(defun move-frame-to (args)
  (interactive "X'(left top)= ")
  (let ((*current-frame* (current-frame)))
    (destructuring-bind (x y) args
      (set-frame-position *current-frame* x y))))


(unless (fboundp 'scroll-bar-columns)
  (defun scroll-bar-columns (side)
    2) )

(defun scroll-bar-width (&optional frame)
  "Return the width of the vertical scroll bar in columns"
  (setf frame (or frame  (current-frame)))
  (if (frame-parameter frame 'vertical-scroll-bars)
      (scroll-bar-columns (frame-parameter frame 'vertical-scroll-bars))
      0))


(unless (fboundp 'window-fringes)
  (defun window-fringes ()
    (list (frame-char-width) (* 2 (frame-char-width)) nil)) )

(unless (fboundp 'fringe-columns)
  (defun fringe-columns (side &optional real)
    "Return the width, measured in columns, of the fringe area on SIDE.
If optional argument REAL is non-nil, return a real floating point
number instead of a rounded integer value.
SIDE must be the symbol `left' or `right'."
    (funcall (if real '/ 'ceiling)
             (or (funcall (if (eq side 'left) 'car 'cadr)
                          (window-fringes))
                 0)
             (float (frame-char-width)))) )

(defun fringe-width ()
  (round (+ (fringe-columns 'left 'real) (fringe-columns 'right 'real))))



(defun position-x (pos &optional frame)
  "
POS:    Either an integer denoting a X window position,
        or a list (+ int) denoting a X window position starting out of screen.
        (+ -2) means two pixels out of the left side of the screen.
RETURN: The X window position for the frame corresponding to pos on the x axis.
        A positive number is relative to the left screen border, and toward 
        the right,
        a negative number is relative to the right screen border, and
        toward the right too  (its absolute value, toward the left).
SEE:   position-y"
  (let ((frame (or frame (current-frame))))
    (cond
      ((consp pos)
       (if (and (eq '+ (first pos)) (minusp (second pos)))
           (position-x (second pos) frame)
           (error "Unexpected x position: %S" pos)))
      ((minusp pos)
       (- (+ pos (frame-pixel-width frame))  (x-display-pixel-width  frame)))
      (t pos))))


(defun position-y (pos &optional frame)
  "
POS:    Either an integer denoting a X window position,
        or a list (+ int) denoting a X window position starting out of screen.
        (+ -2) means two pixels out of the top side of the screen.
RETURN: The X window position for the frame corresponding to pos on the y axis.
        A positive number is relative to the top screen border, and down,
        a negative number is relative to the bottom screen border, and down too 
        (its absolute value, up).
SEE:   position-x
"
  (let ((frame (or frame (current-frame))))
    (cond
      ((consp pos)
       (if (and (eq '+ (first pos)) (minusp (second pos)))
           (position-y (second pos) frame)
           (error "Unexpected y position: %S" pos)))
      ((minusp pos)
       (- (+ pos (frame-pixel-height frame))  (x-display-pixel-height  frame)))
      (t pos))))


(defun screen-usable-origin (&optional frame)
  "
RETURN: The origin of the screen where the frame lies.

NOTE:   For multi-screen displays, the coordinate system could be such that
        the origin of a screen may be expressed in negative coordinates.
        In this case, the returned values may be lists of the form: 
        (+ -|x|) (+ -|y|).
"
  (let ((frame (or frame (current-frame))))
    (let ((x (frame-pixel-left frame))
          (y (frame-pixel-top  frame)))
      (set-frame-position frame 0 0)
      (prog1 (list (frame-pixel-left frame) (frame-pixel-top frame))
        (set-frame-position frame (position-x x) (position-y y))))))

(defun screen-usable-area (&optional frame)
  "
RETURN: The origin and width and height of the screen where the frame lies,
        expressed as non negative numbers.
"
  (let* ((frame (or frame (current-frame)))
         (origin        (screen-usable-origin frame))
         (screen-width  (x-display-pixel-width  frame))
         (screen-height (x-display-pixel-height frame)))
    (list (eval (first origin))
          (eval (second origin))
          (- screen-width (eval (first origin)))
          (- screen-height (eval (second origin))))))


;; (list (frame-pixel-left) (frame-pixel-top) (frame-width) (frame-height))
;; (0 (+ -23) 179 78)

(defun full-frame (&optional prefix)
  "Spread the frame to cover the full screen, or parts of it.

The screens as managed on xinerama or mergedfb, with one coordinate system,
so we just divide the screen size in two 'screens'.

Vertical decorations    Vertical decorations 
in screen.              out of screen.
 
+---------++---------+
|         ..         |
|         .. 1       |  -1
|         ..         |
+---------++---------+

+---------++---------+
|         ||         |
|    2    ||    3    |  -2 -3
|         ||         |
+---------++---------+

+----+----++----+----+
|    |    ||    |    |
|  4 |  5 ||  6 |  7 |  -4 -5 -6 -7
|    |    ||    |    |
+----+----++----+----+

+---------++---------+
|    21   ||   31    |
+---------++---------+  No decorationless here.
|    22   ||   32    |
+---------++---------+

+----+----++----+----+
| 41 | 51 || 61 | 71 |  
+----+----++----+----+  No decorationless here.
| 42 | 52 || 62 | 72 |  
+----+----++----+----+

"
  (interactive "p")
  (let* ((frame (current-frame))
         (area (screen-usable-area frame))
         (screen-left   (first  area))
         (screen-top    (second area))
         (screen-width  (third  area))
         (screen-height (fourth area)))
    (if (not (member (abs prefix) '(1 2 3 4 5 6 7
                                    -1 -2 -3 -4 -5 -6 -7
                                    21 22 31 32
                                    41 42 51 52 61 62 71 72)))
        (message "Invalid prefix %S; expecting: %s"
                 prefix
                 "[   1   ]   [ 2 | 3 ]   [4|5|6|7]
Multiply by -1 = without decoration.
Multiply by 10 and add 1 for top half, and 2 for bottom half.
")
        (let* ((top-offset    (if (minusp prefix)
                                  (- *window-manager-above*) 0))
               (height-offset (if (minusp prefix)
                                  0 (- *window-manager-y-offset*)))
               (prefix (abs prefix))
               (hpref  (if (< prefix 10) prefix (truncate prefix 10))) ; 1..7
               (vpref  (if (< prefix 10) 0 (mod prefix 10))) ; 0,1,2
               (left   (+ screen-left
                          (case hpref
                            ((1 2 4) 0)
                            ((3 6)   (truncate screen-width 2))
                            ((5)     (truncate screen-width 4))
                            ((7)     (* 3 (truncate screen-width 4))))))
               (width  (truncate screen-width (case hpref
                                                ((1)       1)
                                                ((2 3)     2)
                                                ((4 5 6 7) 4))))
               (top    (+ screen-top
                          (case vpref
                            ((0 1) 0)
                            ((2)   (truncate (- screen-height
                                                *window-manager-y-offset*)
                                             2)))))
               (height (case vpref
                         ((0)   screen-height)
                         ((1 2) (truncate (- screen-height
                                             *window-manager-y-offset*) 2)))))
          (labels ((mesframe (frame)
                     (message "0: x=%8S y=%8S w=%8S h=%8S"
                              (frame-pixel-left frame)
                              (frame-pixel-top frame)
                              (frame-pixel-width frame)
                              (frame-pixel-height frame)))
                   (move-frame (x w y h)
                     (mesframe frame)
                     (message "1: x=%8S y=%8S w=%8S h=%8S" x y w h)
                     (set-frame-width
                      frame
                      (pixel-to-char-width
                       (- w (char-to-pixel-width
                             (+ (fringe-width) (scroll-bar-width frame))))
                       frame))
                     (set-frame-height frame (pixel-to-char-height h frame))
                     (mesframe frame)
                     (setf x (position-x x)
                           y (position-y y)
                           w (frame-pixel-width  frame)
                           h (frame-pixel-height frame))
                     (message "2: x=%8S y=%8S w=%8S h=%8S" x y w h)
                     (set-frame-position frame x y)
                     (mesframe frame)))
            (move-frame left width
                        (+ top top-offset) (+ height height-offset)))))))



               
(defun single-frame ()
  "Reduce the frame, to one 80-columns window."
  (interactive)
  (let ((*current-frame* (current-frame)))
    (set-frame-width *current-frame* 81)
    (set-frame-height *current-frame* (max-frame-line-number))
    (if current-prefix-arg
        (set-frame-position *current-frame* -1  0)
        (set-frame-position *current-frame* -64 0))
    (delete-other-windows)))


(defun double-frame ()
  "Enlarge the frame, and split it horizontally in two 80-column windows."
  (interactive)
  (let ((*current-frame* (current-frame)))
    (setq truncate-partial-width-windows nil)
    (set-frame-width *current-frame* 167)
    (set-frame-height *current-frame* (max-frame-line-number))
    (set-frame-position *current-frame* 0 0)
    (delete-other-windows)
    (split-window-horizontally 86)
    (other-window 1)
    (switch-to-buffer
     (do ((buffers (buffer-list) (cdr buffers)))
         ((or (null buffers)
              (not (or (position (char (buffer-name (car buffers)) 0) " *")
                       (equal (current-buffer) (car buffers)))))
          (car buffers))))))


(defun half-frame ()
  "Reduce the frame, to one 40-columns window."
  (interactive)
  (let ((*current-frame* (current-frame)))
    (set-frame-width *current-frame* 41)
    (set-frame-position *current-frame* -64 0)
    (delete-other-windows)))


(defun naiad-frame ()
  ""
  (interactive)
  (let ((*current-frame* (current-frame)))
    (set-frame-width  *current-frame* 81)
    (set-frame-height *current-frame* 55)
    (set-frame-position *current-frame* -64 0)))


(defvar *frame-maximized-states* (make-hash-table)
  "Maps frames to their maximized state: When not maximized = nil; 
                                         when maximized = ((x y) w h)")

;; assuming each frame has its own state.
;; The following is to clean up the entry in the hash table when the 
;; frame is deleted:

(add-hook 'delete-frame-hook
          (lambda (frame) (setf (gethash frame *frame-maximized-states*) nil)))

;; Now let's toggle:

(defun toggle-maximize-frame ()
  (interactive)
  (let* ((frame (selected-frame))
         (state (gethash frame *frame-maximized-states*)))
    (if state
        (progn
          (apply (function set-frame-position) frame (first state))
          (set-frame-width  frame (second state))
          (set-frame-height frame (third state))
          (setf state nil))
        (let ((fp (frame-parameters nil)))
          (setf state (list (list (cdr (assoc 'left fp))
                                  (cdr (assoc 'top fp)))
                            (cdr (assoc 'width fp))
                            (cdr (assoc 'height fp))))
          (set-frame-width  frame (max-frame-column-number frame 34))
          ;; I don't know where these 34 go?
          (set-frame-height frame (max-frame-line-number   frame))
          (set-frame-position frame 0 0)))
    (setf (gethash frame *frame-maximized-states*) state)))


(defun main-frame ()
  "The current frame becomes the main frame, ie. the other frames will
only display one window with the scratch buffer"
  (interactive)
  (let ((current-frame (current-frame)))
    (dolist (frame (remove current-frame (frame-list)))
      (select-frame frame)
      (delete-other-windows)
      (switch-to-buffer "*scratch*"))
    (select-frame current-frame)))


(defun after-make-frame/full-frame-meat (&optional frame)
  "Move the new frame to an open area.
   +----+----+   +----+----+
   |    |    |   |  4 | 5  |
   |  2 | 3  |   +----+----+
   |    |    |   |  6 | 7  |
   +----+----+   +----+----+
"
  ;; TODO: The magic constant 40 depends actually on the window manager decorations
  ;; TODO: Replace it with
  (interactive)
  (let* ((frame         (or frame (current-frame)))
         (area          (screen-usable-area frame))
         (screen-left   (first  area))
         (screen-top    (second area))
         (screen-width  (third  area))
         (screen-height (fourth area))
         (other-frames  (remove-if
                         (lambda (fr) (or (eq fr frame)
                                     (not (equal (frame-display fr)
                                                 (frame-display frame)))))
                         (frame-list))))
    (select-frame frame)
    (case (length other-frames)
      ((0) (full-frame 3))              ; by default go to the right.
      ((1) (let ((left (eval (frame-pixel-left (first other-frames)))))
             (if (< left (truncate (- screen-width 20) 2))
                 (full-frame 3)
                 (full-frame 2))))
      (otherwise
       (let ((used-squares '()))
         (dolist (fr other-frames)
           (let ((h (if (< (eval (frame-pixel-left fr))
                           (- (truncate screen-width  2) *window-manager-x-offset*))
                        ;; on the left 46 [+ 57]
                        (if (<= (frame-pixel-width fr) (truncate screen-width 2))
                            '(4 6)
                            '(4 6 5 7))
                        ;; on the right 57 whatever.
                        '(5 7)))
                 (v (if (< (eval (frame-pixel-top fr))
                           (- (truncate screen-height 2) *window-manager-y-offset*))
                        ;; on the top 45 [+ 67]
                        (if (<= (frame-pixel-height fr) (truncate screen-height 2))
                            '(4 5)
                            '(4 5 6 7))
                        ;; on the bottom whatever.
                        '(6 7))))
             (setf used-squares (union used-squares (intersection h v)))))
         (cond
           ((null (intersection '(5 7) used-squares)) (full-frame 3))
           ((null (intersection '(4 6) used-squares)) (full-frame 2))
           ((not (member 4 used-squares))             (full-frame 4))
           ((not (member 5 used-squares))             (full-frame 5))
           ((not (member 6 used-squares))             (full-frame 6))
           ((not (member 7 used-squares))             (full-frame 7))
           (t                                         (full-frame 3))))))))

(when (and window-system (not (getenv "RATPOISON")))
  (pushnew (function after-make-frame/full-frame-meat) after-make-frame-functions))

;; (setf  after-make-frame-functions  (remove (function after-make-frame/full-frame-meat) after-make-frame-functions))


(defvar *excluded-frames* '()
  "List of excluded frames, not considered by `other-frame-non-excluded'")

(defun exclude-frame ()
  "Remove the selected frame from the frames used by other-frame-non-excluded"
  (interactive)
  (pushnew (selected-frame) *excluded-frames*))

(defun include-frame ()
    "Add the selected frame to the frames used by other-frame-non-excluded"
    (setf *excluded-frames* (delete (selected-frame) *excluded-frames*)))

(defun other-frame-non-excluded (arg)
  "Select the argth different visible frame on current display, and raise it,
but ignoring the frames listed in `*excluded-frames*'.
All frames are arranged in a cyclic order.
This command selects the frame arg steps away in that order.
A negative arg moves in the opposite order.

To make this command work properly, you must tell Emacs
how the system (or the window manager) generally handles
focus-switching between windows.  If moving the mouse onto a window
selects it (gives it focus), set `focus-follows-mouse' to t.
Otherwise, that variable should be nil.

See also: `exclude-frame' and `include-frame'
"
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (while (not (eq (frame-visible-p frame) t))
        (setq frame (next-frame frame)))
      (unless  (member frame *excluded-frames*)
        (setq arg (1- arg))))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (while (not (eq (frame-visible-p frame) t))
        (setq frame (previous-frame frame)))
      (unless (member frame *excluded-frames*)
        (setq arg (1+ arg))))
    (select-frame-set-input-focus frame)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'swap-windows 'rotate-buffers)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Searching selected text
;;

(defvar *last-search-text* nil)

(defun pjb-search-backward-region ()
  (interactive)
  (let ((text *last-search-text*))
    (when mark-active
      (let ((start (min (mark) (point)))
            (end   (max (mark) (point))))
        (setq text (buffer-substring-no-properties start end))
        (setq *last-search-text* text)
        (goto-char start)))
    (cond
      ((null text) (error "No text to search."))
      ((search-backward text nil t)
       (set-mark (match-end 0)))
      (t (error "Can't find %S" text))))
  ) ;;pjb-search-backward-region


(defun pjb-search-forward-region ()
  (interactive)
  (let ((text *last-search-text*))
    (when mark-active
      (let ((start (min (mark) (point)))
            (end   (max (mark) (point))))
        (setq text (buffer-substring-no-properties start end))
        (setq *last-search-text* text)
        (goto-char end)))
    (cond
      ((null text) (error "No text to search."))
      ((search-forward text nil t)
       (set-mark (match-beginning 0)))
      (t (error "Can't find %S" text))))
  ) ;;pjb-search-forward-region


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Masking private text
;;


(defun filter-region (fun &optional start end)
  "
DO:    Apply the function fun(character)->string to the region from 
       start or (min (point) (mark)) to end or (max (point) (mark)).
       The region is replaced at the end of the processing.
"
  (setq start (or start (min (point) (mark))))
  (setq end (or end (max (point) (mark))))
  (do* ((pos start (1+ pos))
        (ch  (buffer-substring-no-properties pos (1+ pos))
             (buffer-substring-no-properties pos (1+ pos)))
        (replacement '()))
       ((<= end pos)
        (progn
          (delete-region start end)
          (insert (apply (function concatenate) 
                         'string (nreverse replacement)))))
    (push (funcall fun pos ch) replacement))
  ) ;;filter-region


(defun is-space (c)
  "RETURN: Whether C is a space."
  (member c '(9 10 11 12 13 32))) ;;is-space


(defun blind-text-region (start end)
  "
DO:         Substitutes every alphanumeric text by a 'x'.
SEE-ALSO:   activate-peril-sensitive-sunglasses
"
  (interactive "*r")
  (filter-region 
   (lambda (pos ch) 
     (cond ((or (is-space (character ch))
                (string=*  "\n" ch)
                (string=* "=" ch)
                (and (string/=* "\n" ch)
                     (string=* "=" (buffer-substring-no-properties 
                                    (- pos 1) pos)))
                (and (string/=* "\n" (buffer-substring-no-properties 
                                      (- pos 1) pos))
                     (string=* "=" (buffer-substring-no-properties 
                                    (- pos 2) (- pos 1)))))
            ch)
           ((alphanumericp (character ch)) "x")
           (t ch)))
   start end))


(defun activate-peril-sensitive-sunglasses ()
  "emergency protection from sight of ugly code
With apologies to Zaphod Beeblebrox.
SEE-ALSO:  blind-text-region
"
  (interactive)
  (mapc (lambda (face)
          (set-face-foreground face "black")
          (set-face-background face "black"))
        (face-list)))


;; (mapcar (function window-buffer) (window-list nil nil))

(defun rotate-buffers ()
  "Rotate the buffers in the current windows."
  (interactive)
  (let ((buffers (mapcar (function window-buffer) (window-list nil nil))))
    (mapcar* (function set-window-buffer)
             (window-list nil nil)
             (if current-prefix-arg
                 (append (cdr buffers) (list (car buffers)))
                 (cons (car (last buffers)) (butlast buffers))))))

(defalias 'rotate-windows 'rotate-buffers)

;;;----------------------------------------
;;; Keymaps:
;;;----------------------------------------
                                                              
(defun make-keymap-local ()
  "Creates a buffer local keymap to have local-set-key define keys local 
to the buffer instead of local to the mode."
  (interactive)
  (let ((km (make-keymap)))
    (set-keymap-parent km (current-local-map))
    (use-local-map km)))


(defmacro rloop (clauses &rest body)
  (if (null clauses)
      `(progn ,@body)
      `(loop ,@(car clauses) do (rloop ,(cdr clauses) ,@body))))

(defun all-bindings ()
  (interactive)
  (message "all-bindings: wait a few seconds please...")
  (let ((data
         (with-output-to-string
             (let ((bindings '()))
               (rloop ((for C in '("" "C-"))       ; Control
                       (for M in '("" "M-"))       ; Meta
                       (for A in '("" "A-"))       ; Alt
                       (for S in '("" "S-"))       ; Shift
                       (for H in '("" "H-"))       ; Hyper
                       (for s in '("" "s-"))       ; super
                       (for x from 32 to 127))
                      (let* ((k (format "%s%s%s%s%s%s%c" C M A S H s x))
                             (key (ignore-errors (read-kbd-macro k))))
                        (when key
                          (push
                           (list k
                                 (format "%-12s  %-12s  %S\n" k key
                                         (or
                                          ;; (string-key-binding key)
                                          ;; What is this string-key-binding?
                                          (key-binding key))))
                           bindings))))
               (dolist (item
                         (sort bindings
                               (lambda (a b)
                                 (or (< (length (first a))
                                        (length (first b)))
                                     (and (= (length (first a))
                                             (length (first b)))
                                          (string< (first a)
                                                   (first b)))))))
                 (princ (second item)))))))  
    (switch-to-buffer (format "Keybindings in %s" (buffer-name)))
    (erase-buffer)
    (insert data)
    (goto-char (point-min))
    (values)))


;;;----------------------------------------
;;; Properties:
;;;----------------------------------------

(defun plist-keys (plist)
  (if (null plist)
      plist
      (cons (car plist) (plist-keys (cddr plist)))))

(defun list-all-properties-in-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (delete-duplicates
     (loop
        for i from (point-min) to (point-max)
        nconc  (delete-duplicates (plist-keys (text-properties-at i nil)))))))

(defun remove-all-properties ()
  (interactive)
  (remove-list-of-text-properties
   (point-min)
   (point-max)
   (list-all-properties-in-buffer (current-buffer))))


;;;----------------------------------------
;;; Morse
;;;----------------------------------------

(require 'morse)
(defun morse-string (string)
  "Convert all text in a given string to morse code."
  (with-output-to-string
      (loop
         with sep = ""
         with morse = nil
         for ch across string do
         (cond
           ((is-space ch) (princ (format "%c" ch)))
           ((setq morse (assoc (string-upcase ch) morse-code))
            (princ sep)
            (princ (cdr morse))
            (setq sep "/"))
           (t
            (setq sep ""))))))


;;;----------------------------------------
;;; acronym
;;;----------------------------------------

(defun acronym ()
  (interactive)
  (w3m-browse-url
   (if (or (not mark-active) (eql (point) (mark)))
       (format "http://www.cygwin.com/acronyms/#%s"
               (read-from-minibuffer "Acronym: "))
       (buffer-substring-no-properties (min (point) (mark))
                                       (max (point) (mark))))))

;;;----------------------------------------
;;; eppelle
;;;----------------------------------------

(defun eppelle ()
  (interactive)
  (let ((text 
         (if (or (not mark-active) (eql (point) (mark)))
             (read-from-minibuffer "Word: ")
             (buffer-substring-no-properties (min (point) (mark))
                                             (max (point) (mark)))))
        (alphabet '((?A . "Alpha")
                    (?B . "Bravo")
                    (?C . "Charlie")
                    (?D . "Delta")
                    (?E . "Echo")
                    (?F . "Foxtrot")
                    (?G . "Golf")
                    (?H . "Hotel")
                    (?I . "India")
                    (?J . "Juliet")
                    (?K . "Kilo")
                    (?L . "Lima")
                    (?M . "Mike")
                    (?N . "November")
                    (?O . "Oscar")
                    (?P . "Papa")
                    (?Q . "Quebec")
                    (?R . "Romeo")
                    (?S . "Sierra")
                    (?T . "Tango")
                    (?U . "Uniform")
                    (?V . "Victor")
                    (?W . "Whiskey")
                    (?X . "X-ray")
                    (?Y . "Yankee")
                    (?Z . "Zulu")
                    (?0 . "Nadazero")
                    (?1 . "Unaone")
                    (?2 . "Bissotwo")
                    (?3 . "Terrathree")
                    (?4 . "Kartefour")
                    (?5 . "Pantafive")
                    (?6 . "Soxisix")
                    (?7 . "Setteseven")
                    (?8 . "Oktoeight")
                    (?9 . "Novenine")
                    (?А . "Алексей")
                    (?Б . "Борис")
                    (?В . "Василий")
                    (?Г . "Григорий")
                    (?Д . "Димитрий")
                    (?Е . "Елена")
                    (?Ж . "Женя")
                    ;;(?  . "Зоя")
                    (?И . "Иван")
                    (?Й . "Иван Каткий")
                    (?К . "Киловат")
                    (?Л . "Леонид")
                    (?М . "Мариа")
                    (?Н . "Николай")
                    (?О . "Олга")
                    (?П . "Павел")
                    (?Р . "Роман")
                    (?С . "Сергей")
                    (?Т . "Татяна")
                    (?У . "Уляна")
                    (?Ф . "Фёдор")
                    (?Х . "Харитон")
                    (?З . "Запля")
                    (?Ч . "Человек")
                    (?Ш . "Шура")
                    (?Щ . "Щука")
                    (?ъ . "Твирдиы Знак")
                    ;;(?  . "Igrek")
                    (?Ь . "Мягкиы Знак Znak")
                    (?э . "Эмиля")
                    (?Ю . "Юри")
                    (?Я . "Яков")

                    ;; Aleph Boaz Gimel David Hagar Vav Ze'ev Hava Tiach Yona
                    ;; Carmel Lea Moshe Nesher Samekh A'in Pesel Tsipor Korakh 
                    ;; Ruth Shamir Telem
                    ;; #+clisp
                    ;; (let ((codes '(("Aleph" "_ALEF")
                    ;;                ("Boaz" "_BET")
                    ;;                ("Gimel" "_GIMEL")
                    ;;                ("David" "_DALET")
                    ;;                ("Hagar" "_HE")
                    ;;                ("Vav" "_VAV")
                    ;;                ("Ze'ev" "_ZAYIN")
                    ;;                ("Hava" "_HET")
                    ;;                ("Tiach" "_TET")
                    ;;                ("Yona" "_YOD")
                    ;;                ("Carmel" "_KAF")
                    ;;                ("Lea" "_LAMED")
                    ;;                ("Moshe" "_MEM")
                    ;;                ("Nesher" "_NUN")
                    ;;                ("Samekh" "_SAMEKH")
                    ;;                ("A'in" "_AYIN")
                    ;;                ("Pesel" "_PE")
                    ;;                ("Tsipor" "_TSADI")
                    ;;                ("Korakh" "_QOF")
                    ;;                ("Ruth" "_RESH")
                    ;;                ("Shamir" "_SHIN")
                    ;;                ("Telem" "_TAV"))))
                    ;;   (dolist (ch (block nil
                    ;;                 (with-output-to-string (*standard-output*)
                    ;;                   (return (lschar :name "HEBREW")))))
                    ;;     (let* ((name (char-name ch))
                    ;;            (code (with-output-to-string (*standard-output*)
                    ;;                    (let ((sep ""))
                    ;;                      (dolist (code codes)
                    ;;                        (when (search (second code) name)
                    ;;                          (princ sep)
                    ;;                          (princ (first code))
                    ;;                          (setf sep " ")))))))
                    ;;       (when (string/= "" code)
                    ;;         (print (cons (intern (format nil "?~C" ch)) code))))))

                    (?ׁ . "Shamir") 
                    (?א . "Aleph") 
                    (?ב . "Boaz") 
                    (?ג . "Gimel") 
                    (?ד . "David") 
                    (?ה . "Hagar") 
                    (?ו . "Vav") 
                    (?ז . "Ze'ev") 
                    (?ח . "Hagar Hava") 
                    (?ט . "Tiach") 
                    (?י . "Yona") 
                    (?ך . "Carmel") 
                    (?כ . "Carmel") 
                    (?ל . "Lea") 
                    (?ם . "Moshe") 
                    (?מ . "Moshe") 
                    (?ן . "Nesher") 
                    (?נ . "Nesher") 
                    (?ס . "Samekh") 
                    (?ע . "A'in") 
                    (?ף . "Pesel") 
                    (?פ . "Pesel") 
                    (?ץ . "Tsipor") 
                    (?צ . "Tsipor") 
                    (?ק . "Korakh") 
                    (?ר . "Ruth") 
                    (?ש . "Shamir") 
                    (?ת . "Telem") 
                    (?װ . "Vav Vav") 
                    (?ױ . "Vav Yona") 
                    (?ײ . "Yona Yona") 
                    (?יִ . "Yona") 
                    (?ײַ . "Yona") 
                    (?ﬠ . "A'in") 
                    (?ﬡ . "Aleph") 
                    (?ﬢ . "David") 
                    (?ﬣ . "Hagar") 
                    (?ﬤ . "Carmel") 
                    (?ﬥ . "Lea") 
                    (?ﬦ . "Moshe") 
                    (?ﬧ . "Ruth") 
                    (?ﬨ . "Telem") 
                    (?שׁ . "Shamir") 
                    (?שׂ . "Shamir") 
                    (?שּׁ . "Shamir") 
                    (?שּׂ . "Shamir") 
                    (?אַ . "Aleph") 
                    (?אָ . "Aleph") 
                    (?אּ . "Aleph") 
                    (?בּ . "Boaz") 
                    (?גּ . "Gimel") 
                    (?דּ . "David") 
                    (?הּ . "Hagar") 
                    (?וּ . "Vav") 
                    (?זּ . "Ze'ev") 
                    (?טּ . "Tiach") 
                    (?יּ . "Yona") 
                    (?ךּ . "Carmel") 
                    (?כּ . "Carmel") 
                    (?לּ . "Lea") 
                    (?מּ . "Moshe") 
                    (?נּ . "Nesher") 
                    (?סּ . "Samekh") 
                    (?ףּ . "Pesel") 
                    (?פּ . "Pesel") 
                    (?צּ . "Tsipor") 
                    (?קּ . "Korakh") 
                    (?רּ . "Ruth") 
                    (?שּ . "Shamir") 
                    (?תּ . "Telem") 
                    (?וֹ . "Vav") 
                    (?בֿ . "Boaz") 
                    (?כֿ . "Carmel") 
                    (?פֿ . "Pesel") 
                    (?ﭏ . "Aleph Lea") 
                    )))
    (switch-to-buffer "*Eppelle*")
    (goto-char (point-max))
    (insert "\n========================================\n"
            text
            "\n----------------------------------------\n")
    (loop
       for ch across text
       for tr = (assoc* (upcase ch) alphabet :test (function =))
       do  (insert (if tr (format "%s " (cdr tr)) (format "%c" ch))))
    (insert "\n")))
    




;;;----------------------------------------
;;; cedet
;;;----------------------------------------
(defvar *pjb-load-noerror* nil)
(defvar *pjb-load-silent*  nil)

(defun compile-cedet ()
  (interactive)
  (require 'ede)
  (load "ede-proj.el" *pjb-load-noerror* *pjb-load-silent*)
  (provide 'ede-proj)
  (let ((default-directory "/usr/local/share/emacs/site-lisp/cedet/ede/")
        (compilation-ask-about-save nil))
    (save-excursion
      (condition-case ignore
          (funcall 'ede-compile-project)
        (error :error)))))

(defun compile-eieio ()
  (interactive)
  (when (file-exists-p
         "/usr/local/share/emacs/site-lisp/cedet/eieio/eieio.el")
    (if (file-newer-than-file-p
         "/usr/local/share/emacs/site-lisp/cedet/eieio/eieio.elc"
         "/usr/local/share/emacs/site-lisp/cedet/eieio/eieio.el")
        (message ".EMACS: eieio.elc is up to date.")
        (progn
          (switch-to-buffer (get-buffer-create "*Compilation of eieio*"))
          (delete-other-windows)
          (erase-buffer)
          (insert "Did you run first: M-x compile-cedet RET ?")
          (split-window-vertically 5)
          (require 'ede)
          (load "ede-proj.el" *pjb-load-noerror* *pjb-load-silent*)
          (provide 'ede-proj)
          (let ((default-directory
                 "/usr/local/share/emacs/site-lisp/cedet/eieio/")
                (compilation-ask-about-save nil))
            (save-excursion
              (condition-case ignore (ede-compile-project)
                (error :error))))))))



;;;----------------------------------------
;;; macros
;;;----------------------------------------

(defmacro* with-marker ((var position) &body body)
  (let ((vposition (gensym))) ; so (eq var position) still works.
    `(let* ((,vposition ,position)
            (,var (make-marker)))
       (set-marker ,var ,vposition)
       (unwind-protect (progn ,@body)
         (set-marker ,var nil)))))


(defmacro* dolines (start-end &body body)
  "Executes the body with start-var and end-var bound to the start and the end of each lines of the current buffer in turn."
  (let ((vline (gensym)))
    (destructuring-bind (start-var end-var) start-end
      `(let ((sm (make-marker))
             (em (make-marker)))
         (unwind-protect
              (progn
                (goto-char (point-min))
                (while (< (point) (point-max))
                  (let ((,vline (point)))
                    (set-marker sm (point))
                    (set-marker em (progn (end-of-line) (point)))
                    (let ((,start-var  (marker-position sm))
                          (,end-var    (marker-position em)))
                      ,@body)
                    (goto-char ,vline)
                    (forward-line 1))))
           (set-marker sm nil)
           (set-marker em nil))
         nil))))


(defmacro* with-file (file-and-options &body body)
  "
find-file or find-file-literally, process body, and optionally save the buffer
and kill it.
save is not done if body exits exceptionnaly.
kill is always done as specified.
FILE-AND-OPTION: either an atom evaluated to a path,
                 or (path &key (save t) (kill t) (literal nil))
"
  (if (atom file-and-options)
      `(with-file (,file-and-options) ,@body)
      ;; destructuring-bind is broken, we cannot give anything else than nil
      ;; as default values:
      (destructuring-bind (path &key (save nil savep) (kill nil killp)
                                (literal nil literalp))
          file-and-options
        (unless savep (setf save t))
        (unless killp (setf kill t))
        `(unwind-protect
                (progn
                  (,(if literal 'find-file-literally 'find-file) ,path)
                  (prog1 (save-excursion ,@body)
                         ,(when save `(save-buffer 1))))
           ,(when kill
                  `(kill-buffer (current-buffer)))))))


(defun constantly (value) (byte-compile `(lambda (&rest arguments) ',value)))

(defun mapfiles (thunk directory &optional recursive exceptions)
  "
THUNK:      a function of one argument called for each file pathname.
DIRECTORY:  the pathname of the base directory.
RECURSIVE:  a boolean indicating whether the directory will be walked recursively.
EXCEPTIONS: either a list of pathnames that musthn't be processed,
            or a predicate indicating the pathnames that mustn't be processed.
"
  (dolist (file (directory-files directory))
    (let ((stat (file-attributes file))
          (predicate (cond
                       ((null exceptions)
                        (constantly nil))
                       ((functionp exceptions)
                        exceptions)
                       ((listp exceptions)
                        (byte-compile `(lambda (x) (member* x ',exceptions :test (function string=)))))
                       (t (error "exceptions must be either a list or a function, not a ~S: ~S"
                                 (type-of exceptions) exceptions))))
          (path  (concat directory
                         (if (string= (subseq directory (1- (length directory)))
                                      "/")
                             "" "/")
                         file)))
      (case (first stat)
        ((t)                            ; directory
         (unless (or (string= "." file) (string= ".." file))
           (when recursive
             (unless (funcall predicate path)
               (mapfiles thunk path recursive predicate)))))
        ((nil) ; file
         (unless (funcall predicate path)
          (funcall thunk path)))
        (otherwise ; symlink
         ;; NOP
         )))))


(defmacro* with-files ((file-var directory-expr &optional recursive exceptions) &body body)
  `(mapfiles (lambda (,file-var) ,@body) ,directory-expr ,recursive ,exceptions))



;;;----------------------------------------
;;; multi-file replace
;;;----------------------------------------

(defvar *recursive-replace-ignored-directories*
  '("_darcs" ".darcsrepo" ".svn" ".hg" ".git" "CVS" "RCS" "MT" "SCCS"
    ".tmp_versions" "{arch}" ".arch-ids"
    "BitKeeper" "ChangeSet" "autom4te.cache"))


(defun recursive-replace-string (from-string to-string &optional directory recursive delimited)
  "Replace the all occurences of `from-string' by `to-string' in all the files in the directory.
If `recursive' is true (or a prefix argument is given), then the files are searched recursively
otherwise only the files directly in the given `directory' are modified.
`*recursive-replace-ignored-directories*' is a list of directory names that are excluded from the
recursive search.  Backup files (name ending in ~) are ignored too.
`delimited', if non-nil, means replace only matches surrounded by word boundaries.
 "
  (interactive
   (let* ((directory (symbol-name (read-minibuffer "Directory: " default-directory)))
          (arguments (query-replace-read-args
                      (format "Replace string in all files in %s" directory)
                      nil)))
     (list (first arguments) (second arguments) directory (third arguments) nil)))
  (with-files (file directory recursive
                    (lambda (path)
                      (let ((name (basename path)))
                        (or (string= "~" (subseq name (1- (length name))))
                            (member* name *recursive-replace-ignored-directories*
                                     :test (function string=))))))
    (with-file (file)
      (message "Processing %S" file)
      (beginning-of-buffer)
      (replace-string from-string to-string delimited))))


(defun recursive-replace-regexp (regexp to-string &optional directory recursive delimited)
  "Replace the all occurences of `regexp' by `to-string' in all the files in the directory.
If `recursive' is true (or a prefix argument is given), then the files are searched recursively
otherwise only the files directly in the given `directory' are modified.
`*recursive-replace-ignored-directories*' is a list of directory names that are excluded from the
recursive search.  Backup files (name ending in ~) are ignored too.
`delimited', if non-nil, means replace only matches surrounded by word boundaries.
 "
  (interactive
   (let* ((directory (symbol-name (read-minibuffer "Directory: " default-directory)))
          (arguments (query-replace-read-args
                      (format "Replace string in all files in %s" directory)
                      nil)))
     (list (first arguments) (second arguments) directory (third arguments) nil)))
  (with-files (file directory recursive
                    (lambda (path)
                      (let ((name (basename path)))
                        (or (string= "~" (subseq name (1- (length name))))
                            (member* name *recursive-replace-ignored-directories*
                                     :test (function string=))))))
    (with-file (file)
      (message "Processing %S" file)
      (beginning-of-buffer)
      (replace-regexp regexp to-string delimited))))


(defun multifile-replace-regexp (regexp to-string files &optional delimited)
  "Replace the all occurences of `regexp' by `to-string' in all the `files'.
`delimited', if non-nil, means replace only matches surrounded by word boundaries.
 "
  (dolist (file files)
    (with-file (file :save t :kill nil)
      (message "Processing %S" file)
      (beginning-of-buffer)
      (replace-regexp regexp to-string delimited))))


;;;: THE END ;;;;
