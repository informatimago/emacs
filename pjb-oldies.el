;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-oldies.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This mode combines paper-mode, caps-mode and upcase-lisp and
;;;;    downcase-lisp in find-file-hook and
;;;;    before-save-hook/after-save-hook to let the user work with
;;;;    uppercase like in old times.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <spam@thalassa.informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-09-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2004 - 202021
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************


(defconst oldies:+computer-paper-colors+  '("azure" "PaleTurquoise" "LightCyan1"
                                            "LightBlue"  "LightCyan" "PowderBlue"))


(defun oldies:delete-all-overlays ()
  (interactive)
  (mapc (lambda (item)
          (if (listp item)
              (mapc (function delete-overlay) item)
              (delete-overlay item)))
        (overlay-lists)))


(defun oldies:put-computer-paper-overlay (modulo block)
  (oldies:delete-all-overlays)
  (goto-char (point-min))
  (let ((backf (make-vector oldies:+computer-paper-colors+ nil))
        (line 0)
        (i   -1))
    (dolist (color oldies:+computer-paper-colors+)
      (setf (aref backf (incf i))
            (let* ((facesym (intern (concatenate 'string color "-face")))
                   (face (make-face facesym)))
              (copy-face 'default face)
              (set-face-foreground face "black")
              (set-face-background face color)
              face)))
    (while (< (point) (point-max))
      (let ((extent (make-overlay (progn (beginning-of-line) (point))
                                  (progn (forward-line block)
                                         (beginning-of-line) (point)))))
        (overlay-put extent 'evaporate t)
        (overlay-put extent 'face (aref backf (mod line modulo)))
        (incf line)))))


(defun oldies:computer-paper ()
  (interactive)
  (let (modulo
        block
        (ncolors (length oldies:+computer-paper-colors+))
        (cond
          ((integerp current-prefix-arg)
           (setf modulo (min current-prefix-arg ncolors)
                 block  1))
          ((null     current-prefix-arg)
           (setf modulo (min 2 ncolors)
                 block  1))
          ((consp    current-prefix-arg)
           (setf modulo (min (read-minibuffer "Modulo: " "2") ncolors)
                 block  (read-minibuffer "Block: " "1")))
          (t (error "Invalid prefix %S" current-prefix-arg)))
        (assert (<= 1 block))
        (assert (and (<= 2 modulo) (<= modulo ncolors)))
        (oldies:put-computer-paper-overlay modulo block)))



  (defvar oldies:*line-length* 132) ; or 80

  (defun oldies:complete-line ()
    (interactive)
    (let ((length (- (progn (end-of-line) (point))
                     (progn (beginning-of-line) (point)))))
      (cond
        ((< length oldies:*line-length*)
         (end-of-line)
         (insert (make-string (- oldies:*line-length* length) 32))
         (next-line))
        ((> length oldies:*line-length*)
         (beginning-of-line)
         (forward-char oldies:*line-length*)
         (insert "\n")))))

  (defun oldies:complete-lines-in-region (start end)
    (interactive "r")
    (let ((endm (make-marker)))
      (set-marker endm end)
      (goto-char start)
      (while (< (point) endm)
        (oldies:complete-line))))

  (defun oldies:complete-lines-of-buffer ()
    (interactive)
    (oldies:complete-lines-in-region (point-min) (point-max)))





  (defun skip-to-next-sexp ()
    (interactive)
    (while (or
            (looking-at "\\([ \n\t\v\f\r]+\\)") ;  spaces
            (looking-at "\\(;.*$\\)")           ;  ;xxx      comment
            (looking-at "\\(#|\\([^|]\\||[^#]\\)*|#\\)")) ;  #|xxx|#   comment
      (goto-char (match-end 0))))

  (defun cl-looking-at-what ()
    (cond
      ((looking-at "[ \n\t\v\f\r]") :space)
      ((looking-at ";")  :semicolon-comment) ; ;xxx
      ((looking-at "#|") :sharp-comment)     ;  #|xxx|#
      ((looking-at "\"") :string)            ; "xx\"x"
      ((looking-at "(")  :beginning-of-list)
      ((looking-at ")")  :end-of-list)
      ((looking-at ",@") :comma-at)
      ((looking-at ",")  :comma)
      ((looking-at "'")  :quote)
      ((looking-at "`")  :backquote)
      (t                 :atom)))


  (defun cl-skip-over-sharp-comment ()
    (let ((start (match-beginning 0)))
      (goto-char (match-end 0))
      (loop named :search do
        (re-search-forward "\\(#|\\||#\\)")
        (if (string= "#|" (match-string 0))
            (progn
              (cl-skip-over-sharp-comment)
              (goto-char (match-end 0)))
            (let ((end (match-end 0)))
              (set-match-data (list start (point)))
              (return-from :search))))))

  (defun cl-skip-over (&optional what)
    (setf what (or what (cl-looking-at-what)))
    (case what
      ((:space)             (looking-at "[ \n\t\v\f\r]+"))
      ((:semicolon-comment) (looking-at ";.*$"))
      ((:sharp-comment)     (when (looking-at "#|")
                              (cl-skip-over-sharp-comment)
                              t))
      ((:string)            (looking-at "\"\\(\\(\\|\\\\.\\|\\\\\n\\)[^\\\\\"]*\\)*\""))
      ((:beginning-of-list) (looking-at "("))
      ((:end-of-list)       (looking-at ")"))
      ((:quote)             (looking-at "'"))
      ((:backquote)         (looking-at "`"))
      ((:comma)             (looking-at ","))
      ((:comma-at)          (looking-at ",@"))
      ((:atom)              (looking-at
                             "\\(|[^|]*|\\|\\\\.\\|#[^|]\\|[^\"\\#|;()'`, \n\t\v\f\r\\]\\)+"))
      (otherwise (error "cannot skip over %s" what)))
    (goto-char (match-end 0)))


  (defun cl-forward  (&optional n)
    (interactive "p")
    (setf n (or n 1))
    (dotimes (i n t)
      (cl-skip-over)))


  (defun cl-what-is-at-point ()
    (interactive)
    (message "%s" (cl-looking-at-what)))


  (defun case-lisp-region (start end transform)
    "
do:      applies transform on all subregions from start to end that are not
         a quoted character, a quote symbol, a comment (;... or #|...|#),
         or a string.
"
    (save-excursion
     (goto-char start)
     (while (< (point) end)
       (while (and (< (point) end)
                   (or (looking-at "[^\"#|;\\\\]+")
                       (and (looking-at "#")
                            (not (looking-at "#|")))))
         (goto-char (match-end 0)))
       (funcall transform start (min end (point)))
       (cl-skip-over)
       (setq start (point)))))

  
  (defun oldies:upcase-lisp-region (start end)
    "
DO:      From the start to end, converts to upcase all symbols.
         Does not touch string literals, comments starting with ';' and
         symbols quoted with '|' or with '\\'.
"
    (interactive "*r")
    (oldies: case-lisp-region start end (function upcase-region))
    (message "Upcase LISP Done."))



  

  (defun upcase-lisp ()
    "
DO:      From the (point) to (point-max), converts to upcase all symbols.
         Does not touch string literals, comments starting with ';' and
         symbols quoted with '|' or with '\\'.
"
    (interactive "*")
    (upcase-lisp-region (point) (point-max)))


  (defun downcase-lisp-region (start end)
    "
DO:      From the start to end, converts to low-case all symbols.
         Does not touch string literals, comments starting with ';' and
         symbols quoted with '|' or with '\\'.
"
    (interactive "*r")
    (case-lisp-region start end (function downcase-region))
    (message "Downcase LISP Done."))


  (defun downcase-lisp ()
    "
DO:      From the (point) to (point-max), converts to lowcase all symbols.
         Does not touch string literals, comments starting with ';' and
         symbols quoted with '|' or with '\\'.
"
    (interactive "*")
    (downcase-lisp-region (point) (point-max)))



  (defun caps-mode-self-insert-command (&optional n)
    "Like `self-insert-command', but uppercase the the typed character."
    (interactive "p")
    (insert-char (upcase last-command-event) n))

  (defvar *caps-mode-map* nil)

  (when (fboundp 'define-minor-mode)
    (define-minor-mode oldies-mode
      "Toggle oldies mode
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
      :init-value nil
      :lighter " Old"

      ;; Caps Mode:
      (setq *caps-mode-map*
            (let ((map (make-sparse-keymap)))
              (substitute-key-definition 'self-insert-command
                                         'caps-mode-self-insert-command
                                         map global-map)
              map))
      (if oldies-mode
          (add-to-list 'minor-mode-map-alist (cons 'oldies-mode *caps-mode-map*))
          (setq minor-mode-map-alist
                (delete (assoc 'oldies-mode minor-mode-map-alist)
                        minor-mode-map-alist)))

      (computer-paper)))

;;;; THE END ;;;;
