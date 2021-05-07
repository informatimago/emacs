;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-page.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    View a buffer page by page.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-06-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2005 - 2011
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

(require 'pjb-cl)

(defvar *saved-scroll-functions* nil)
(make-local-variable '*saved-scroll-functions*)

(defparameter *page-mode-bindings*
  '((pm-backward-page       scroll-down         "<prior>" "C-c p p")
    (pm-forward-page        scroll-up           "<next>"  "C-c p n")
    (pm-beginning-of-buffer beginning-of-buffer "<home>"  "C-c p a" "C-c p <")
    (pm-end-of-buffer       end-of-buffer       "<end>"   "C-c p e" "C-c p >")))

(defun pjb-reset-page-mode-key-bindings ()
  (loop for (new-fun old-fun . keys) in *page-mode-bindings*
        do (loop for key in keys
                 do (local-set-key (kbd key) new-fun))))

(defun pjb-set-page-mode-key-bindings (&optional on)
  (if (if on (plusp on) (not *saved-scroll-functions*))
      (progn
        (unless *saved-scroll-functions*
          (setf *saved-scroll-functions*
                (loop for (new-fun old-fun . keys) in *page-mode-bindings*
                      append (loop for key in keys
                                   collect (list (kbd key) (key-binding (kbd key)))))))
        (pjb-reset-page-mode-key-bindings)
        (pjb-narrow-to-page))
      (progn
        (widen)
        (beginning-of-buffer)
        (if *saved-scroll-functions*
            (progn
              (mapc (lambda (args) (apply (function local-set-key) args))
                    *saved-scroll-functions*)
              (setf *saved-scroll-functions* nil))
            (loop for (new-fun old-fun . keys) in *page-mode-bindings*
                  do (loop for key in keys
                           do (local-set-key (kbd key) old-fun)))))))

(defun page-mode (&optional on)
  (interactive "p")
  (pjb-set-page-mode-key-bindings on)
  (unless on
    (normal-mode t)))

(defun pjb-narrow-to-page (&optional arg)
  (interactive)
  (narrow-to-page arg)
  (normal-mode t)
  (pjb-reset-page-mode-key-bindings))

(defun pm-forward-page (&optional count)
  (interactive "p")
  (setf count (or count 1))
  (widen)
  (unless (search-forward "\f" nil 'at-limit count)
    (goto-char (point-max)))
  (pjb-narrow-to-page))


(defun pm-backward-page (&optional count)
  (interactive "p")
  (setf count (or count 1))
  (widen)
  (unless (search-backward "\f" nil 'at-limit (1+ count))
    (goto-char (point-min)))
  (pjb-narrow-to-page))


(defun pm-beginning-of-buffer ()
  (interactive)
  (widen)
  (goto-char (point-min))
  (pjb-narrow-to-page))


(defun pm-end-of-buffer ()
  (interactive)
  (widen)
  (goto-char (point-max))
  (pjb-narrow-to-page))



(defun list-all-major-modes ()
  "Well, all the major modes we can find.
We only include the symbols that are loaded.
"
  ;; There's no definite list, notably if you want potential major modes
  ;; that have not been loaded or installed yet.  There's a
  ;; minor-mode-list but there is no major-mode-list.
  (let ((major-modes '()))
    (do-symbols (symbol)
      (when (let ((name (symbol-name symbol)))
              (and (< 5 (length name))
                   (string= "-mode" (subseq name (- (length name) 5)))
                   (not (member symbol minor-mode-list))
                   (fboundp symbol)
                   (not (macrop symbol))))
        (push symbol major-modes)))
    major-modes))

(defun pm-insert-page (new-major-mode)
  "Inserts a new page at point."
  (interactive (intern (completing-read "New mode: "
                                        (sort (mapcar (function symbol-name)
                                                      (list-all-major-modes))
                                              (function string<))
                                        nil
                                        :require-match
                                        (cons (symbol-name major-mode)
                                              0))))
  (let ((new-page (format "%c\n" 12)))
    (insert new-page "\n" new-page)
    (backward-char 3)
    (pjb-narrow-to-page)
    (handler-case (funcall new-major-mode 1)
      (error () (funcall new-major-mode)))
    (insert (format "%s -*- mode:%s -*- %s\n\n"
                    (if (member new-major-mode
                                '(lisp-mode
                                  emacs-lisp-mode))
                        ";;"
                        comment-start)
                    new-major-mode
                    comment-end))))


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
      (force-mode-line-update t))
    (message "Done.")))

;;;; THE END ;;;;

