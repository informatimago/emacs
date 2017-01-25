;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-too-early.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Evaluate things too early.
;;;;
;;;;    Works only when paredit-mode is active, and with
;;;;    emacs-lisp-mode or lisp-mode+slime-mode
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    gnu affero general public license for more details.
;;;;
;;;;    you should have received a copy of the gnu affero general public license
;;;;    along with this program.  if not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(require 'cl)
(require 'lisp-mode)
(require 'paredit)
(require 'slime)

(define-modify-macro too-early-appendf (&rest args) append "Append onto list")


(defun too-early-prefixp (prefix string)
  "
PREFIX:  A sequence.
STRING:  A sequence.
RETURN:  Whether PREFIX is a prefix of the STRING.
"
  (string= prefix (subseq string 0 (min (length string) (length prefix)))))

(defun too-early-suffixp (suffix string)
  "
PREFIX:  A sequence.
STRING:  A sequence.
RETURN:  Whether PREFIX is a prefix of the STRING.
"
  (string= suffix (subseq string (max 0 (- (length string) (length suffix))))))


;; set-window-dedicated-p window flag

(defstruct (too-early-form
             (:constructor %make-too-early-form))
  package   ; last in-package form.
  operator  ; defpackage, defmacro, defun, defvar, defparameter, def*, define-*
  name
  start     ; marker
  end       ; marker
  )

(defun new-marker (point &optional buffer)
  (let ((marker (make-marker)))
    (set-marker marker point (or buffer (current-buffer)))))

(defun* make-too-early-form (&key package operator name start end)
  (%make-too-early-form
   :package package
   :operator operator
   :name name
   :start (new-marker start)
   :end (new-marker end)))


(defun too-early-make-index ()
  "Returns a too-early index from the current lisp source buffer.
A too-early index is a list of too-early-form"
  (save-excursion
    (let ((index '())
          (last-in-package)
          (*map-sexps-deeply*    nil)
          (*map-sexps-atoms*     nil)
          (*map-sexps-top-level* t))
      (goto-char (point-min))
      (walk-sexps (lambda (sexp start end)
                    (when (and (listp sexp)
                               (symbolp (first sexp))
                               (or (string-equal "in-package" (symbol-name (first sexp)))
                                   (too-early-suffixp ":in-package" (symbol-name (first sexp)))))
                      (setf last-in-package sexp))
                    (push (make-too-early-form
                           :package last-in-package
                           :operator (when (listp sexp) (first sexp))
                           :name (when (listp sexp) (second sexp))
                           :start start
                           :end end)
                          index)))
      index)))



(defvar *too-early-definitions*
  nil
  "A list of `too-early-form's.")

(setf *too-early-definitions* (too-early-make-index))



(defstruct (too-early-frame
             (:constructor %make-too-early-frame))
  name
  frame
  show-menu-p
  menu-buffer   menu-window
  main-buffer   main-window
  child-buffers child-windows)


(defun too-early-frame-menu-width (teframe)
  (declare (ignore teframe))
  32)


(defun too-early-frame-split-menu-window (teframe)
  (with-selected-window (too-early-frame-main-window teframe)
    (let ((menu-window (selected-window)))
      (split-window-horizontally (too-early-frame-menu-width teframe))
      (set-window-dedicated-p menu-window nil)
      (set-window-buffer menu-window (too-early-frame-menu-buffer teframe))
      (set-window-dedicated-p menu-window t)
      (setf (too-early-frame-menu-window teframe) menu-window)
      (other-window 1 nil)
      (set-window-dedicated-p (selected-window) t))))


(defun too-early-frame-split-child-window (teframe child-buffer)
  (flet ((set-child-window ()
           (other-window 1 nil)
           (set-window-dedicated-p (selected-window) nil)
           (set-window-buffer      (selected-window) child-buffer)
           (set-window-dedicated-p (selected-window) t)
           (too-early-appendf (too-early-frame-child-windows teframe) (list (selected-window)))))
    (if (too-early-frame-child-windows teframe)
        (with-selected-window (first (last (too-early-frame-child-windows teframe)))
          (split-window-vertically)
          (set-child-window))
        (with-selected-window (too-early-frame-main-window teframe)
          (split-window-horizontally (/ (window-width) 2))
          (set-child-window)))))


(defun too-early-setup-windows (teframe)
  (with-selected-frame (too-early-frame-frame teframe)
    (delete-other-windows)
    (let ((main-window (selected-window)))
      (set-window-dedicated-p main-window nil)
      (set-window-buffer main-window (too-early-frame-main-buffer teframe))
      (set-window-dedicated-p main-window t)
      (setf (too-early-frame-main-window teframe) main-window))
    (when (too-early-frame-show-menu-p teframe)
      (too-early-frame-split-menu-window teframe))
    (dolist (child (too-early-frame-child-buffers teframe))
      (too-early-frame-split-child-window teframe child))))


(defun too-early-frame-menu-buffer-name (teframe)
  (format "*Menu %s*" (too-early-frame-name teframe)))

(defun too-early-frame-main-buffer-name (teframe)
  (format "*Main %s*" (too-early-frame-name teframe)))

(defun too-early-frame-child-buffer-name (teframe child-name)
  (format "*Child %s / %s*" (too-early-frame-name teframe) child-name))

(defun too-early-setup-buffers (teframe)
  (setf (too-early-frame-main-buffer teframe)
        (get-buffer-create (too-early-frame-name teframe))
        (too-early-frame-menu-buffer teframe)
        (get-buffer-create (too-early-frame-menu-buffer-name teframe))))


(defun make-too-early-frame-on-frame (name frame)
  (let ((teframe (%make-too-early-frame :name name :frame frame)))
    (with-selected-frame frame (set-frame-name name))
    (too-early-setup-buffers teframe)
    (too-early-setup-windows teframe)
    teframe))


(defun make-too-early-frame-on-display (name display &optional parameters)
  (make-too-early-frame-on-frame name (make-frame-on-display display parameters)))

(defun make-too-early-frame (name &optional parameters)
  (make-too-early-frame-on-frame name (make-frame parameters)))


(defun too-early-frame-hide-menu (teframe)
  (when (too-early-frame-menu-window teframe)
    (ignore-errors (delete-window (too-early-frame-menu-window teframe)))
    (setf (too-early-frame-menu-window teframe) nil
          (too-early-frame-show-menu-p teframe) nil)))


(defun too-early-frame-show-menu (teframe)
  (too-early-frame-hide-menu teframe)
  (setf (too-early-frame-show-menu-p teframe) t)
  (too-early-frame-split-menu-window teframe))


(defun too-early-frame-add-child (teframe child-name)
  (too-early-frame-split-child-window
   teframe
   (get-buffer-create (too-early-frame-child-buffer-name teframe child-name))))

(setf *teframe* (make-too-early-frame "Test"))
(too-early-frame-show-menu *teframe*)
(too-early-frame-add-child *teframe* "child-1")
(first (last (too-early-frame-child-windows *teframe*)))



(defvar too-early-mode-hook '()
  "too early mode hook.")



(define-minor-mode too-early-mode
    "Processes lisp source buffers and helps programming and debugging."
  nil
  " TE"
  too-early-keymap
  :group 'too-early
  :global nil
  (progn))



(defvar *too-early-last-sexp* nil
  "the last sexp processed by too-early.")

(defun too-early/emacs-lisp (text)
  (insert (format "emacs-lisp\n%s\n" text))
  )


(defun too-early/common-lisp (text)
  (insert (format "common-lisp\n%s\n" text))
  )


(defun synchronize-cursor-position (buffer)
  "move the window-point in each window on this buffer to the point."
  (dolist (window (window-list))
    (when (eq (window-buffer window) (current-buffer))
      ;; we need to use both to get the effect.
      (set-window-point window (point))
      (end-of-buffer))))



(defvar *too-early-window-width* 64
  "*window width for the *too-early* buffer.")

(defvar *too-early-modes*
  '((emacs-lisp-mode (paredit-mode)            too-early/emacs-lisp)
    (lisp-mode       (paredit-mode slime-mode) too-early/common-lisp))
  "an a-list mapping major modes to a list whose first element is a
list of minor modes that must be active along with the major mode,
and whose second element is the too-early function.")


(defun too-early-function ()
  "return the too-early function corresponding to the current modes"
  (let ((entry (assoc major-mode *too-early-modes*)))
    (and entry
         (every (function symbol-value) (second entry))
         (third entry))))


(defun too-early-meat ()
  "too early meat, called in the post-command-hook.
see: `toggle-too-early'."
  (message "command = %s" this-command)
  (unless (member this-command '(digit-arguments
                                 universal-argument-other-key))
    ;; (eq this-command 'self-insert-command)
    (let ((deactivate-mark deactivate-mark)
          (too-early-function (too-early-function)))
      (when too-early-function
        (save-excursion
          (when (beginning-of-defun)
            (let ((pt (point)))
              (forward-sexp)
              (let ((text (buffer-substring-no-properties pt (point))))
                (with-current-buffer (get-buffer-create "*too-early*")
                  (funcall too-early-function text)
                  (synchronize-cursor-position (current-buffer)))))))))))



(defun too-early-show-window ()
  "Show the too-early window."
  (delete-other-windows)
  (split-window nil (- (window-width) *too-early-window-width*) t)
  (other-window 1)
  (switch-to-buffer (get-buffer-create "*too-early*"))
  (set-window-dedicated-p (selected-window) t)
  (other-window 1))

(defun too-early-hide-window ()
  "Hide the too-early window."
  (dolist (window (window-list))
    (when (eq (window-buffer window) (get-buffer "*too-early*"))
      (delete-window window))))

(defun too-early-activate ()
  "Use `toggle-too-early'."
  (too-early-show-window))

(defun too-early-deactivate ()
  "Use `toggle-too-early'."
  (too-early-hide-window))




(defun too-early-active-p/post-command-hook ()
  (member 'too-early-meat  post-command-hook))

(defun toggle-too-early/post-command-hook ()
  ;; This doesn't work, it makes C-l jump all over the place, same as
  ;; C-p or C-n.
  (interactive)
  (if (too-early-active-p/post-command-hook)
      (progn
        (remove-hook 'post-command-hook 'too-early-meat)
        (too-early-deactivate))
      (progn
        (add-hook 'post-command-hook 'too-early-meat)
        (too-early-activate))))



(defun *too-early-timer* nil)

(defun too-early-active-p/idle-timer ()
  *too-early-timer*)

(defun toggle-too-early/idle-timer ()
  (interactive)
  (if (too-early-active-p/idle-timer)
      (progn
        (cancel-timer *too-early-timer*)
        (setf *too-early-timer* nil)
        (too-early-deactivate))
      (progn
        (setf *too-early-timer* (run-with-idle-timer 1 t (function too-early-meat)))
        (too-early-activate))))



(defun too-early-active-p ()
  (toggle-too-early/idle-timer))

(defun toggle-too-early ()
  (interactive)
  (toggle-too-early/idle-timer))


(defun too-early (activate)
  (interactive "p")
  (unless (eq (not (plusp activate))
              (not (too-early-active-p)))
    (toggle-too-early)))


(provide 'too-early)
;;;; THE END ;;;;

