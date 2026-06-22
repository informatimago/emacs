;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;****************************************************************************
;;;;FILE:               pjb-shell.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    shell/nshell defined as functions instead of advices.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-06-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2004 - 2011
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

(require 'shell)
(provide 'pjb-shell)

(defvar original-shell (symbol-function 'shell))

(defun shell-buffer-name (i) (format "%dshell" i))

(defun pjb-shell--multiple-buffers (orig-fun &optional buffer)
  "Create a new shell when none exist, or switch to the last created one.
Around-advice for `shell'."
  (interactive "P")
  (if buffer
      (funcall orig-fun buffer)
    ;; find *a* shell:
    (let ((i 0))
      (while (get-buffer (shell-buffer-name i))
        (setq i (1+ i)))
      (if (= 0 i)
          (progn  ;; no shell, let's make the first one
            (funcall orig-fun nil)
            (rename-buffer (shell-buffer-name 0)))
        ;; already have some shells, let's jump to one
        (switch-to-buffer (shell-buffer-name (1- i)))))))
(advice-add 'shell :around #'pjb-shell--multiple-buffers)


(defvar *forward-control-c-shells*
  '("ssh$"))

(defun nshell (ask-for-shell-p)
  "Create a new shell."
  (interactive "P")
  (flet ((new-shell ()
           (let ((i 0))
             (while (get-buffer (shell-buffer-name i))
                (setq i (1+ i)))
             (switch-to-buffer (get-buffer-create (shell-buffer-name i)))
             (shell (current-buffer)))))
   (if ask-for-shell-p
       (let* ((command (read-from-minibuffer
                        "Command: " "/bin/bash" nil nil nil
                        "/bin/bash"))
              (args (split-string command "[ \t]+"))
              (explicit-shell-file-name (first args))
              (pname (file-name-nondirectory explicit-shell-file-name))
              (xargs-name (intern (concat "explicit-" pname "-args"))))
         (eval `(let ((,xargs-name ',(rest args)))
                  (message "pgm=%S args=%S"
                           explicit-shell-file-name
                           ,xargs-name)
                  (new-shell)))
         (message "command=%S f=%S member=%S"
                  (first args) *forward-control-c-shells*
                  (member* (first args) *forward-control-c-shells*
                           :test (lambda (string regexp) (string-match regexp string))))
         (when (member* (first args) *forward-control-c-shells*
                        :test (lambda (string regexp) (string-match regexp string)))
           (make-keymap-local)
           (local-set-key (kbd "C-c C-c")
                          (lambda ()
                            (interactive)
                            (comint-send-string
                             (get-buffer-process (current-buffer))
                             (format "%c" 3))))))
       (new-shell))))


(defconst pjb-comint-color-query-regexp
  "\e\\][01][01];\\?\\(\e\\\\\\|\a\\)"
  "Matches an OSC 10/11 foreground/background color query.
The bytes are ESC ] 1 0|1 ; ? ST, where ST is ESC \\ or BEL.")

(defun pjb-comint-strip-color-queries (string)
  "Strip OSC 10/11 foreground/background color queries from STRING.

A program (vim, htop, ...) may ask the terminal for its fg/bg colors with
an OSC 10/11 query.  A comint `shell' buffer is a line editor, not a
terminal emulator, so there is nobody to answer.

This used to *reply* with `process-send-string', but a `shell' buffer has
only one input channel: the shell's own stdin.  At the prompt that reply
is therefore fed to the shell as if typed, yielding `command not found'
noise (the OSC bytes), spurious `> ' PS2 continuations, and a corrupted
current command line.  So we just drop the queries, keeping the buffer
clean and leaving stdin untouched; programs that need a real answer should
run under a terminal emulator such as `M-x ansi-term'."
  (replace-regexp-in-string pjb-comint-color-query-regexp "" string))

(define-obsolete-function-alias 'pjb-comint-answer-color-queries
  'pjb-comint-strip-color-queries "2026-06"
  "Renamed: it now strips the queries instead of injecting a reply into stdin.")

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-preoutput-filter-functions
                      #'pjb-comint-strip-color-queries nil t)
            (ansi-color-for-comint-mode-on)))


;;;; THE END ;;;;
