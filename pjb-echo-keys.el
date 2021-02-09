;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-echo-keys.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Echo emacs key chords as they are typed.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-02-09 <PJB> By suggestion of quiliro@irc.freenode.org, added
;;;;                     the option of displaying repeated command count.
;;;;    2014-06-05 <PJB> Extracted from ~/.emacs.
;;;;BUGS
;;;;    We should define a customization for *echo-key-coallesce-repeats*.
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2021
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

(defvar *echo-keys-last* nil "Last command processed by `echo-keys'.")
(defvar *echo-keys-count* 0 "Number of times the `*echo-keys-last*` command was repeated.")
(defvar *echo-key-width*  40 "Default width of the *echo-keys* window.")
(defvar *echo-key-password-disable* nil "Temporarily disable echo key for password input.")

(defvar *echo-key-coallesce-repeats* nil
  "When true, displays <key> <command> [N times] instead of n lines <key> <commands>.")


(defun echo-keys ()
  (let ((deactivate-mark deactivate-mark)
        (keys            (this-command-keys)))
    (when (and keys (not *echo-key-password-disable*))
      (save-excursion
       (with-current-buffer (get-buffer-create "*echo-key*")
         (goto-char (point-max))
         (if (eql this-command 'self-insert-command)
             (let ((desc (key-description keys)))
               (if (= 1 (length desc))
                   (insert desc)
                   (insert " " desc " "))
               (setf *echo-keys-last* this-command
                     *echo-keys-count* 1))
             (if (and *echo-key-coallesce-repeats*
                      (eql *echo-keys-last* this-command))
                 (progn
                   (incf *echo-keys-count*)
                   ;; update the last line
                   (forward-line -1)
                   (if (= 2 *echo-keys-count*)
                       (progn
                         (end-of-line)
                         (insert (format " [%d times]" *echo-keys-count*)))
                       (save-match-data
                        (when (re-search-forward " \\[\\([0-9]+\\) times\\]" nil t)
                          (delete-region (match-beginning 1) (match-end 1))
                          (goto-char (match-beginning 1))
                          (insert (format "%d" *echo-keys-count*)))))
                   (forward-line 1))
                 (progn
                   (insert (if (eq 'self-insert-command *echo-keys-last*)
                               "\n"
                               "")
                           (format "%-12s %s\n"
                                   (key-description keys)
                                   this-command))
                   (setf *echo-keys-last* this-command
                         *echo-keys-count* 1))))
         (dolist (window (window-list))
           (when (eq (window-buffer window) (current-buffer))
             (with-selected-window window
               ;; We need to use both to get the effect.
               (set-window-point window (point))
               (end-of-buffer)))))))))


(defun toggle-echo-keys ()
  "Toggle displaying the *echo-key* buffer."
  (interactive)
  (if (member 'echo-keys (default-value 'pre-command-hook))
      (let ((echo-buffer (get-buffer "*echo-key*")))
        (remove-hook 'pre-command-hook 'echo-keys)
        (dolist (window (window-list))
          (when (eq (window-buffer window) echo-buffer)
            (delete-window window))))
      (progn
        (delete-other-windows)
        (split-window nil (- (window-width) *echo-key-width*) t)
        (other-window 1)
        (switch-to-buffer (get-buffer-create "*echo-key*"))
        (toggle-truncate-lines +1)
        (set-window-dedicated-p (selected-window) t)
        (other-window 1)
        (add-hook 'pre-command-hook 'echo-keys))))

(defadvice echo-key--read-passwd--disable (before read-passwd)
  (message "echo-key--read-passwd--disable")
  (setf *echo-key-password-disable* t))

(defadvice echo-key--read-passwd--enable (after read-passwd)
    (message "echo-key--read-passwd--enable")
  (setf *echo-key-password-disable* nil))

(provide 'pjb-echo-keys)
;;;; THE END ;;;;
