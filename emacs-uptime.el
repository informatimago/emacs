;;;----------------------------------------------------------------------------
;;; emacs-uptime.el
;;;
;;; Copyright (C) 1998, 2000, 2002, 2004, 2007, 2008 Thien-Thi Nguyen
;;;
;;; This file is part of ttn's personal elisp library, released under
;;; the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.  There is NO WARRANTY.  See file COPYING for details.

;;; Description: Give Emacs' uptime and some other stats in the modeline.

(defvar *emacs-start-time*   (current-time) "For (emacs-uptime);")

;;;###autoload
(defun emacs-uptime ()
  "Gives Emacs' uptime, based on global var `*emacs-start-time*'."
  (interactive)
  (let* ((st *emacs-start-time*)                ; set in do-it-now.el
         (cur (current-time))
         (hi-diff (- (car cur) (car st)))
         (tot-sec (+ (ash hi-diff 16) (- (cadr cur) (cadr st))))
         (days (/ tot-sec (* 60 60 24)))
         (hrs  (/ (- tot-sec (* days 60 60 24)) (* 60 60)))
         (mins (/ (- tot-sec (* days 60 60 24) (* hrs 60 60)) 60))
         (secs (/ (- tot-sec (* days 60 60 24) (* hrs 60 60) (* mins 60)) 1)))
    (message "Up %dd %dh %dm %ds (%s), %d buffers, %d files"
             days hrs mins secs
             (format-time-string "%a %Y-%m-%d %T" st)
             (length (buffer-list))
             (count t (buffer-list)
                    :test-not
                    (lambda (ignore buf)
                      (null (cdr (assoc 'buffer-file-truename
                                        (buffer-local-variables buf)))))))))

(provide 'emacs-uptime)

;;; emacs-uptime.el ends here
;;;----------------------------------------------------------------------------
