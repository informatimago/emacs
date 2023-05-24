;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               insert-image.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A patch to emacs to be able to insert images in a comint buffer
;;;;    such as inferior-lisp REPL.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-04-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
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
;;;;**************************************************************************
(require 'cl)
(require 'comint)

(defun splice (new-list old list)
  "Like substitute but replace the old by the elements in the new-list."
  (loop
     with result = '()
     for item in list
     do (if (eql old item)
            (loop
               for item in new-list
               do (push item result))
            (push item result))
     finally (return (nreverse result))))

(defun comint-output-filter (process string)
  (pjb-comint-output-filter process string))

(defun pjb-comint-output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    ;; First check for killed buffer or no input.
    (when (and string oprocbuf (buffer-name oprocbuf))
      (with-current-buffer oprocbuf
        ;; Run preoutput filters
        (let ((functions (splice (default-value 'comint-preoutput-filter-functions)
                                 t
                                 comint-preoutput-filter-functions))
              (strings (list string)))

          (while (and functions strings)
            (setf strings (loop
                             with result = ()
                             for string in strings
                             do (setf result (revappend (ensure-list (funcall (car functions) string)) result))
                             finally (return (nreverse result))))
            (setq functions (cdr functions)))
          (setf string strings))

        ;; Insert STRING
        (let ((inhibit-read-only t)
              ;; The point should float after any insertion we do.
              (saved-point (copy-marker (point) t)))

          ;; We temporarly remove any buffer narrowing, in case the
          ;; process mark is outside of the restriction
          (save-restriction
            (widen)

            (goto-char (process-mark process))
            (set-marker comint-last-output-start (point))

            ;; insert-before-markers is a bad thing. XXX
            ;; Luckily we don't have to use it any more, we use
            ;; window-point-insertion-type instead.
            (loop
               for item in string
               do (cond
                    ((stringp item) (insert item))
                    ((consp   item) (insert-image (first item) (second item)))
                    (t (error "Unexpected kind of insert %S" item))))


            ;; Advance process-mark
            (set-marker (process-mark process) (point))
            (setf string (buffer-substring comint-last-output-start (point)))
            (unless comint-inhibit-carriage-motion
              ;; Interpret any carriage motion characters (newline, backspace)
              (comint-carriage-motion comint-last-output-start (point)))

            ;; Run these hooks with point where the user had it.
            (goto-char saved-point)
            (run-hook-with-args 'comint-output-filter-functions string)
            (set-marker saved-point (point))

            (goto-char (process-mark process)) ; in case a filter moved it

            (unless comint-use-prompt-regexp
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (add-text-properties comint-last-output-start (point)
                                     '(front-sticky
                                       (field inhibit-line-move-field-capture)
                                       rear-nonsticky t
                                       field output
                                       inhibit-line-move-field-capture t))))

            ;; Highlight the prompt, where we define `prompt' to mean
            ;; the most recent output that doesn't end with a newline.
            (let ((prompt-start (save-excursion (forward-line 0) (point)))
                  (inhibit-read-only t)
                  (inhibit-modification-hooks t))
              (when comint-prompt-read-only
                (or (= (point-min) prompt-start)
                    (get-text-property (1- prompt-start) 'read-only)
                    (put-text-property
                     (1- prompt-start) prompt-start 'read-only 'fence))
                (add-text-properties
                 prompt-start (point)
                 '(read-only t rear-nonsticky t front-sticky (read-only))))
              (when (boundp 'comint-last-prompt-overlay)
               (unless (and (bolp) (null comint-last-prompt-overlay))
                 ;; Need to create or move the prompt overlay (in the case
                 ;; where there is no prompt ((bolp) == t), we still do
                 ;; this if there's already an existing overlay).
                 (if comint-last-prompt-overlay
                     ;; Just move an existing overlay
                     (move-overlay comint-last-prompt-overlay
                                   prompt-start (point))
                     ;; Need to create the overlay
                     (setq comint-last-prompt-overlay
                           (make-overlay prompt-start (point)))
                     (overlay-put comint-last-prompt-overlay
                                  'font-lock-face 'comint-highlight-prompt)))))
            (goto-char saved-point)))))))


(defun pjb-comint-preoutput-insert-image (string)
  (when string
   (let ((case-fold-search t))
     (loop
       with result = '()
       while (and (plusp (length string))
                  (string-match "\\(.*\\)(EMACS:INSERT-IMAGE[ \t\n]+#P\"\\(\\([^\\\"]\\|\\.\\)*\\)\")\\(.*\\)"
                                string))
       do (let ((before (match-string 1 string))
                (path   (match-string 2 string))
                (after  (match-string 4 string)))
            (push before result)
            (push (list (create-image path) " ") result)
            (setf string after))
       finally (push string result) (return (nreverse result))))))

;;;; THE END ;;;;
