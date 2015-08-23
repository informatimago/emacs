;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-emacs-patches.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Random patches.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-20 <PJB> Extracted from ~/rc/emacs-common.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(when (< emacs-major-version 22)
  (unless (fboundp 'called-interactively-p)
    (defun called-interactively-p () (interactive-p))))


(when (string= emacs-version "24.3.1")
  (require 'minibuffer)
  
  (defun completion--twq-all (string ustring completions boundary
                              unquote requote)
    (when completions
      (pcase-let*
          ((prefix
            (let ((completion-regexp-list nil))
              (try-completion "" (cons (substring ustring boundary)
                                       completions))))
           (`(,qfullpos . ,qfun)
             (funcall requote (+ boundary (length prefix)) string))
           (qfullprefix (substring string 0 qfullpos))
           ;; FIXME: This assertion can be wrong, e.g. in Cygwin, where
           ;; (unquote "c:\bin") => "/usr/bin" but (unquote "c:\") => "/".
           ;;(cl-assert (completion--string-equal-p
           ;;            (funcall unquote qfullprefix)
           ;;            (concat (substring ustring 0 boundary) prefix))
           ;;           t))
           (qboundary (car (funcall requote boundary string)))
           ;; (_ (cl-assert (<= qboundary qfullpos)))
           ;; FIXME: this split/quote/concat business messes up the carefully
           ;; placed completions-common-part and completions-first-difference
           ;; faces.  We could try within the mapcar loop to search for the
           ;; boundaries of those faces, pass them to `requote' to find their
           ;; equivalent positions in the quoted output and re-add the faces:
           ;; this might actually lead to correct results but would be
           ;; pretty expensive.
           ;; The better solution is to not quote the *Completions* display,
           ;; which nicely circumvents the problem.  The solution I used here
           ;; instead is to hope that `qfun' preserves the text-properties and
           ;; presume that the `first-difference' is not within the `prefix';
           ;; this presumption is not always true, but at least in practice it is
           ;; true in most cases.
           (qprefix (propertize (substring qfullprefix qboundary)
                                'face 'completions-common-part)))

        ;; Here we choose to quote all elements returned, but a better option
        ;; would be to return unquoted elements together with a function to
        ;; requote them, so that *Completions* can show nicer unquoted values
        ;; which only get quoted when needed by choose-completion.
        (nconc
         (mapcar (lambda (completion)
                   ;; (cl-assert (string-prefix-p prefix completion 'ignore-case) t)
                   (let* ((new (substring completion (length prefix)))
                          (qnew (funcall qfun new))
                          (qcompletion (concat qprefix qnew)))
                     ;; FIXME: Similarly here, Cygwin's mapping trips this
                     ;; assertion.
                     ;;(cl-assert
                     ;; (completion--string-equal-p
                     ;;  (funcall unquote
                     ;;           (concat (substring string 0 qboundary)
                     ;;                   qcompletion))
                     ;;  (concat (substring ustring 0 boundary)
                     ;;          completion))
                     ;; t)
                     qcompletion))
                 completions)
         qboundary)))))

(ignore-errors
  (require 'newcomment)
  (defun comment-region-internal (beg end cs ce
                                  &optional ccs cce block lines indent)
    "Comment region BEG..END.
CS and CE are the comment start resp end string.
CCS and CCE are the comment continuation strings for the start resp end
of lines (default to CS and CE).
BLOCK indicates that end of lines should be marked with either CCE, CE or CS
\(if CE is empty) and that those markers should be aligned.
LINES indicates that an extra lines will be used at the beginning and end
of the region for CE and CS.
INDENT indicates to put CS and CCS at the current indentation of the region
rather than at left margin."
    ;;(assert (< beg end))
    (let ((no-empty nil ; PJB: always no-empty.
            ;;  (not (or (eq comment-empty-lines t)
            ;;           (and comment-empty-lines (zerop (length ce)))))
            ))
      ;; Sanitize CE and CCE.
      (if (and (stringp ce) (string= "" ce)) (setq ce nil))
      (if (and (stringp cce) (string= "" cce)) (setq cce nil))
      ;; If CE is empty, multiline cannot be used.
      (unless ce (setq ccs nil cce nil))
      ;; Should we mark empty lines as well ?
      (if (or ccs block lines) (setq no-empty nil))
      ;; Make sure we have end-markers for BLOCK mode.
      (when block (unless ce (setq ce (comment-string-reverse cs))))
      ;; If BLOCK is not requested, we don't need CCE.
      (unless block (setq cce nil))
      ;; Continuation defaults to the same as CS and CE.
      (unless ccs (setq ccs cs cce ce))

      (save-excursion
        (goto-char end)
        ;; If the end is not at the end of a line and the comment-end
        ;; is implicit (i.e. a newline), explicitly insert a newline.
        (unless (or ce (eolp)) (insert "\n") (indent-according-to-mode))
        (comment-with-narrowing
            beg end
          (let ((min-indent (point-max))
                (max-indent 0))
            (goto-char (point-min))
            ;; Quote any nested comment marker
            (comment-quote-nested comment-start comment-end nil)
            
            ;; Loop over all lines to find the needed indentations.
            (goto-char (point-min))
            (while
                (progn
                  (unless (looking-at "[ \t]*$")
                    (setq min-indent (min min-indent (current-indentation))))
                  (end-of-line)
                  (setq max-indent (max max-indent (current-column)))
                  (not (or (eobp) (progn (forward-line) nil)))))

            (setq max-indent
                  (+ max-indent (max (length cs) (length ccs))
                     ;; Inserting ccs can change max-indent by (1- tab-width)
                     ;; but only if there are TABs in the boxed text, of course.
                     (if (save-excursion (goto-char beg)
                                         (search-forward "\t" end t))
                         (1- tab-width) 0)))
            ;; ;; Inserting ccs can change max-indent by (1- tab-width).
            ;; (setq max-indent
            ;;   (+ max-indent (max (length cs) (length ccs)) tab-width -1))
            (unless indent (setq min-indent 0))
            
            ;; make the leading and trailing lines if requested
            (when lines
              (let ((csce
                     (comment-make-extra-lines
                      cs ce ccs cce min-indent max-indent block)))
                (setq cs (car csce))
                (setq ce (cdr csce))))

            (goto-char (point-min))
            ;; Loop over all lines from BEG to END.
            (while
                (progn
                  (unless (and no-empty (looking-at "[ \t]*$"))
                    (move-to-column min-indent t)
                    (insert cs) (setq cs ccs) ;switch to CCS after the first line
                    (end-of-line)
                    (if (eobp) (setq cce ce))
                    (when cce
                      (when block (move-to-column max-indent t))
                      (insert cce)))
                  (end-of-line)
                  (not (or (eobp) (progn (forward-line) nil))))))))))
  );;patch



;;;; THE END ;;;;
