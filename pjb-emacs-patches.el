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

;;;; THE END ;;;;
