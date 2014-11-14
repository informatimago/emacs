;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-comint.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Add output filters to process more emacs-048 codes.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-07-27 <PJB> Extracted from ~/.emacs
;;;;BUGS
;;;;
;;;;    This doesn't work.  When there are errors signaled from the
;;;;    filter functions, this may hang emacs.  So we need to add
;;;;    error handler or to debug better.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2011
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

(when nil
  
  (defun pjb-comint-filter-meat/erase-screen (string)
    (let ((pos (search "c" string :from-end t)))
      (if pos
          (progn
            (erase-buffer)
            (subseq string (+ 2 pos)))
          string)))

  (defun pjb-comint-filter-meat/erase-in-line (string)
    (let* ((pos (search "\\[\\([0-9]*\\)K" string :from-end t))
           (ps  (match-string 1 string)))
      (if pos
          (progn
            (erase-buffer)
            (if (or (string= "" ps) (= 0 (parse-number ps)))
                (subseq string 0 pos)
                ""))
          string)))

  (defun ecma-048-cuu ()
    (backward-line 1))

  (defun ecma-048-cuf (offset)
    (let ((new-column (+ (point) offset)))
      (if (< (save-excursion (end-of-line) (point))  new-column)
          (progn (end-of-line)
                 ;; (insert (make-string (- new-column (point)) 32))
                 )
          (forward-char offset))))

  (defun ecma-048-crlf ()
    (insert (make-string (forward-line 1) 10)))

  (defun pjb-comint-filter-meat/position (string)
    (let ((commands '(("\nA"            beginning-of-line)
                      ("\\(\\[0-9\\]+\\)C"  ecma-048-cuf 1)
                      ("\\(\\[0-9;\\]*\\)H" ignore)))
          (start 0))
      (while (let ((cmd (find-if (lambda (cmd) (eql start (string-match (first cmd) string start))) commands)))
               (when cmd
                 (setf start (match-end 0))
                 (apply (second cmd) (mapcar (lambda (i) (nth-value 0 (cl:parse-integer (match-string i string)))) (cddr cmd)))
                 t)))
      (if (zerop start)
          string
          (subseq string start))))

  (defun pjb-comint-filter-meat/color (string)
    "Remove color ansi codes."
    (with-temp-buffer
      (insert string)
      (goto-char 0)
      (let ((changed nil))
        (while (re-search-forward "[\\[0-9;\\]*m" (point-max) t)
          (setf changed t)
          (delete-region (match-beginning 0) (match-end 0)))
        (if changed
            (buffer-substring-no-properties (point-min) (point-max))
            string))))

  ;; (add-hook 'comint-preoutput-filter-functions 'pjb-comint-filter-meat/position)
  (add-hook 'comint-preoutput-filter-functions 'pjb-comint-filter-meat/erase-screen)
  (add-hook 'comint-preoutput-filter-functions 'pjb-comint-filter-meat/erase-in-line)
  (add-hook 'comint-preoutput-filter-functions 'pjb-comint-filter-meat/color)


  ;; comint-preoutput-filter-functions
  (setf comint-preoutput-filter-functions nil)


  );; when nil


;;;; THE END ;;;;


