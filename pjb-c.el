;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-c.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports C indenting functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2003-02-25 <PJB> Updated, from pjb-objc-mode.el, for emacs 21.2.
;;;;    199?-??-?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2003
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;******************************************************************************
(debug "Should not load this file!")
(provide 'pjb-c)
(require 'cc-mode)
(require 'pjb-utilities)
(error "should not be loaded...")


(defun pjb-c-syntax-of-previous-line ()
  "
RETURN: the syntax (in c-guess-basic-syntax sense) of the previous line.
"
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (c-guess-basic-syntax))
  );;pjb-c-syntax-of-previous-line


(defun pjb-c-comment-of-method    (syntcont)
  "
RETURN: whether the previous line is a objc-method-intro
        and the current line is a comment-intro.
"
  (and (eq (caar (pjb-c-syntax-of-previous-line)) 'objc-method-intro)
       (eq (caar syntcont)                        'comment-intro))
  );;pjb-c-comment-of-method


(defun pjb-c-comment-of-function  (syntcont)
  "
RETURN: whether the current syntactic context is
            ((comment-intro) (knr-argdecl-intro . N))
"
  (and (= (length syntcont) 2)
       (eq (caar syntcont) 'comment-intro)
       (or (eq (caadr  syntcont) 'knr-argdecl-intro)
           (eq (caadr  syntcont) 'objc-method-args-cont)))
  );;pjb-c-comment-of-function


(defun pjb-c-comment-of-top-level (syntcont)
  "
RETURN: whether the current syntactic context is
            ((comment-intro) (topmost-intro . N))
"
  (and (= (length syntcont) 2)
       (eq (caar  syntcont) 'comment-intro)
       (eq (caadr syntcont) 'topmost-intro))
  );;pjb-c-comment-of-top-level


(defun pjb-c-previous-line-is-//-comment ()
  "
RETURN: whether the previous line is a // comment.
"
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (looking-at "//"))
  );;pjb-c-previous-line-is-//-comment


(defun pjb-c-current-line-is-//-comment ()
  "
RETURN: whether the current line is a // comment.
"
  (save-excursion
    (back-to-indentation)
    (looking-at "//"))
  );;pjb-c-current-line-is-//-comment


(defun pjb-c-current-line-is-/*-comment ()
  "
RETURN: whether the current line is a /* comment.
"
  (save-excursion
    (back-to-indentation)
    (looking-at "/[*]"))
  );;pjb-c-current-line-is-/*-comment


(defun pjb-c-beginning-of-comment ()
  "
PRE:    (point) is inside a comment.
DO:     Move the point at the beginning of the comment.
"
  (interactive)
  (let* ((syntax  (c-guess-basic-syntax))
         (boc     (assoc 'c syntax)))
    (while boc
      (forward-line -1)
      (pjb-c-beginning-of-comment)
      (setq syntax  (c-guess-basic-syntax)
            boc     (assoc 'c syntax)))
    (if (eq (caar syntax) 'comment-intro)
        (back-to-indentation)
      (error "We must start from the inside of a comment!")))
  );;pjb-c-beginning-of-comment


(defun pjb-c-first-line-of-comment (syntcont)
  "
RETURN: whether the current line is on the first line of a comment, 
        discounting the comment-intro on the previous line.
EXAMPLES:
     // YES comment-intro  /* NO  comment-intro  /*    NO  comment-intro
     // NO  comment-intro     YES c                 // YES comment-intro c
     // NO  comment-intro     NO  c                 // NO  comment-intro c
     // NO  comment-intro  */ NO  c              */    NO  c
"
  (let ((boc (assoc 'c syntcont))
        (curr-point (save-excursion (beginning-of-line) (point))))
    (save-excursion
      (if boc
        ;; within a /* comment.
        (progn 
          (pjb-c-beginning-of-comment)
          (and (= (point) (cdr boc))
               (= 1 (count-lines (point) curr-point))))
        ;; within a range of // comments.
        (and (pjb-c-current-line-is-//-comment)
             (not (pjb-c-previous-line-is-//-comment))))))
  );;pjb-c-first-line-of-comment



(defun pjb-c-really-comment-intro (syntcont)
  "
RETURN: whether the given syntcont is really a comment intro, and not a mere
        // comment following a // comment or a comment inside a /* comment.
"
  (and (eq (caar syntcont) 'comment-intro) 
       (not (assoc 'c syntcont)))
  );;pjb-c-really-comment-intro


(defun pjb-c-keyword-comment-line (syntcont)
  "
RETURN:         Wether we're in a ''KEYWORD:  comment'' line.
"
  (let ((result (progn
  (back-to-indentation)
  (let ((case-fold-search nil))
    (looking-at "[A-Z][-A-Z ]*[A-Z]: "))
  ))) (message "c-keyword-comment=%S" result) result)
  );;pjb-c-keyword-comment-line


(defvar pjb-c-keyword-comment-indent 16
  "Indentation of keyworded comments.");;pjb-c-keyword-comment-indent


(defun pjb-c-lineup-C-comments (langelem)
  "
PRE:            langelem = (comment-intro) or langelem = (c . N)
NOTE:           When langelem = (comment-intro), we're on the line '/*',
                when langelem = (c . N), we're inside the comment.
RETURN:         The relative indentation of the following line.
SEE ALSO:       c-indent-line
"
  (save-excursion
    (let ((c-syntactic-context (c-guess-basic-syntax)))
      (message "\n(c-guess-basic-syntax)=%S\nc-syntactic-context=%S\nlangelem=%S" (c-guess-basic-syntax) c-syntactic-context langelem)
      (if (pjb-c-really-comment-intro c-syntactic-context)
        (progn
          (message "really-comment-intro")
          ;; The indentation for "/*" depends on the previous line.
          (cond
           ((or (pjb-c-comment-of-method    c-syntactic-context)
                (pjb-c-comment-of-function  c-syntactic-context)
                (pjb-c-comment-of-top-level c-syntactic-context))
            (- c-basic-offset))
           (t
            0))
          )
        (progn
          (message "Body or end of comment")
          ;; Body or end of comment.
          (back-to-indentation)
          (cond
           ((looking-at "[*]+/")
            ;; The indentation of "*/" is the same as the indentation of "/*".
            (message "at end of comment")
            (goto-char (match-end 0))
            (forward-comment -1)
            0) ;;(- (current-column) (c-langelem-col langelem)))
           ((pjb-c-first-line-of-comment c-syntactic-context)  
            (message "first line of comment")
            c-basic-offset)
           (t
            ;; The indentation of the other lines of the comment
            ;; is the same as that of the previous line.
            ;; TODO: We should check if the previous lines begins 
            ;;       with a keyword.
            ;;         EXEMPLE:        comment tralala
            ;;                         comment tralala
            (message "In middle of comment")
            (cond
             ((pjb-c-keyword-comment-line c-syntactic-context) 
              (message "keyword line")
              c-basic-offset)
             ((save-excursion
                (forward-line -1)
                (pjb-c-keyword-comment-line c-syntactic-context)) 
              (message "just after keyword line")
              (+ c-basic-offset pjb-c-keyword-comment-indent))
             (t 
              (message "other comment line")
              ;; same indentation as previous line.
              (save-excursion
                (forward-line -1)
                (back-to-indentation)
                (- (current-column) (c-langelem-col langelem))) ))))))))
  );;pjb-c-lineup-C-comments



(defconst pjb-c-todo-regexp 
  "\\(\\(//\\)? *\\(TODO\\|SEE\\):.*\\|/\\* *\\(TODO\\|SEE\\):\\(\\*[^/]\\|[^\\*]\\)* \\*/\\)"
  "regexp to match a TODO: (obsolete SEE:) comment.");;pjb-c-todo-regexp


(defun pjb-c-todo-hook ()
 "A hook for C-like modes to add specific font-lock to TODO comments."
  (font-lock-add-keywords
   major-mode (list (list pjb-c-todo-regexp 1 'font-lock-warning-face t))))


;; (defconst pjb-c-C++-method-key
;;   (concat 
;;    "^\\s *"
;;    "\\(ABSTRACTMETHOD\\|CLASSMETHOD\\|METHOD"
;;    "\\|PROCEDURE\\|CONSTRUCTOR2?\\|DESTRUCTOR\\)"
;;    "([^)]*\\(([^)]*)\\)?[^)]*)\\s *"))
;; 
;;  
;; (defun pjb-c++-mode-hook ()
;;   "A hook for C++ mode where we install our own pjb-c-C++-method-key."
;;   (setq c-method-key pjb-c-C++-method-key))




;;; (setq tab-width                     4)  ; the true one!

;;; (setq c-indent-level                4) ;  Indentation of C statements with
;;;                                         ; respect to containing block.

;;; (setq c-brace-imaginary-offset      0) ;  Imagined indentation of a C open brace
;;;                                         ; that actually follows a statement.

;;; (setq c-brace-offset                0) ;  Extra indentation for braces, compared
;;;                                         ; with other text in same context.

;;; (setq c-argdecl-indent              4) ;  Indentation level of declarations of
;;;                                         ; C function arguments.

;;; (setq c-label-offset               -4) ;  Offset of C label lines and case
;;;                                         ; statements relative to usual 
;;;                                         ; indentation.

;;; (setq c-continued-statement-offset  4) ;  Extra indent for lines not starting
;;;                                         ; new statements.

;;; (setq c-continued-brace-offset      0) ;  Extra indent for substatements that
;;;                                         ; start with open-braces.

;;; (setq c-basic-offset                4) ;  Amount of basic offset used by `+' and
;;;                                         ; `-' symbols in `c-offsets-alist'.

;;; (setq c-auto-newline                nil) ; Non-nil means automatically newline
;;;                                         ;  before and after braces, and after
;;;                                         ;  colons and semicolons, inserted in C
;;;                                         ;  code. If you do not want a leading
;;;                                         ;  newline before braces then use:
;;;                                         ;  (define-key c-mode-map \"{\"
;;;                                         ;           'electric-c-semi)"

;;; (setq c-tab-always-indent           nil) ; Non-nil means TAB in C mode should
;;;                                         ;  always reindent the current line,
;;;                                         ;  regardless of where in the line point
;;;                                         ;  is when the TAB command is used.





;;;; pjb-c.el                         -- 2003-10-10 23:50:40 -- pascal   ;;;;
