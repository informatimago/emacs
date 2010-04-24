;;;;******************************************************************************
;;;;FILE:               pjb-objc-mode.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    199?/??/?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2001
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
(provide 'pjb-objc-mode)
(require 'cc-mode)
(require 'pjb-utilities)
(require 'pjb-cl)

(defun alist-setObject-forKey (alist object key)
   (cond ((null alist) (list (cons key object)))
		 ((eq (caar alist) key) (cons (cons key object) (cdr alist)))
		 (t (cons (car alist) (alist-setObject-forKey (cdr alist) object key)))))


(defvar c-indent-level)
(defvar c-brace-imaginary-offset)
(defvar c-brace-offset)
(defvar c-argdecl-indent)
(defvar c-label-offset)
(defvar c-continued-statement-offset)
(defvar c-continued-brace-offset)



(setq tab-width                     4)  ; the true one!

(setq c-indent-level                4) ;  Indentation of C statements with
                                        ; respect to containing block.

(setq c-brace-imaginary-offset      0) ;  Imagined indentation of a C open brace
                                        ; that actually follows a statement.

(setq c-brace-offset                0) ;  Extra indentation for braces, compared
                                        ; with other text in same context.

(setq c-argdecl-indent              4) ;  Indentation level of declarations of
                                        ; C function arguments.

(setq c-label-offset               -4) ;  Offset of C label lines and case
                                        ; statements relative to usual 
                                        ; indentation.

(setq c-continued-statement-offset  4) ;  Extra indent for lines not starting
                                        ; new statements.

(setq c-continued-brace-offset      0) ;  Extra indent for substatements that
                                        ; start with open-braces.

(setq c-basic-offset                4) ;  Amount of basic offset used by `+' and
                                        ; `-' symbols in `c-offsets-alist'.

(setq c-auto-newline                nil) ; Non-nil means automatically newline
                                        ;  before and after braces, and after
                                        ;  colons and semicolons, inserted in C
                                        ;  code. If you do not want a leading
                                        ;  newline before braces then use:
                                        ;  (define-key c-mode-map \"{\"
                                        ;           'electric-c-semi)"

(setq c-tab-always-indent           nil) ; Non-nil means TAB in C mode should
                                        ;  always reindent the current line,
                                        ;  regardless of where in the line point
                                        ;  is when the TAB command is used.





(defun syntax-of-previous-line ()
  "RETURN: the syntax (in c-guess-basic-syntax sense) of the previous line."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (c-guess-basic-syntax)))


(defun comment-of-method    ()
  "RETURN: whether the previous line is a objc-method-intro"
  (eq (caar (syntax-of-previous-line)) 'objc-method-intro))

(defvar c-syntactic-context '())

(defun comment-of-function  ()
  "RETURN: whether the current syntactic context is
            ((comment-intro) (knr-argdecl-intro . N))"
  (and (= (length c-syntactic-context) 2)
       (equal (nth 0 c-syntactic-context) '(comment-intro))
       (or (equal (car (nth 1 c-syntactic-context)) 'knr-argdecl-intro)
           (equal (car (nth 1 c-syntactic-context)) 'objc-method-args-cont))))


(defun comment-of-top-level ()
  "RETURN: whether the current syntactic context is
            ((comment-intro) (topmost-intro . N))"
  (and (= (length c-syntactic-context) 2)
       (equal (nth 0 c-syntactic-context) '(comment-intro))
       (equal (car (nth 1 c-syntactic-context)) 'topmost-intro)))


(defun previous-line-is-//-comment ()
  "RETURN: whether the previous line is a // comment."
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (looking-at "//")))


(defun current-line-is-//-comment ()
  "RETURN: whether the current line is a // comment."
  (save-excursion
    (back-to-indentation)
    (looking-at "//")))


(defun current-line-is-/*-comment ()
  "RETURN: whether the current line is a /* comment."
  (save-excursion
    (back-to-indentation)
    (looking-at "/[*]")))



(defun beginning-of-comment ()
  "PRE:    (point) is inside a comment.
DO:     Move the point at the beginning of the comment."
  (interactive)
  (let* ((syntax  (c-guess-basic-syntax))
         (boc     (assoc 'c syntax)))
    (while boc
      (forward-line -1)
      (beginning-of-comment)
      (setq syntax  (c-guess-basic-syntax)
            boc     (assoc 'c syntax)))
    (if (eq (caar syntax) 'comment-intro)
        (back-to-indentation)
      (error "We must start from the inside of a comment!"))))


(defun first-line-of-comment ()
  "
RETURN: whether the current line is on the first line of a comment, 
        discounting the comment-intro on the previous line.

     // YES comment-intro  /* NO  comment-intro  /*    NO  comment-intro
     // NO  comment-intro     YES c                 // YES comment-intro c
     // NO  comment-intro     NO  c                 // NO  comment-intro c
     // NO  comment-intro  */ NO  c              */    NO  c
"
  (save-excursion
    (let ((boc (assoc 'c c-syntactic-context)))
      (if boc
          ;; within a /* comment.
          (progn 
            (beginning-of-comment)
            (= (point) (cdr boc)))
        ;; within a range of // comments.
        (and (current-line-is-//-comment)
           (not (previous-line-is-//-comment)))))))



(defun really-comment-intro (langelem)
  "RETURN: whether the given langelem is really a comment intro, and not a mere
        // comment following a // comment or a comment inside a /* comment."
  (save-excursion
    (and (eq (car langelem) 'comment-intro) 
         (not (assoc 'c c-syntactic-context)))))


(defun pjb-lineup-C-comments (langelem)
  "PRE:    langelem = (comment-intro) or langelem = (c . N)
NOTE:   When langelem = (comment-intro), we're on the line '/*',
        when langelem = (c . N), we're inside the comment.
RETURN: The relative indentation of the following line.
SEE-ALSO:  c-indent-line"
  (save-excursion
    (if (really-comment-intro langelem)
        (progn
          ;; The indentation for "/*" depends on the previous line.
          (message "method=%S function=%S top-level=%S"
                   (comment-of-method) (comment-of-function) (comment-of-top-level))
          (cond
           ((comment-of-method)     c-basic-offset)
           ((comment-of-function)   c-basic-offset)
           ((comment-of-top-level)  c-basic-offset)
           (t                       0))
          )
      ;; Body or end of comment.
      (back-to-indentation)
      (cond

       ((looking-at "[*]+/")
        ;; The indentation of "*/" is the same as the indentation of "/*".
        (message "looking at end of comment")
        (goto-char (match-end 0))
        (message "match-end=%S" (point))
        (forward-comment -1)
        (message "before comment=%S" (point))
        (message "result=%S"         (- (current-column) (c-langelem-col langelem)))
        0) ;;(- (current-column) (c-langelem-col langelem)))

       ((first-line-of-comment)  
        c-basic-offset)

       (t
        ;; The indentation of the other lines of the comment
        ;; is the same as that of the previous line.
        0)))))


(if nil
(defun c-indent-line (&optional syntax)
  ;; indent the current line as C/C++/ObjC code. Optional SYNTAX is the
  ;; syntactic information for the current line. Returns the amount of
  ;; indentation change (in columns).
  (let* ((c-syntactic-context (or syntax (c-guess-basic-syntax)))
         (pos (- (point-max) (point)))
         (indent (apply '+ (mapcar 'c-get-offset c-syntactic-context)))
         (shift-amt  (- (current-indentation) indent)))
    (message "syntax: %s, indent= %d" c-syntactic-context indent)
    (if (zerop shift-amt)
        nil
      (delete-region (c-point 'bol) (c-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (c-point 'boi))
        (back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))
      )
    (run-hooks 'c-special-indent-hook)
    shift-amt))
)




(defconst c-C++-method-key
   (concat 
    "^\\s *"
    "\\(ABSTRACTMETHOD\\|CLASSMETHOD\\|METHOD"
    "\\|PROCEDURE\\|CONSTRUCTOR2?\\|DESTRUCTOR\\)"
    "([^)]*\\(([^)]*)\\)?[^)]*)\\s *"))


(defvar c-C++-access-key)
(defvar c-C++-class-key)
(defvar c-C++-comment-start-regexp)
(defvar c-C++-conditional-key)
(defvar c-C++-extra-toplevel-key)
(defvar c-C++-method-key)
(defvar cc-imenu-c++-generic-expression)
(defvar c-conditional-key)
(defvar c-extra-toplevel-key)
(defvar c-access-key)
(defvar c-method-key)

(defun c++-mode ()
  "Major mode for editing C++ code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c++-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{c++-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table c++-mode-syntax-table)
  (setq major-mode 'c++-mode
	mode-name "C++"
	local-abbrev-table c++-mode-abbrev-table)
  (use-local-map c++-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end ""
	c-conditional-key c-C++-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
	c-class-key c-C++-class-key
	c-extra-toplevel-key c-C++-extra-toplevel-key
	c-access-key c-C++-access-key
    c-method-key c-C++-method-key
	c-recognize-knr-p nil
	imenu-generic-expression cc-imenu-c++-generic-expression
	imenu-case-fold-search nil
    case-fold-search nil
	)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c++-mode-hook)
  (c-update-modeline))



(defun c-just-after-func-arglist-p (&optional containing)
  ;; Return t if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  Optional CONTAINING is position of containing s-exp open
  ;; brace.  If not supplied, point is used as search start.
  (save-excursion
    (c-backward-syntactic-ws)
    (let ((checkpoint (or containing (point))))
      (goto-char checkpoint)
      ;; could be looking at const specifier
      (if (and (eq (char-before) (character "t"))
               (forward-word -1)
               (looking-at "\\<const\\>"))
          (c-backward-syntactic-ws)
        ;; otherwise, we could be looking at a hanging member init
        ;; colon
        (goto-char checkpoint)
        (if (and (eq (char-before) (character ":"))
                 (progn
                   (forward-char -1)
                   (c-backward-syntactic-ws)
                   (looking-at "[ \t\n]*:\\([^:]+\\|$\\)")))
            nil
          (goto-char checkpoint))
        )

      (if (and (eq c-method-key c-C++-method-key)
               (eq (char-before) (character "\\"))
               ;; Let's check if we're after a C++ method definition macro.
               (save-excursion
                 (c-forward-sexp -1) ;; at the opening parenthesis.
                 (beginning-of-line) ;; at the beginning of the line.
                 (looking-at c-C++-method-key)))
          nil
        (and (eq (char-before) (character "\\"))
             ;; check if we are looking at a method def
             (or (not c-method-key)
                 (progn
                   (c-forward-sexp -1)
                   (forward-char -1)
                   (c-backward-syntactic-ws)
                   (not (or (position (char-before) "-+")
                            ;; or a class category
                            (progn
                              (c-forward-sexp -2)
                              (looking-at c-class-key))
                            ))))))
      )))


(defconst todo-regexp 
  "\\(\\(//\\)? *\\(TODO\\|SEE\\):.*\\|/\\* *\\(TODO\\|SEE\\):\\(\\*[^/]\\|[^\\*]\\)* \\*/\\)"
  "regexp to match a TODO: (obsolete SEE:) comment.")

(font-lock-add-keywords
 'c-mode (list (list todo-regexp 1 'font-lock-warning-face t)))
(font-lock-add-keywords
 'c++-mode (list (list todo-regexp 1 'font-lock-warning-face t)))




(add-hook 'c-mode-common-hook
          (lambda () 
            (define-key c-mode-map "{"  'self-insert-command)))

(add-hook 'objc-mode-hook
          (lambda () 
            (define-key c-mode-map "{"  'self-insert-command)))

;;;; pjb-objc-mode.el                 -- 2004-02-22 02:47:23 -- pascal   ;;;;
