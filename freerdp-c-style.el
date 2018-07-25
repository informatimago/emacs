;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-c-style.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines my own C-style.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2012-11-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal Bourguignon 2012 - 2012
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
(require 'cl) ; always
(require 'cc-mode)


(setf auto-mode-alist (append '(("src/.*\\.h$" . objc-mode)
                                ("\\.m$"  . objc-mode)
                                ("\\.mm$" . objc-mode))
                              auto-mode-alist))



(c-add-style
 "freerdp"
 '((c-backslash-column             .  72)

   (c-backslash-max-column         . 128)

   (c-basic-offset                 . 8)  ; Amount of basic offset used by `+' and
                                        ; `-' symbols in `c-offsets-alist'.

   (c-block-comment-prefix         .  "")

   (c-cleanup-list                 . (brace-else-brace
                                      brace-elseif-brace
                                      brace-catch-brace
                                      list-close-comma
                                      scope-operator))

   (c-comment-only-line-offset     . (0 . 0))

   ;; (c-comment-prefix-regexp)

   (c-doc-comment-style            . javadoc)

   (c-hanging-braces-alist
    . (
       ;; (statement-cont)
       ;; (brace-list-intro)
       (inexpr-class-open    . (before after))
       (inexpr-class-close   . (before after))

       (defun-open           . (before after))         ; Brace that opens a function definition.
       (defun-close          . (before after))        ; Brace that closes a function definition.
       (class-open           . (before after))  ; Brace that opens a class definition.
       (class-close          . ())              ; Brace that closes a class definition.
       (inline-open          . (before after))  ; Brace that opens an in-class inline method.
       (inline-close         . (before after))        ; Brace that closes an in-class inline method.
       (block-open           . ())              ; Statement block open brace.
       (block-close          . c-snug-do-while) ; Statement block close brace.
       (brace-list-open      . (before after))         ; Open brace of an enum or static array list.
       (brace-list-close     . (before after))  ; Close brace of an enum or static array list.
       (brace-entry-open     . (before after))  ; Subsequent lines in an enum or static array
       (statement-case-open  . (before after))         ; The first line in a case block starting with brace.
       (substatement-open    . (before after))         ; The brace that opens a substatement block.
       (extern-lang-open     . (before after))         ; Brace that opens an "extern" block.
       (extern-lang-close    . (before after))  ; Brace that closes an "extern" block.
       (namespace-open       . (before after))
       (namespace-close      . ())
       (module-open          . (before after))
       (module-close         . ())
       (composition-open     . (before after))
       (composition-close)   . ()))

   (c-hanging-colons-alist         . ((case-label           . (after))
                                      (label                . (after))
                                      (access-label         . (after))
                                      (member-init-intro    . ())
                                      (inher-intro          . ())))

   (c-hanging-semi&comma-criteria  . ())

   ;; (c-indent-comment-alist)

   (c-indent-comments-syntactically-p . nil)

   (c-label-minimum-indentation       . 2)

   (c-offsets-alist
    . (

       (string                . 0)
       ;; Inside multi-line string.

       (c                     . 0)
       ;; Inside a multi-line C style block comment.

       (defun-open            . 0)
       ;; Brace that opens a function definition.

       (defun-close           . 0)
       ;; Brace that closes a function definition.

       (defun-block-intro     . +)
       ;; The first line in a top-level defun.

       (class-open            . 0)
       ;; Brace that opens a class definition.

       (class-close           . 0)
       ;; Brace that closes a class definition.

       (inline-open           . 0)
       ;; Brace that opens an in-class inline method.

       (inline-close          . 0)
       ;; Brace that closes an in-class inline method.

       (func-decl-cont        . +)
       ;; The region between a function definition's
       ;; argument list and the function opening brace
       ;; (excluding K&R argument declarations).  In C, you
       ;; cannot put anything but whitespace and comments
       ;; between them; in C++ and Java, throws declarations
       ;; and other things can appear in this context.

       (knr-argdecl-intro     . +)
       ;; First line of a K&R C argument declaration.

       (knr-argdecl           . +)
       ;; Subsequent lines in a K&R C argument declaration.

       (topmost-intro         . 0)
       ;; The first line in a topmost construct definition.

       (topmost-intro-cont    . (c-lineup-string-cont
                                 0))
       ;; Topmost definition continuation lines.

       (member-init-intro     . +)
       ;; First line in a member initialization list.

       (member-init-cont      . ++)
       ;; Subsequent member initialization list lines.

       (inher-intro           . +)
       ;; First line of a multiple inheritance list.

       (inher-cont            . ++)
       ;; Subsequent multiple inheritance lines.

       (block-open            . 0)
       ;; Statement block open brace.

       (block-close           . 0)
       ;; Statement block close brace.

       (brace-list-open       . 0)
       ;; Open brace of an enum or static array list.

       (brace-list-close      . 0)
       ;; Close brace of an enum or static array list.

       (brace-list-intro      . +)
       ;; First line in an enum or static array list.

       (brace-list-entry      . 0)
       ;; Subsequent lines in an enum or static array list.

       (brace-entry-open      . 0)
       ;; Subsequent lines in an enum or static array
       ;; list that start with an open brace.

       (statement             . (c-lineup-runin-statements
                                 0))
       ;; A C (or like) statement.

       (statement-cont        . (c-lineup-string-cont
                                 ++))
       ;; A continuation of a C (or like) statement.

       (statement-block-intro . +)
       ;; The first line in a new statement block.

       (statement-case-intro  . +)
       ;; The first line in a case "block".

       (statement-case-open   . +)
       ;; The first line in a case block starting with brace.

       (substatement          . +)
       ;; The first line after an if/while/for/do/else.

       (substatement-open     . 0)
       ;; The brace that opens a substatement block.

       (substatement-label    . /)
       ;; Labelled line after an if/while/for/do/else.

       (case-label            . +)
       ;; A "case" or "default" label.

       (access-label          . -)
       ;; C++ private/protected/public access label.

       (label                 . /)
       ;; Any ordinary label.

       (do-while-closure      . 0)
       ;; The "while" that ends a do/while construct.

       (else-clause           . 0)
       ;; The "else" of an if/else construct.

       (catch-clause          . 0)
       ;; The "catch" or "finally" of a try/catch construct.

       (comment-intro         . 0)
       ;; A line containing only a comment introduction.

       (arglist-intro         . (c-lineup-arglist-intro-after-paren
                                 +))
       ;; The first line in an argument list.

       (arglist-cont          . (c-lineup-string-cont
                                 ;; c-lineup-arglist-intro-after-paren
                                 ;; ssssc-lineup-argcont
                                 +))
       ;; Subsequent argument list lines when no
       ;; arguments follow on the same line as the
       ;; arglist opening paren.

       (arglist-cont-nonempty . (c-lineup-string-cont
                                 ;; c-lineup-arglist-intro-after-paren
                                 ;; c-lineup-argcont
                                 +))
       ;; Subsequent argument list lines when at
       ;; least one argument follows on the same
       ;; line as the arglist opening paren.

       (arglist-close         . (c-lineup-argcont
                                 c-lineup-arglist-intro-after-paren
                                 -))
       ;; The solo close paren of an argument list.

       (stream-op             . (c-lineup-streamop +))
       ;; Lines continuing a stream operator construct.

       (inclass               . +)
       ;; The construct is nested inside a class definition.
       ;; Used together with e.g. `topmost-intro'.

       (cpp-macro             . [0])
       ;; The start of a C preprocessor macro definition.

       (cpp-macro-cont        . [8])
       ;; Inside a multi-line C preprocessor macro definition.

       (friend                . 0)
       ;; A C++ friend declaration.

       (objc-method-intro     . 0)
       ;; The first line of an Objective-C method definition.

       (objc-method-args-cont . (c-lineup-ObjC-method-args-2
                                 +))
       ;; Lines continuing an Objective-C method definition.

       (objc-method-call-cont . (c-lineup-ObjC-method-call-colons
                                 c-lineup-ObjC-method-call
                                 +))
       ;; Lines continuing an Objective-C method call.

       (extern-lang-open      . 0)
       ;; Brace that opens an "extern" block.

       (extern-lang-close     . 0)
       ;; Brace that closes an "extern" block.

       (inextern-lang         . +)
       ;; Analogous to the `inclass' syntactic symbol,
       ;; but used inside "extern" blocks.

       (namespace-open        . 0)

       (namespace-close       . 0)

       (innamespace           . +)
       ;; Similar to the three `extern-lang' symbols, but for
       ;; C++ "namespace" blocks.

       (module-open           . 0)

       (module-close          . 0)

       (inmodule              . +)
       ;; Similar to the three `extern-lang' symbols, but for
       ;; CORBA IDL "module" blocks.

       (composition-open      . 0)

       (composition-close     . 0)

       (incomposition         . +)
       ;; Similar to the three `extern-lang' symbols, but for
       ;; CORBA CIDL "composition" blocks.

       (template-args-cont    . (c-lineup-template-args +))
       ;; C++ template argument list continuations.

       (inlambda              . +)
       ;; In the header or body of a lambda function.

       (lambda-intro-cont     . ++)
       ;; Continuation of the header of a lambda function.

       (inexpr-statement      . +)
       ;; The statement is inside an expression.

       (inexpr-class          . +)
       ;; The class is inside an expression.  Used e.g. for
       ;; Java anonymous classes.
       ))

   ;; Only called when c-syntactic-indentation is non nil.
   ;; (c-special-indent-hook . user-fun)
   (c-label-minimum-indentation    . 8)

   ;; other emacs variables:
   ;; (c-comment-continuation-stars "" t)
   ;; (c-echo-syntactic-information-p t)
   ;; (c-hanging-comment-ender-p nil t)
   ;; (c-hanging-comment-starter-p nil t)
   ;; (c-macro-shrink-window-flag          . t)


   (tab-width                      . 8)  ; the true one!

   (c-indent-level                 . 8)  ; Indentation of C statements with
                                        ; respect to containing block.

   (c-brace-imaginary-offset       . 0)  ; Imagined indentation of a C open brace
                                        ; that actually follows a statement.

   (c-brace-offset                 . 0)  ; Extra indentation for braces, compared
                                        ; with other text in same context.

   (c-argdecl-indent               . 8)  ; Indentation level of declarations of
                                        ; C function arguments.

   (c-label-offset                 . -)  ; Offset of C label lines and case
                                        ; statements relative to usual
                                        ; indentation.

   (c-continued-statement-offset   . 8)  ; Extra indent for lines not starting
                                        ; new statements.

   (c-continued-brace-offset       . 0)  ; Extra indent for substatements that
                                        ; start with open-braces.


   (c-auto-newline                . nil) ; Non-nil means automatically newline
                                        ; before and after braces, and after
                                        ; colons and semicolons, inserted in C
                                        ; code. If you do not want a leading
                                        ; newline before braces then use:
                                        ; (define-key c-mode-map \"{\"
                                        ;          'electric-c-semi)"

   (c-tab-always-indent           . t)  ; Non-nil means TAB in C mode should
                                        ; always reindent the current line,
                                        ; regardless of where in the line point
                                        ; is when the TAB command is used.
   ))

;; ,  -> remove previous spaces; add following space;
;; (  -> remove previous spaces; insert one previous space;
;; {  -> insert one previous newline; indent;
;; }  -> insert one previous newline; indent; add one following newline; indent;
;; =  -> if previous is [-+!<=>&|^*/]
;;       then remove previous spaces; add following space;
;;       else add following space;
;; ^  -> add following space
;; */ -> remove previous spaces; if previous is not other then insert one previous space; end; add following space;
;; -+&| -> if previous is same
;;         then remove previous spaces; add following space;
;;         else add following space;

(defun spacep (ch)
  (string-match "[[:blank:]]"  (string ch)))

(defun* freerdp-remove-previous-spaces (&optional (from (point)))
  (save-excursion
   (goto-char from)
   (while (spacep (char-before))
          (delete-region (1- (point)) (point)))))


(defun freerdp-electric-space-after (repeat)
  (interactive "p")
  (if (= 1 repeat)
      (progn
        (freerdp-remove-previous-spaces)
        (self-insert-command 1)
        (insert " "))
       (self-insert-command repeat)))

(defun freerdp-electric-space-before (repeat)
  (interactive "p")
  (if (= 1 repeat)
      (progn
        (freerdp-remove-previous-spaces)
        (insert " ")
        (self-insert-command 1))
       (self-insert-command repeat)))

(defun freerdp-electric-space-before-after (repeat)
  (interactive "p")
  (if (= 1 repeat)
      (progn
        (freerdp-remove-previous-spaces)
        (insert " ")
        (self-insert-command 1)
        (insert " "))
       (self-insert-command repeat)))


(defun freerdp-electric-space-before-after-double (repeat)
  (interactive "p")
  (let ((ch (this-command-keys)))
   (if (and (= 1 repeat) (stringp ch) (= 1 (length ch)))
       (progn
         (freerdp-remove-previous-spaces)
         (when (or (/= (aref ch 0) (char-before))
                   (=  (aref ch 0) (char-before (1- (point)))))
           (insert " "))
         (self-insert-command 1)
         (insert " "))
       (self-insert-command repeat))))

(defun freerdp-electric-space-before-after-= (repeat)
  (interactive "p")
  (if (= 1 repeat)
       (let ((=-prefixes "-+!<=>&|^*/"))
         (freerdp-remove-previous-spaces)
         (when  (if (= ?= (char-before))
                    (position (char-before (1- (point))) =-prefixes)
                    (not (position (char-before) =-prefixes)))
           (insert " "))
         (self-insert-command 1)
         (insert " "))
       (self-insert-command repeat)))

(defun* freerdp-reverse-skip-spaces (&optional (from (point)))
  (while (spacep (char-before from))
         (decf from))
  from)


(defun freerdp-electric-space-before-after-> (repeat)
  "Same as freerdp-electric-space-before-after-double
unless -> in which case we remove the spaces."
  (interactive "p")
  (if (= 1 repeat)
      (if (save-excursion (beginning-of-line)
                          (thing-at-point-looking-at "[\t ]*#"))
          (self-insert-command repeat)
          (let ((space-after t))
            ;;  x - - | x--  | x-- > | x-- >
            ;;  x - > | x->a |
            ;;  x>    | x >  |
            (freerdp-remove-previous-spaces)
            (if (= ?- (char-before))
                (let ((pos (freerdp-reverse-skip-spaces (1- (point)))))
                  (if (= ?- (char-before pos))
                      (insert " ")
                      (progn
                        (freerdp-remove-previous-spaces (- (point) 1))
                        (freerdp-remove-previous-spaces (- (point) 2))
                        (setf space-after nil))))
                (insert " "))
            (self-insert-command 1)
            (when space-after (insert " "))))
      (self-insert-command repeat)))

(defun freerdp-electric-space-before-after-*/ (repeat)
  (interactive "p")
  (if (= 1 repeat)
       (let ((ch (this-command-keys)))
         (freerdp-remove-previous-spaces)
         (when (cond
                 ((string= "*" ch) (and (/= ?/  (char-before))
                                        (/= ?\( (char-before))))
                 ((string= "/" ch) (and (/= ?*  (char-before))
                                        (/= ?/  (char-before))))
                 (t                t))
           (insert " "))
         (self-insert-command 1)
         (unless (and (string= "*" ch) (looking-at "[()]"))
           (insert " ")))
       (self-insert-command repeat)))



(defun freerdp-electric-brace-open (repeat)
  (interactive "p")
  (if (= 1 repeat)
      (let ((start (point)))
        (freerdp-remove-previous-spaces)
        (insert "\n")
        (self-insert-command 1)
        (insert "\n\n}\n")
        (indent-region start (point))
        (previous-line 2)
        (c-indent-or-tab))
       (self-insert-command repeat)))

(defun freerdp-electric-brace-close (repeat)
  (interactive "p")
  (if (= 1 repeat)
      (progn
        (if (search-forward "}" nil t)
            (forward-char)
            (let ((start (point)))
              (insert "\n}\n")
              (indent-region start (point)))))
       (self-insert-command repeat)))

(defun freerdp-c-keyword-p (string-designator)
   (intern-soft (etypecase string-designator
                    (symbol    (symbol-name (symbol-at-point)))
                    (string    string-designator)
                    (character (string string-designator)))
                c-keywords-obarray))

(defun freerdp-electric-special-character-p (ch)
  (string-match "[[:punct:]]" (string ch)))

(defvar freerdp-electric-identifier-character-regexp "[[:alnum:]_]")


(defun freerdp-electric-paren-open (repeat)
  (interactive "p")
  (if (= 1 repeat)
      (progn
        (if (and (looking-at freerdp-electric-identifier-character-regexp)
                 ;; (looking-back freerdp-electric-identifier-character-regexp (- (point) 1))
                 )
            (insert "()")
            (progn
              (freerdp-remove-previous-spaces)
              (let ((current (point)))
                (cond
                  ((bolp)
                   (insert "()"))
                  (t
                   (backward-sexp)
                   (if (prog1 (freerdp-c-keyword-p (symbol-at-point))
                         (forward-sexp))
                       (insert " ()")
                       (insert "()")))))))
        (backward-char))
      (self-insert-command repeat)))

(defun freerdp-electric-paren-close (repeat)
  (interactive "p")
  (if (= 1 repeat)
      (progn
        (if (search-forward ")" nil t)
            (progn) ;;            (forward-char)
            (insert ")")))
       (self-insert-command repeat)))


(defun freerdp-style-set-local-bindings ()
  (interactive)
  (setf indent-tabs-mode t
        tab-width        8)
  (local-set-key "," 'freerdp-electric-space-after)
  (local-set-key "=" 'freerdp-electric-space-before-after-=)
  (local-set-key ">" 'freerdp-electric-space-before-after->)
  (dolist (key '("<" "+" "-" "&" "|"))
    (local-set-key key 'freerdp-electric-space-before-after-double))
  (dolist (key '("*" "/"))
    (local-set-key key 'freerdp-electric-space-before-after-*/))
  (dolist (key '("%" "^"))
    (local-set-key key 'freerdp-electric-space-before-after))
  (local-set-key "{" 'freerdp-electric-brace-open)
  (local-set-key "}" 'freerdp-electric-brace-close)
  (local-set-key "(" 'freerdp-electric-paren-open)
  (local-set-key ")" 'freerdp-electric-paren-close))

(provide 'freerdp-c-style)
;;;; THE END ;;;;
