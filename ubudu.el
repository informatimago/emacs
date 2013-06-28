;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ubudu.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Emacs configuration used at Ubudu SAS.
;;;;
;;;;    Add to your ~/.emacs:
;;;;
;;;;         (require 'ubudu)
;;;;
;;;;    Then new Java buffers will get the ubudu style.
;;;;
;;;;    To change the c-style manually:
;;;;
;;;;        M-x c-set-style RET ubudu RET
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2013-06-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal Bourguignon 2013 - 2013
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


(c-add-style
 "ubudu"
 '((c-backslash-column             .  72)

   (c-backslash-max-column         . 128)

   (c-basic-offset                 . 2)  ; Amount of basic offset used by `+' and
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
       (inexpr-class-open    . (after))
       (inexpr-class-close   . (before))

       (defun-open           . (after))         ; Brace that opens a function definition.
       (defun-close          . (before))        ; Brace that closes a function definition.
       (class-open           . (before after))  ; Brace that opens a class definition.
       (class-close          . ())              ; Brace that closes a class definition.
       (inline-open          . (after))         ; Brace that opens an in-class inline method.
       (inline-close         . (before))        ; Brace that closes an in-class inline method.
       (block-open           . ())              ; Statement block open brace.
       (block-close          . c-snug-do-while) ; Statement block close brace.
       (brace-list-open      . (after))         ; Open brace of an enum or static array list.
       (brace-list-close     . (before after))  ; Close brace of an enum or static array list.
       (brace-entry-open     . (before after))  ; Subsequent lines in an enum or static array
       (statement-case-open  . (after))         ; The first line in a case block starting with brace.
       (substatement-open    . (after))         ; The brace that opens a substatement block.
       (extern-lang-open     . (after))         ; Brace that opens an "extern" block.
       (extern-lang-close    . (before after))  ; Brace that closes an "extern" block.
       (namespace-open       . (after))
       (namespace-close      . ())
       (module-open          . (after))
       (module-close         . ())
       (composition-open     . (after))
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
       (annotation-var-cont   . 0)

       (string                . 0)
       ;; Inside multi-line string.

       (c                     . 1)
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
       
       (brace-entry-open      . +)
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
       
       (case-label            . *)
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
       
       (arglist-intro         . (;; c-lineup-arglist-intro-after-paren
                                 ++))
       ;; The first line in an argument list.
       
       (arglist-cont          . (c-lineup-string-cont
                                 c-lineup-arglist-intro-after-paren
                                 c-lineup-argcont
                                 ))
       ;; Subsequent argument list lines when no
       ;; arguments follow on the same line as the
       ;; arglist opening paren.
       
       (arglist-cont-nonempty . (c-lineup-string-cont
                                 c-lineup-arglist-intro-after-paren
                                 c-lineup-argcont
                                 0))
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
   (c-label-minimum-indentation    . 2)

   ;; other emacs variables:
   ;; (c-comment-continuation-stars "" t)
   ;; (c-echo-syntactic-information-p t)
   ;; (c-hanging-comment-ender-p nil t)
   ;; (c-hanging-comment-starter-p nil t)
   ;; (c-macro-shrink-window-flag          . t)


   (tab-width                      . 4)  ; the true one!

   (c-indent-level                 . +)  ; Indentation of C statements with
                                        ; respect to containing block.

   (c-brace-imaginary-offset       . 0)  ; Imagined indentation of a C open brace
                                        ; that actually follows a statement.

   (c-brace-offset                 . 0)  ; Extra indentation for braces, compared
                                        ; with other text in same context.

   (c-argdecl-indent               . ++)  ; Indentation level of declarations of
                                        ; C function arguments.

   (c-label-offset                 . -)  ; Offset of C label lines and case
                                        ; statements relative to usual
                                        ; indentation.

   (c-continued-statement-offset   . ++)  ; Extra indent for lines not starting
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



(defun ubudu-c-mode-common-meat ()
  (interactive)
  (c-set-style "ubudu")
  (c-toggle-auto-newline 1)
  (setf c-basic-offset 2))

(add-hook 'c-mode-common-hook 'ubudu-c-mode-common-meat)

(provide 'ubudu)
;;;; THE END ;;;;
