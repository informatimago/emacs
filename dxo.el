;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dxo.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Emacs configuration used at DxO Labs / Optics Pro Mac team.
;;;;
;;;;    Add to your ~/.emacs:
;;;;
;;;;         (require 'dxo)
;;;;
;;;;    Then new C, Objective-C or C++ buffers will get the dxo style.
;;;;
;;;;    To change the c-style manually:
;;;;
;;;;        M-x c-set-style RET dxo RET
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-11-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(require 'cl) ; always
(require 'cc-mode)


(setf auto-mode-alist (append '(("OpticsProMac.*\\.h$" . objc-mode)
                                ("\\.m$"  . objc-mode)
                                ("\\.mm$" . objc-mode))
                              auto-mode-alist))


(c-add-style
 "dxo"
 '((c-backslash-column             .  72)
   
   (c-backslash-max-column         . 128)
   
   (c-basic-offset                 .   2)
   
   (c-block-comment-prefix         . "* ")
   
   (c-cleanup-list                 . (list-close-comma
                                      scope-operator))
   
   (c-comment-only-line-offset     . (0 . 0))
   
   ;; (c-comment-prefix-regexp)

   (c-doc-comment-style            . javadoc)

   (c-hanging-braces-alist         . (
                                      ;; (statement-cont)
                                      ;; (brace-list-intro)
                                      (inexpr-class-open    . (before after))
                                      (inexpr-class-close   . (before after))
                                      
                                      (defun-open           . (before after)) ; Brace that opens a function definition.
                                      (defun-close          . (before after)) ; Brace that closes a function definition.
                                      (class-open           . (before after)) ; Brace that opens a class definition.
                                      (class-close          . (before after)) ; Brace that closes a class definition.
                                      (inline-open          . (before after)) ; Brace that opens an in-class inline method.
                                      (inline-close         . (before after)) ; Brace that closes an in-class inline method. 
                                      (block-open           . (before after)) ; Statement block open brace.
                                      (block-close          . (before after)) ; Statement block close brace.
                                      (brace-list-open      . (before after)) ; Open brace of an enum or static array list.
                                      (brace-list-close     . (before after)) ; Close brace of an enum or static array list.
                                      (brace-entry-open     . (before after)) ; Subsequent lines in an enum or static array
                                      (statement-case-open  . (before after)) ; The first line in a case block starting with brace.
                                      (substatement-open    . (before after)) ; The brace that opens a substatement block.
                                      (extern-lang-open     . (before after)) ; Brace that opens an "extern" block.
                                      (extern-lang-close    . (before after)) ; Brace that closes an "extern" block.
                                      (namespace-open       . (before after))
                                      (namespace-close      . (before after))
                                      (module-open          . (before after))
                                      (module-close         . (before after))
                                      (composition-open     . (before after))
                                      (composition-close)   . (before after)))

   (c-hanging-colons-alist         . ((case-label           . (after))
                                      (label                . (after))
                                      (access-label         . (after))
                                      (member-init-intro    . ())
                                      (inher-intro          . ())))
   
   (c-hanging-semi&comma-criteria  . ())
   
   ;; (c-indent-comment-alist)

   (c-indent-comments-syntactically-p . nil)
   
   (c-label-minimum-indentation       . 2)

   (c-offsets-alist                   . (
                                         (string             . 0)                 
                                         ;; Inside multi-line string.
                                         (c                  . 1)                      
                                         ;; Inside a multi-line C style block comment.
                                         (defun-open         . 0)             
                                         ;; Brace that opens a function definition.
                                         (defun-close        . 0)            
                                         ;; Brace that closes a function definition.
                                         (defun-block-intro  . +)      
                                         ;; The first line in a top-level defun.
                                         (class-open         . 0)             
                                         ;; Brace that opens a class definition.
                                         (class-close        . 0)            
                                         ;; Brace that closes a class definition.
                                         (inline-open        . 0)            
                                         ;; Brace that opens an in-class inline method.
                                         (inline-close       . 0)           
                                         ;; Brace that closes an in-class inline method.
                                         (func-decl-cont     . +)         
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
                                         (topmost-intro-cont    . 0)     
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
                                         (statement             . 0)
                                         ;; A C (or like) statement.
                                         (statement-cont        . ++)         
                                         ;; A continuation of a C (or like) statement.
                                         (statement-block-intro . +)  
                                         ;; The first line in a new statement block.
                                         (statement-case-intro  . +)   
                                         ;; The first line in a case "block".
                                         (statement-case-open   . +)    
                                         ;; The first line in a case block starting with brace.
                                         (substatement          . 0)           
                                         ;; The first line after an if/while/for/do/else.
                                         (substatement-open     . 0)
                                         ;; The brace that opens a substatement block.
                                         (substatement-label    . /)     
                                         ;; Labelled line after an if/while/for/do/else.
                                         (case-label            . +)             
                                         ;; A "case" or "default" label.
                                         (access-label          . 0)           
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
                                         (arglist-intro         . +)          
                                         ;; The first line in an argument list.
                                         (arglist-cont          . +)           
                                         ;; Subsequent argument list lines when no
                                         ;; arguments follow on the same line as the
                                         ;; arglist opening paren.
                                         (arglist-cont-nonempty . +)  
                                         ;; Subsequent argument list lines when at
                                         ;; least one argument follows on the same
                                         ;; line as the arglist opening paren.
                                         (arglist-close         . 0)          
                                         ;; The solo close paren of an argument list.
                                         (stream-op             . +)              
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
                                         (objc-method-intro     . [0])      
                                         ;; The first line of an Objective-C method definition.
                                         (objc-method-args-cont . 0)  
                                         ;; Lines continuing an Objective-C method definition.
                                         (objc-method-call-cont . ++)  
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
                                         (template-args-cont    . +)     
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

   
   (tab-width                     . 4)  ; the true one!

   (c-indent-level                . 2)  ; Indentation of C statements with
                                        ; respect to containing block.

   (c-brace-imaginary-offset      . 0)  ; Imagined indentation of a C open brace
                                        ; that actually follows a statement.

   (c-brace-offset                . 0)  ; Extra indentation for braces, compared
                                        ; with other text in same context.

   (c-argdecl-indent              . +)  ; Indentation level of declarations of
                                        ; C function arguments.

   (c-label-offset                . -)  ; Offset of C label lines and case
                                        ; statements relative to usual 
                                        ; indentation.

   (c-continued-statement-offset  . 4)  ; Extra indent for lines not starting
                                        ; new statements.

   (c-continued-brace-offset      . 0)  ; Extra indent for substatements that
					; start with open-braces.


   (c-auto-newline                . t)  ; Non-nil means automatically newline
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



(defun dxo-c-mode-common-meat ()
  (interactive)
  (c-set-style "dxo")
  (c-toggle-auto-newline 1))

(add-hook 'c-mode-common-hook 'dxo-c-mode-common-meat)



(defun dxo-company-name ()
  "Substitute __MyCompanyName__ by DxO Consumer in the whole current buffer."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "__MyCompanyName__" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert "DxO Labs"))
  (goto-char (point-min))
  (message "Did you mind setting the company name in XCode preferences?"))


(defun dxo-remove-unneeded-spaces (start end)
  "Remove duplicate spaces in the region. (Inverse function of align-cols"
  (interactive "r")
  (goto-char start)
  (with-marker (end end)
   (while (< (point) end)
     (cond

       ((looking-at "\"\\([^\\\"]\\|\\.\\)*\"")
        (message "string ")
        ;; skip over strings
        (goto-char (match-end 0)))

       ((looking-at "//.*\n")
        (message "//comment ")
        ;; skip over // comments
        (goto-char (match-end 0)))

       ((looking-at "/\\*\\(.\\|\n\\)*?\\*/")
        (message "/*comment ")
        ;; skip over C comments..
        (goto-char (match-end 0)))

       ((looking-at "  +")
        (message "whitespaces ")
        (delete-region (1+ (match-beginning 0)) (match-end 0)))

       (t
        (message ". ")
        (forward-char 1))))
   (indent-region start end)))



(provide 'dxo)
;;;; THE END ;;;;

