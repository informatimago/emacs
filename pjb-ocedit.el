(require 'cc-mode)


;; (dolist (var '(c-basic-offset c-comment-only-line-offset c-indent-comment-alist c-indent-comments-syntactically-p c-block-comment-prefix c-comment-prefix-regexp c-doc-comment-style c-cleanup-list c-hanging-braces-alist c-hanging-colons-alist c-hanging-semi&comma-criteria c-backslash-column c-backslash-max-column c-special-indent-hook c-label-minimum-indentation c-offsets-alist))
;;   (insert (format ";; %-30s = %S\n" var (symbol-value var))))
;; 
;; (dolist (var (sort '(c-basic-offset c-comment-only-line-offset c-indent-comment-alist c-indent-comments-syntactically-p c-block-comment-prefix c-comment-prefix-regexp c-doc-comment-style c-cleanup-list c-hanging-braces-alist c-hanging-colons-alist c-hanging-semi&comma-criteria c-backslash-column c-backslash-max-column c-special-indent-hook c-label-minimum-indentation c-offsets-alist)
;;                (function string<)))
;;   (insert (format "'%S\n" `(,var set-from-style))))


(c-add-style
 "pjb"
 '((c-backslash-column             .  72)
   (c-backslash-max-column         . 128)
   (c-basic-offset                 .   4)
   (c-block-comment-prefix         .  "")
   (c-cleanup-list                 . (brace-else-brace
                                      brace-elseif-brace
                                      brace-catch-brace
                                      list-close-comma
                                      scope-operator))
   (c-comment-only-line-offset     . (0 . 0))
   ;; (c-comment-prefix-regexp)
   (c-doc-comment-style            . javadoc)
   (c-hanging-braces-alist         . (
                                      ;; (statement-cont)
                                      ;; (brace-list-intro)
                                      (inexpr-class-open    . (after))
                                      (inexpr-class-close   . (before))
                                      
                                      (defun-open           . (after))         ; Brace that opens a function definition.
                                      (defun-close          . (before))        ; Brace that closes a function definition.
                                      (class-open           . (before after))  ; Brace that opens a class definition.
                                      (class-close          . ())              ; Brace that closes a class definition.
                                      (inline-open          . (before after))  ; Brace that opens an in-class inline method.
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
   ;; (c-hanging-semi&comma-criteria)
   ;; (c-indent-comment-alist)
   (c-indent-comments-syntactically-p . nil)
   (c-label-minimum-indentation       . 2)
   
   (c-offsets-alist                   . (
                                         (string             . 0)                 
                                         ;; Inside multi-line string.
                                         (c                  . 0)                      
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
                                         (knr-argdecl-intro  . +)      
                                         ;; First line of a K&R C argument declaration.
                                         (knr-argdecl        . +)            
                                         ;; Subsequent lines in a K&R C argument declaration.
                                         (topmost-intro      . 0)          
                                         ;; The first line in a topmost construct definition.
                                         (topmost-intro-cont . ++)     
                                         ;; Topmost definition continuation lines.
                                         (member-init-intro  . 0)      
                                         ;; First line in a member initialization list.
                                         (member-init-cont   . ++)       
                                         ;; Subsequent member initialization list lines.
                                         (inher-intro        . +)            
                                         ;; First line of a multiple inheritance list.
                                         (inher-cont         . ++)             
                                         ;; Subsequent multiple inheritance lines.
                                         (block-open         . 0)             
                                         ;; Statement block open brace.
                                         (block-close        . 0)            
                                         ;; Statement block close brace.
                                         (brace-list-open    . 0)        
                                         ;; Open brace of an enum or static array list.
                                         (brace-list-close   . 0)       
                                         ;; Close brace of an enum or static array list.
                                         (brace-list-intro   . +)       
                                         ;; First line in an enum or static array list.
                                         (brace-list-entry   . +)       
                                         ;; Subsequent lines in an enum or static array list.
                                         (brace-entry-open   . +)       
                                         ;; Subsequent lines in an enum or static array
                                         ;; list that start with an open brace.
                                         (statement          . +)              
                                         ;; A C (or like) statement.
                                         (statement-cont     . 0)         
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
                                         (case-label            . 0)             
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
                                         (comment-intro         . +)          
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
                                         (objc-method-intro     . 0)      
                                         ;; The first line of an Objective-C method definition.
                                         (objc-method-args-cont . 0)  
                                         ;; Lines continuing an Objective-C method definition.
                                         (objc-method-call-cont . +)  
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
   (c-label-minimum-indentation        . 2)

   ;; other emacs variables:
   ;; (c-brace-imaginary-offset 0 t)
   ;; (c-brace-offset 0 t)
   ;; (c-comment-continuation-stars "" t)
   ;; (c-continued-brace-offset 0 t)
   ;; (c-continued-statement-offset 4 t)
   ;; (c-echo-syntactic-information-p t)
   ;; 
   ;; (c-hanging-comment-ender-p nil t)
   ;; (c-hanging-comment-starter-p nil t)
   ;; (c-indent-level                      . 4)
   ;; 
   ;; (c-label-offset                      . -)
   ;; (c-macro-shrink-window-flag          . t) 
   ;; (c-tab-always-indent                 . t))

 )



;; (c-cleanup-list brace-else-brace brace-elseif-brace brace-catch-brace list-close-comma scope-operator)



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
   
   ;; (c-hanging-semi&comma-criteria)
   ;; (c-indent-comment-alist)
   (c-indent-comments-syntactically-p . nil)
   (c-label-minimum-indentation       . 2)

   (c-offsets-alist                   . (
                                         (string             . 0)                 
                                         ;; Inside multi-line string.
                                         (c                  . 0)                      
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
                                         (knr-argdecl-intro  . +)      
                                         ;; First line of a K&R C argument declaration.
                                         (knr-argdecl        . +)            
                                         ;; Subsequent lines in a K&R C argument declaration.
                                         (topmost-intro      . +)          
                                         ;; The first line in a topmost construct definition.
                                         (topmost-intro-cont . ++)     
                                         ;; Topmost definition continuation lines.
                                         (member-init-intro  . +)      
                                         ;; First line in a member initialization list.
                                         (member-init-cont   . ++)       
                                         ;; Subsequent member initialization list lines.
                                         (inher-intro        . +)            
                                         ;; First line of a multiple inheritance list.
                                         (inher-cont         . ++)             
                                         ;; Subsequent multiple inheritance lines.
                                         (block-open         . 0)             
                                         ;; Statement block open brace.
                                         (block-close        . 0)            
                                         ;; Statement block close brace.
                                         (brace-list-open    . 0)        
                                         ;; Open brace of an enum or static array list.
                                         (brace-list-close   . 0)       
                                         ;; Close brace of an enum or static array list.
                                         (brace-list-intro   . +)       
                                         ;; First line in an enum or static array list.
                                         (brace-list-entry   . +)       
                                         ;; Subsequent lines in an enum or static array list.
                                         (brace-entry-open   . +)       
                                         ;; Subsequent lines in an enum or static array
                                         ;; list that start with an open brace.
                                         (statement          . +)              
                                         ;; A C (or like) statement.
                                         (statement-cont     . ++)         
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
                                         (case-label            . 0)             
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
                                         (comment-intro         . +)          
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
                                         (objc-method-call-cont . +)  
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
   ))



;; (c-offsets-alist
;;  (topmost-intro . 0)
;;  (inclass . c-basic-offset)
;;  (cpp-macro . [0])
;;  (objc-method-intro . [0])
;;  (defun-block-intro . c-basic-offset)
;;  (statement-block-intro . c-basic-offset)
;;  (access-label . -4)
;;  (defun-open . 0)
;;  (topmost-intro . 0)
;;  (member-init-cont . 0)
;;  (statement-cont . 0)
;;  (inclass . 4)
;;  (cpp-macro . -1000)
;;  (objc-method-intro . 0))
;; 
;; (c-label-minimum-indentation . 2)
;; 
;; (c-special-indent-hook)
;; 
;; 
;; 
;; (c-hanging-braces-alist
;;  (defun-open before after)
;;  (defun-close before)
;;  (class-open before after)
;;  (inline-open before after)
;;  (inline-close before)
;;  (block-open)
;;  (block-close . c-snug-do-while)
;;  (substatement-open after)
;;  (statement-case-open after)
;;  (extern-lang-open after)
;;  (extern-lang-close before after)
;;  (brace-list-open after)
;;  (brace-list-close before after)
;;  (brace-list-intro before after)
;;  (brace-entry-open before after)
;;  (inexpr-class-open after)
;;  (inexpr-class-close before))
;; 
;; (c-cleanup-list brace-else-brace brace-elseif-brace brace-catch-brace list-close-comma scope-operator)
;; 
;; (c-comment-only-line-offset 0 . 0)
;; 
;; (c-basic-offset . 4)
;; 
;; 
;; 
;; (indent-tabs-mode fill-column)


;; c-basic-offset                 = 4
;; c-comment-only-line-offset     = (0 . 0)
;; c-indent-comment-alist         = set-from-style
;; c-indent-comments-syntactically-p = set-from-style
;; c-block-comment-prefix         = set-from-style
;; c-comment-prefix-regexp        = set-from-style
;; c-doc-comment-style            = set-from-style
;; c-cleanup-list                 = (brace-else-brace brace-elseif-brace brace-catch-brace list-close-comma scope-operator)
;; c-hanging-braces-alist         = ((defun-open before after) (defun-close before) (class-open before after) (inline-open before after) (inline-close before) (block-open) (block-close . c-snug-do-while) (substatement-open after) (statement-case-open after) (extern-lang-open after) (extern-lang-close before after) (brace-list-open after) (brace-list-close before after) (brace-list-intro before after) (brace-entry-open before after) (inexpr-class-open after) (inexpr-class-close before))
;; c-hanging-colons-alist         = set-from-style
;; c-hanging-semi&comma-criteria  = set-from-style
;; c-backslash-column             = 72
;; c-backslash-max-column         = set-from-style
;; c-special-indent-hook          = nil
;; c-label-minimum-indentation    = 2
;; c-offsets-alist                = ((topmost-intro . 0) (inclass . c-basic-offset) (cpp-macro . [0]) (objc-method-intro . [0]) (defun-block-intro . c-basic-offset) (statement-block-intro . c-basic-offset) (access-label . -4) (defun-open . 0) (topmost-intro . 0) (member-init-cont . 0) (statement-cont . 0) (inclass . 4) (cpp-macro . -1000) (objc-method-intro . 0))



;; ;; to modify c-style-alist use c-add-style
;; (c-add-style style description &optional set-p)
;; ;; description =   ([BASESTYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])
;; ;; set-p ==> (c-set-style style)
;; 
;; c-indentation-style
;; c-special-indent-hook
;; 
;; c-offsets-alist
;; ;; ((topmost-intro . 0) (inclass . c-basic-offset) (cpp-macro . [0]) (objc-method-intro . [0]) (defun-block-intro . c-basic-offset) (statement-block-intro . c-basic-offset) (access-label . -4) (defun-open . 0) (topmost-intro . 0) (member-init-cont . 0) (statement-cont . 0) (inclass . 4) (cpp-macro . -1000) (objc-method-intro . 0))
;; 
;; 
;; (c-set-style STYLENAME &optional DONT-OVERRIDE)
;; (c-set-offset symbol offset)

;; ("user" (c-offsets-alist (topmost-intro . 0) (inclass . c-basic-offset) (cpp-macro . [0]) (objc-method-intro . [0]) (defun-block-intro . c-basic-offset) (statement-block-intro . c-basic-offset) (access-label . -4) (defun-open . 0) (topmost-intro . 0) (member-init-cont . 0) (statement-cont . 0) (inclass . 4) (cpp-macro . -1000) (objc-method-intro . 0)) (c-label-minimum-indentation . 2) (c-special-indent-hook) (c-backslash-column . 72) (c-hanging-braces-alist (defun-open before after) (defun-close before) (class-open before after) (inline-open before after) (inline-close before) (block-open) (block-close . c-snug-do-while) (substatement-open after) (statement-case-open after) (extern-lang-open after) (extern-lang-close before after) (brace-list-open after) (brace-list-close before after) (brace-list-intro before after) (brace-entry-open before after) (inexpr-class-open after) (inexpr-class-close before)) (c-cleanup-list brace-else-brace brace-elseif-brace brace-catch-brace list-close-comma scope-operator) (c-comment-only-line-offset 0 . 0) (c-basic-offset . 4))
;; 
;; ("gnu" (c-basic-offset . 2) (c-comment-only-line-offset 0 . 0) (c-hanging-braces-alist (substatement-open before after) (arglist-cont-nonempty)) (c-offsets-alist (statement-block-intro . +) (knr-argdecl-intro . 5) (substatement-open . +) (substatement-label . 0) (label . 0) (statement-case-open . +) (statement-cont . +) (arglist-intro . c-lineup-arglist-intro-after-paren) (arglist-close . c-lineup-arglist) (inline-open . 0) (brace-list-open . +) (topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont)) (c-special-indent-hook . c-gnu-impose-minimum) (c-block-comment-prefix . ""))
;; 
;; ("k&r" (c-basic-offset . 5) (c-comment-only-line-offset . 0) (c-offsets-alist (statement-block-intro . +) (knr-argdecl-intro . 0) (substatement-open . 0) (substatement-label . 0) (label . 0) (statement-cont . +)))
;; 
;; ("bsd" (c-basic-offset . 8) (c-comment-only-line-offset . 0) (c-offsets-alist (statement-block-intro . +) (knr-argdecl-intro . +) (substatement-open . 0) (substatement-label . 0) (label . 0) (statement-cont . +) (inline-open . 0) (inexpr-class . 0)))
;; 
;; ("stroustrup" (c-basic-offset . 4) (c-comment-only-line-offset . 0) (c-offsets-alist (statement-block-intro . +) (substatement-open . 0) (substatement-label . 0) (label . 0) (statement-cont . +)))
;; 
;; ("whitesmith" (c-basic-offset . 4) (c-comment-only-line-offset . 0) (c-offsets-alist (defun-open . +) (defun-close . c-lineup-whitesmith-in-block) (defun-block-intro add c-lineup-whitesmith-in-block c-indent-multi-line-block) (class-open . +) (class-close . +) (inline-open . +) (inline-close . c-lineup-whitesmith-in-block) (knr-argdecl-intro . +) (block-open . 0) (block-close . c-lineup-whitesmith-in-block) (brace-list-open . +) (brace-list-close . c-lineup-whitesmith-in-block) (brace-list-intro add c-lineup-whitesmith-in-block c-indent-multi-line-block) (brace-list-entry add c-lineup-after-whitesmith-blocks c-indent-multi-line-block) (brace-entry-open add c-lineup-after-whitesmith-blocks c-indent-multi-line-block) (statement add c-lineup-after-whitesmith-blocks c-indent-multi-line-block) (statement-block-intro add c-lineup-whitesmith-in-block c-indent-multi-line-block) (substatement-open . +) (substatement-label . +) (label . 0) (arglist-intro add c-lineup-whitesmith-in-block c-indent-multi-line-block) (arglist-cont add c-lineup-after-whitesmith-blocks c-indent-multi-line-block) (arglist-cont-nonempty add c-lineup-whitesmith-in-block c-indent-multi-line-block) (arglist-close . c-lineup-whitesmith-in-block) (inclass . c-lineup-whitesmith-in-block) (extern-lang-open . +) (namespace-open . +) (module-open . +) (composition-open . +) (extern-lang-close . +) (namespace-close . +) (module-close . +) (composition-close . +) (inextern-lang . c-lineup-whitesmith-in-block) (innamespace . c-lineup-whitesmith-in-block) (inmodule . c-lineup-whitesmith-in-block) (incomposition . c-lineup-whitesmith-in-block) (inexpr-class . 0)))
;; 
;; ("ellemtel" (c-basic-offset . 3) (c-comment-only-line-offset . 0) (c-hanging-braces-alist (substatement-open before after) (arglist-cont-nonempty)) (c-offsets-alist (topmost-intro . 0) (substatement . +) (substatement-open . 0) (case-label . +) (access-label . -) (inclass . +) (inline-open . 0)))
;; 
;; ("linux" (c-basic-offset . 8) (c-comment-only-line-offset . 0) (c-hanging-braces-alist (brace-list-open) (brace-entry-open) (substatement-open after) (block-close . c-snug-do-while) (arglist-cont-nonempty)) (c-cleanup-list brace-else-brace) (c-offsets-alist (statement-block-intro . +) (knr-argdecl-intro . 0) (substatement-open . 0) (substatement-label . 0) (label . 0) (statement-cont . +)))
;; 
;; ("python" (indent-tabs-mode . t) (fill-column . 78) (c-basic-offset . 8) (c-offsets-alist (substatement-open . 0) (inextern-lang . 0) (arglist-intro . +) (knr-argdecl-intro . +)) (c-hanging-braces-alist (brace-list-open) (brace-list-intro) (brace-list-close) (brace-entry-open) (substatement-open after) (block-close . c-snug-do-while) (arglist-cont-nonempty)) (c-block-comment-prefix . ""))
;; 
;; ("java" (c-basic-offset . 4) (c-comment-only-line-offset 0 . 0) (c-offsets-alist (inline-open . 0) (topmost-intro-cont . +) (statement-block-intro . +) (knr-argdecl-intro . 5) (substatement-open . +) (substatement-label . +) (label . +) (statement-case-open . +) (statement-cont . +) (arglist-intro . c-lineup-arglist-intro-after-paren) (arglist-close . c-lineup-arglist) (access-label . 0) (inher-cont . c-lineup-java-inher) (func-decl-cont . c-lineup-java-throws)))
;; 
;; ("awk" (c-basic-offset . 4) (c-comment-only-line-offset . 0) (c-hanging-braces-alist (defun-open after) (defun-close . c-snug-1line-defun-close) (substatement-open after) (block-close . c-snug-do-while) (arglist-cont-nonempty)) (c-hanging-semi&comma-criteria) (c-cleanup-list) (c-offsets-alist (statement-block-intro . +) (substatement-open . 0) (statement-cont . +)))
;; 
;; nil






;; Objective-C Coding Rules 
;; General 
;; Write a single class declaration/implementation per file with the same name. 
;; Compile code using the C99 C language dialect. 
;; Compile with a maximum level of warnings activated (TBD). 
;; Do not declare static variable at function/method scope, use global static variable instead. 
;; Do not use goto, however early return is allowed in some cases (TBD). 
;; Use Objective-C 2.0 properties when appropriate. 
;; Do not use Objective-C Garbage Collector. 
;; Use the Dot Syntax to access properties only. 
;; Use Fast Enumeration whenever possible. 
;; Do not use protocols in object type specification , use an abstract base class 
;; (id<protocol> *) 
;; instead. 
;; Declare private methods if needed in a special ) category 
;; (Private 
;; Formatting 
;; Use 2 spaces for tabulation. 
;; Put opening brace on new line. 
;; Always use braces even for a single line of code. 
;; Leave a single space around binary operators. 
;; Do not add space around unary operator. 
;; Do not add space around parentheses. 
;; for(int i = 0; i < 3; i++) 
;; { 
;;   if(!(x % 2)) 
;;   { 
;;     x += 1; 
;;   } 
;; } 
;; Leave a single space before * in pointer type specification. 
;; - (void)setContent:(NSArray *)items 
;; { 
;;   NSArray *newContent = (NSArray *) [items copy]; 
;;   (...) 
;; } 
;; Place method/function declaration on a single line of code. 
;; + (id)loadFromFile:(NSString *)path format:(DXFImageFormat)format 
;; options:(NSUInteger)options error:(NSError **)error; 
;; Write method/function call on a single line of code.
;; 10.  
;; 1.  
;; 2.  
;; 3.  
;; 4.  
;; 5.  
;; 6.  
;; 7.  
;; + (id)loadFromFile:(NSString *)path format:(DXFImageFormat)format 
;; options:(NSUInteger)options error:(NSError **)error 
;; { 
;;   return [DXFMetaDataDictionary loadFromFile:path format:format 
;; options:options error:error]; 
;; } 
;; Double parentheses around assignment expression in conditionnal expression is only allowed for 
;; initializer implementation. 
;; if((self = [super init])) 
;; { 
;;   (...) 
;; } 
;; Naming 
;; Use Pascal case with uppercase trigram for class names. 
;; @interface DOPCustomClass <DOPCustomProtocol> 
;; @end 
;; Use Pascal case with uppercase trigram for protocol names. 
;; @protocol DOPCustomProtocol; 
;; Use Pascal case with uppercase trigram for category names. 
;; @interface NSString (DOPCustomAdditions) 
;; @end 
;; Use standard Caml case for public method names. 
;; - (void)setObject:(id)object forKey:(NSString *)key; 
;; Use Caml case with underscore for private method names. 
;; - (id)_objectWithIdentifier:(NSString *)identifier; 
;; Use Pascal case with uppercase trigram for public function names. 
;; extern void DXFLog(NSString *format, ...); 
;; Use regular Pascal case for private function names
;; static void DictionaryApply(id object, NSString *key, const void *context); 
;; Use regular Caml case for local variable names. 
;; NSArray *localArray = nil; 
;; Use regular Caml case for method/function parameter names. 
;; - (id)initWithString:(NSString *)paramString; 
;; Use Caml case with underscore for member variable names. 
;; NSArray *_memberArray; 
;; Use Caml case with double underscore for global variable names. 
;; static NSDictionary *__staticGlobalDictonary = nil; 
;; Comments 
;; Write a header at the top of each file using C++ style comments. 
;; // 
;; //  <File> 
;; //  <Project> 
;; // 
;; //  Created by <Developer> on <Date>. 
;; //  Copyright 2011 DxO Labs. All rights reserved. 
;; // 
;; Write a brief description before each interface declaration using C style comments. 
;; /* 
;;  * DOPSampleClass 
;;  * Sample class to illustrate the Objective-C coding rules 
;;  */ 
;; Write a brief description before each method/function declaration using C++ style comments. 
;; // Close database connection and reset query count 
;; - (void)close; 
;; A full example 
;; Header file.
;; DOPSampleClass.h 
;; // 
;; //  DOPSampleClass.h 
;; //  DXOOpticsPro 
;; // 
;; //  Created by Etienne Guérard on 21/10/11. 
;; //  Copyright 2011 DxO Labs. All rights reserved. 
;; // 
;; #import <Cocoa/Cocoa.h> 
;; /* 
;;  * DOPSampleClass 
;;  * Sample class to illustrate the Objective-C coding rules 
;;  */ 
;; @class DOPSampleClassHelper; 
;; @interface DOPSampleClass : NSObject 
;; { 
;;   DOPSampleClassHelper *_helper; 
;;   NSString *_name; 
;; } 
;; @property (nonatomic, copy) NSString *name; 
;; // Initialize this sample with the given name 
;; - (id)initWithName:(NSString *)name; 
;; // Close database connection and reset query count 
;; - (void)close; 
;; @end 
;; Implementation file.
;; DOPSampleClass.m 
;; // 
;; //  DOPSampleClass.m 
;; //  DXOOpticsPro 
;; // 
;; //  Created by Etienne Guérard on 21/10/11. 
;; //  Copyright 2011 DxO Labs. All rights reserved. 
;; // 
;; #import "DOPSampleClass.h" // <- Local include first 
;; #import "DOPSampleClassHelper.h" 
;; #import "OtherLocalHeader.h" 
;; #import <DXFFramework/DXFFramework.h> // Frameworks 
;; #import <SytemFramework/SystemFramework.h> 
;; #include <stdarg.h> // System includes last 
;; @implementation DOPSampleClass 
;; @synthesize name = _name; // Properties first 
;; - (id)initWithName:(NSString *)name 
;; { 
;;   NSParameterAssert(name != nil); 
;;   if((self = [super init])) 
;;   { 
;;     _helper = [[DOPSampleClassHelper alloc] initFor:self withIdentifier:@"Truc" 
;; name:name value:3.5f]; 
;;     if(!_helper) 
;;     { 
;;       [self dealloc]; 
;;       return nil; 
;;     } 
;;     // Initialization follows 
;;     self.name = [NSString stringWithFormat:@"Truc(%@:%f)", name, _helper.value]; 
;;   } 
;;   return self; 
;; } 
;; - (void)dealloc 
;; { 
;;   self.name = nil; 
;;   [_helper release]; 
;;   [super dealloc]; 
;; } 
;; - (void)close 
;; { 
;;   [_helper closeDatabase]; 
;;   [_helper resetQueryCount]; 
;; } 
;; @end
