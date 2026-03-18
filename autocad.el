;;; autocad.el --- Major modes for AutoCAD files: AutoLISP/Visual LISP, DCL, DWG -*- lexical-binding: t; -*-
;;
;; Features:
;; - autocad-lisp-mode for .lsp/.vlx/.fas (editing sources mainly .lsp)
;; - Font-lock for core AutoLISP + common Visual LISP / ActiveX symbols
;; - Indentation rules for common special forms (if/cond/while/repeat/foreach/etc.)
;; - Imenu support for (defun ...) and (defun c:COMMAND ...) entries
;; - Optional “undeclared variable” warnings inside defun bodies:
;;     Warns on bare symbol usage that is NOT:
;;       * a parameter, or a local (after “/” in defun arglist)
;;       * a known constant or keyword
;;       * a function position symbol (immediately after '(')
;;   This is intentionally conservative and may have false positives/negatives.
;; - autocad-dcl-mode for .dcl with basic highlighting and comment syntax
;; - autocad-dwg-mode for .dwg (opens in read-only hexl-mode)
;;
;; Fixes vs initial draft:
;; - font-lock-defaults now correctly quotes its SYNTAX-ALIST.
;; - Adds support for AutoLISP block comments ;| ... |; via syntax-propertize.
;; - Avoids raw forward-sexp across unrecognized block comments by using
;;   comment-aware skipping in our defun-arglist parser.
;;
;; Usage:
;;   (require 'autocad)
;;   (setq autocad-lisp-warn-undeclared-variables t)
;;
;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'imenu)
(require 'lisp-mode)
(require 'thingatpt)

(defgroup autocad nil
  "Editing support for AutoCAD file formats."
  :group 'languages)

(defface autocad-warning-undeclared-variable
  '((t :inherit font-lock-warning-face))
  "Face used for undeclared variable warnings."
  :group 'autocad)

(defface autocad-lisp-special-operator-face
  '((t :inherit font-lock-keyword-face))
  "Face used for AutoLISP special operators."
  :group 'autocad)

(defface autocad-lisp-autolisp-function-face
  '((t :inherit font-lock-builtin-face))
  "Face used for AutoLISP builtin functions."
  :group 'autocad)

(defface autocad-lisp-visual-lisp-function-face
  '((t :inherit font-lock-type-face))
  "Face used for Visual LISP functions."
  :group 'autocad)

(defface autocad-lisp-user-function-face
  '((t :inherit font-lock-function-name-face))
  "Face used for user-defined AutoLISP functions."
  :group 'autocad)

(defface autocad-lisp-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for AutoLISP variables and locals."
  :group 'autocad)

(defcustom autocad-lisp-warn-undeclared-variables t
  "If non-nil, warn (via font-lock) about variables not declared in defun arglist."
  :type 'boolean
  :group 'autocad)

(defcustom autocad-lisp-undeclared-ignore-symbols
  '("nil" "t")
  "Symbols that should never be warned as undeclared."
  :type '(repeat string)
  :group 'autocad)

(defcustom autocad-lisp-extra-known-symbols nil
  "Extra symbols treated as known builtins (won't be warned)."
  :type '(repeat string)
  :group 'autocad)

(defcustom autocad-lisp-indent-spec
  ;; Indentation spec: like `lisp-indent-function` rules.
  ;; nil means default lisp indentation; integer N = like (defun ...) style
  ;; list means more complex. We'll map common AutoLISP special forms.
  '((defun . defun)
    (lambda . defun)
    (progn . 0)
    (if . 2)
    (cond . 0)
    (while . 1)
    (repeat . 1)
    (foreach . 2)
    (mapcar . 1)
    (vlax-for . 2)
    (vl-catch-all-apply . 1)
    (vl-catch-all-error-p . 1)
    (vl-load-com . 0))
  "Indentation rules for AutoLISP forms."
  :type '(alist :key-type symbol :value-type (choice (const defun) integer))
  :group 'autocad)

;; --- Known symbols (partial but broad; extend as needed) ---------------------

(defcustom autocad-lisp-special-operators
  '("defun" "lambda" "quote" "setq" "set" "progn" "if" "cond" "and" "or"
    "not" "while" "repeat" "foreach" "function")
  "Special operators and control forms highlighted as language forms."
  :type '(repeat string)
  :group 'autocad)

(defcustom autocad-lisp-autolisp-functions
  '(
    "setvar" "getvar" "command" "command-s" "eval" "apply" "mapcar"

    ;; types & predicates
    "type" "atom" "listp" "numberp" "stringp" "boundp" "null" "eq" "equal"
    "=" "<" "<=" ">" ">="

    ;; arithmetic
    "+" "-" "*" "/" "1+" "1-" "abs" "fix" "float" "max" "min" "sqrt" "sin" "cos"
    "atan" "log" "exp" "rem"

    ;; list
    "car" "cdr" "caar" "cadr" "cdar" "cddr" "caadr" "caddr" "cadadr"
    "cons" "append" "reverse" "length" "member" "assoc" "subst"

    ;; strings
    "strcat" "strlen" "substr"

    ;; io / printing
    "princ" "prin1" "print" "prompt" "terpri" "getstring" "getint" "getreal"
    "getkword" "getpoint" "getdist" "getangle" "getfiled" "getenv" "setenv"

    ;; files
    "open" "close" "read-line" "write-line" "read-char" "findfile"

    ;; selection / entities / DXF
    "entget" "entmod" "entupd" "entdel" "entnext" "entlast" "entmake"
    "ssget" "ssadd" "ssdel" "sslength" "ssname"
    "tblsearch" "tblnext" "tbllist"

    ;; errors
    "error" "*error*"

    ;; common CAD helpers
    "trans" "polar" "angle" "distance" "rtos" "atof" "itoa" "atoi"
    "osnap" "grread" "initget"
    )
  "Common AutoLISP builtin functions."
  :type '(repeat string)
  :group 'autocad)

(defcustom autocad-lisp-visual-lisp-functions
  '("vl-consp" "vl-listp" "vl-stringp" "vl-symbolp"
    "vl-remove" "vl-remove-if" "vl-remove-if-not" "vl-sort" "vl-position"
    "vl-string-search" "vl-string-translate"
    "vl-string-left-trim" "vl-string-right-trim" "vl-princ-to-string"
    "vl-file-directory-p" "vl-file-copy" "vl-file-delete"
    "vl-filename-directory" "vl-filename-base" "vl-filename-extension" "vl-mkdir"
    "vl-load-com" "vlax-get" "vlax-put" "vlax-invoke" "vlax-method-applicable-p"
    "vlax-property-available-p" "vlax-ename->vla-object" "vlax-vla-object->ename"
    "vla-get-ActiveDocument" "vla-get-Application"
    "vlax-for" "vlax-release-object"
    "vl-catch-all-apply" "vl-catch-all-error-message" "vl-catch-all-error-p")
  "Common Visual LISP builtin functions."
  :type '(repeat string)
  :group 'autocad)

(defun autocad-lisp-known-function-symbols ()
  "Return all known AutoLISP and Visual LISP function symbols."
  (delete-dups
   (append autocad-lisp-special-operators
           autocad-lisp-autolisp-functions
           autocad-lisp-visual-lisp-functions
           autocad-lisp-extra-known-symbols)))

(defconst autocad-lisp--constants
  '("T" "nil" "*error*" "PI")
  "Common symbols/constants to highlight.")

;; DCL keywords / atoms (not exhaustive)
(defconst autocad-dcl--keywords
  '("dialog" "label" "key" "value" "action" "is_cancel" "is_default"
    "boxed_column" "boxed_row" "column" "row" "text" "button" "edit_box"
    "list_box" "popup_list" "toggle" "radio_button" "spacer"
    "image" "slider" "errtile" "ok_cancel" "ok_only" "cancel_only"
    "children_alignment" "alignment" "fixed_width" "fixed_height"
    "width" "height" "aspect_ratio" "color" "tabs" "tab"
    "default_value" "mnemonic" "true" "false")
  "DCL keywords and common tiles.")

;; --- Utility: comments / sexp scanning -------------------------------------

(defun autocad--in-string-or-comment-p (&optional pos)
  "Non-nil if POS (or point) is in string or comment."
  (let* ((ppss (syntax-ppss (or pos (point)))))
    (or (nth 3 ppss) (nth 4 ppss))))

(defun autocad--skip-line-comment ()
  "If point is at a ';' line comment, skip it. Return non-nil if skipped."
  (when (and (eq (char-after) ?\;)
             ;; don't treat ;| as line comment
             (not (eq (char-after (1+ (point))) ?|)))
    (end-of-line)
    t))

(defun autocad--skip-block-comment ()
  "If point is at a ';|' block comment, skip it. Return non-nil if skipped.

AutoLISP block comments are delimited by ;| and |; (not nestable in practice)."
  (when (and (eq (char-after) ?\;)
             (eq (char-after (1+ (point))) ?|))
    (let ((start (point)))
      (if (search-forward "|;" nil t)
          t
        ;; Unterminated block comment: move to end to avoid loops.
        (goto-char (point-max))
        ;; still report we skipped something
        (> (point) start)))))

(defun autocad--skip-ws-and-comments ()
  "Skip whitespace and AutoLISP comments at point. Return non-nil if moved."
  (let ((moved nil)
        (again t))
    (while again
      (setq again nil)
      (let ((p (point)))
        (skip-chars-forward " \t\n\r")
        (setq moved (or moved (/= p (point)))))
      (cond
       ((autocad--skip-block-comment)
        (setq moved t again t))
       ((autocad--skip-line-comment)
        (setq moved t again t))))
    moved))

;; Syntax-propertize for ;| ... |; so Emacs parsers/indent can treat them as comments.
(defconst autocad-lisp--syntax-propertize-rules
  (syntax-propertize-rules
   ;; Mark ';' of ';|' as comment start (style b).
   (";|" (0 (string-to-syntax "< b")))
   ;; Mark '|' of '|;' as comment end (style b).
   ("|;" (0 (string-to-syntax "> b"))))
  "Syntax propertize rules for AutoLISP block comments.")

;; --- defun-local declarations / warnings -----------------------------------

(defun autocad--symbol-before-point-is-function-position-p ()
  "Return non-nil when the symbol before point is in function position."
  (save-excursion
    (skip-chars-backward "A-Za-z0-9_:-*?<>")
    (skip-chars-backward " \t\n")
    (eq (char-before) ?\()))

(defun autocad--current-defun-boundaries ()
  "Return (START . END) of the current defun, or nil."
  (save-excursion
    (when (ignore-errors (beginning-of-defun) t)
      (let ((start (point)))
        (when (ignore-errors (end-of-defun) t)
          (cons start (point)))))))

(defun autocad--parse-defun-decls (defun-start)
  "Parse defun at DEFUN-START and return a hash-set of declared vars.
Declared = parameters + locals after \"/\" in the arglist.

This parser is comment-aware for AutoLISP's ;| ... |; comments."
  (let ((decl (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char defun-start)
      (autocad--skip-ws-and-comments)
      (when (looking-at-p (rx "(" (0+ space) "defun" symbol-end))
        (condition-case _
            (progn
              (forward-char 1)              ; (
              (autocad--skip-ws-and-comments)
              (forward-sexp 1)              ; defun
              (autocad--skip-ws-and-comments)
              (forward-sexp 1)              ; name
              (autocad--skip-ws-and-comments)
              (when (looking-at-p "(")
                (let ((arglist-start (point)))
                  ;; arglist may contain comments; syntax-propertize handles block, and ; handles line
                  (forward-sexp 1)
                  (let* ((arglist (buffer-substring-no-properties
                                   (1+ arglist-start) (1- (point))))
                         ;; strip block comments from arglist text defensively
                         (arglist (replace-regexp-in-string (rx ";|" (*? anything) "|;") " " arglist))
                         (arglist (replace-regexp-in-string (rx ";" (* nonl)) " " arglist))
                         (tokens (split-string arglist "[ \t\n]+" t)))
                    (dolist (tok tokens)
                      (cond
                       ((string= tok "/") nil)
                       ((string-prefix-p "&" tok) nil)
                       (t
                        (setq tok (replace-regexp-in-string (rx (any "'" "`" ",")) "" tok))
                        (when (and (not (string-empty-p tok))
                                   (string-match-p (rx bos (1+ (or word (any "-_:*?<>"))) eos) tok))
                          (puthash tok t decl)))))))))
          (error nil))))
    decl))

(defun autocad--known-symbol-p (sym)
  "Non-nil if SYM is a known builtin/constant/ignored."
  (or (member sym (autocad-lisp-known-function-symbols))
      (member sym autocad-lisp--constants)
      (member sym autocad-lisp-undeclared-ignore-symbols)
      ;; AutoCAD command function definitions often named c:FOO (case-insensitive)
      (string-match-p (rx bos (or "c:" "C:")) sym)
      ;; Common dynamic/special vars are *like-this*
      (string-match-p (rx bos "*" (+ (not (any space "\t" "\n" "(" ")"))) "*" eos) sym)))

;; Cache declared vars per defun start position (buffer-local).
(defvar-local autocad--decl-cache (make-hash-table :test 'eq))
(defvar-local autocad--decl-cache-tick 0)
(defvar-local autocad--function-cache (make-hash-table :test 'equal))
(defvar-local autocad--function-cache-tick 0)
(defvar-local autocad--font-lock-face nil)

(defun autocad--declared-vars-for-current-defun ()
  "Return declared vars hash-table for current defun, caching by defun start."
  (let ((b (autocad--current-defun-boundaries)))
    (when b
      (let* ((start (car b))
             (tick (buffer-chars-modified-tick)))
        (unless (eq autocad--decl-cache-tick tick)
          ;; Invalidate whole cache on any buffer change; simple + safe.
          (setq autocad--decl-cache (make-hash-table :test 'eq)
                autocad--decl-cache-tick tick))
        (or (gethash start autocad--decl-cache)
            (let ((decl (autocad--parse-defun-decls start)))
              (puthash start decl autocad--decl-cache)
              decl))))))

(defun autocad--undeclared-symbol-matcher (limit)
  "Font-lock matcher: find undeclared variable symbols up to LIMIT.
Sets match 0 to the symbol."
  (let ((case-fold-search t)
        (sym-regex (rx symbol-start
                       ;; allow typical AutoLISP symbol chars
                       (1+ (or word (any "-_:*?<>")))
                       symbol-end)))
    (catch 'found
      (while (re-search-forward sym-regex limit t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0)))
          (unless (or (autocad--in-string-or-comment-p beg)
                      (autocad--symbol-before-point-is-function-position-p)
                      ;; avoid highlighting numbers accidentally
                      (string-match-p (rx bos (1+ digit) eos)
                                      (buffer-substring-no-properties beg end)))
            (let ((sym (buffer-substring-no-properties beg end)))
              (unless (autocad--known-symbol-p sym)
                (let ((decl (autocad--declared-vars-for-current-defun)))
                  (when (and decl (not (gethash sym decl)))
                    (set-match-data (list beg end))
                    (throw 'found t))))))))
      nil)))

(defun autocad--function-position-face (sym)
  "Return the face to use for function-position symbol SYM."
  (cond
   ((member sym autocad-lisp-special-operators)
    'autocad-lisp-special-operator-face)
   ((member sym autocad-lisp-autolisp-functions)
    'autocad-lisp-autolisp-function-face)
   ((or (member sym autocad-lisp-visual-lisp-functions)
        (string-match-p (rx bos (or "vl-" "vlax-" "vla-")) sym))
    'autocad-lisp-visual-lisp-function-face)
   ((autocad--user-defined-function-p sym)
    'autocad-lisp-user-function-face)
   (t nil)))

(defun autocad--collect-user-functions ()
  "Return a hash table of function names defined in the current buffer."
  (let ((defs (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (let ((re (rx "(" (0+ space) "defun" (1+ space)
                    (group (1+ (or word (any "-_:*?<>")))))))
        (while (re-search-forward re nil t)
          (puthash (match-string-no-properties 1) t defs))))
    defs))

(defun autocad--user-defined-functions ()
  "Return cached user-defined functions for the current buffer."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq autocad--function-cache-tick tick)
      (setq autocad--function-cache (autocad--collect-user-functions)
            autocad--function-cache-tick tick))
    autocad--function-cache))

(defun autocad--user-defined-function-p (sym)
  "Return non-nil if SYM is defined by a buffer-local `defun`."
  (gethash sym (autocad--user-defined-functions)))

(defun autocad--function-symbol-matcher (limit)
  "Find function-position symbols up to LIMIT and capture their face."
  (let ((case-fold-search t)
        (found nil)
        (sym-regex (rx symbol-start
                       (1+ (or word (any "-_:*?<>")))
                       symbol-end)))
    (while (and (not found) (re-search-forward sym-regex limit t))
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (sym (buffer-substring-no-properties beg end))
             (face (and (not (autocad--in-string-or-comment-p beg))
                        (autocad--symbol-before-point-is-function-position-p)
                        (autocad--function-position-face sym))))
        (when face
          (setq autocad--font-lock-face face)
          (set-match-data (list beg end))
          (setq found t))))
    found))

(defun autocad--declared-variable-matcher (limit)
  "Find declared variables up to LIMIT."
  (let ((case-fold-search t)
        (found nil)
        (sym-regex (rx symbol-start
                       (1+ (or word (any "-_:*?<>")))
                       symbol-end)))
    (while (and (not found) (re-search-forward sym-regex limit t))
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (sym (buffer-substring-no-properties beg end))
             (decl (and (not (autocad--in-string-or-comment-p beg))
                        (not (autocad--symbol-before-point-is-function-position-p))
                        (not (string-match-p (rx bos (1+ digit) eos) sym))
                        (autocad--declared-vars-for-current-defun))))
        (when (and decl
                   (gethash sym decl)
                   (not (member sym autocad-lisp--constants)))
          (set-match-data (list beg end))
          (setq found t))))
    found))

;; --- AutoLISP font-lock / indentation / imenu / completion ------------------

(defconst autocad-lisp--font-lock-keywords
  (let ((specials (regexp-opt autocad-lisp-special-operators 'symbols))
        (autolisp (regexp-opt autocad-lisp-autolisp-functions 'symbols))
        (visual (regexp-opt autocad-lisp-visual-lisp-functions 'symbols))
        (consts (regexp-opt autocad-lisp--constants 'symbols))
        (visual-prefix (rx symbol-start
                           (or "vl-" "vlax-" "vla-")
                           (1+ (or word (any "-_")))
                           symbol-end)))
    `(
      (,specials . 'autocad-lisp-special-operator-face)
      (,autolisp . 'autocad-lisp-autolisp-function-face)
      (,visual . 'autocad-lisp-visual-lisp-function-face)
      (,visual-prefix . 'autocad-lisp-visual-lisp-function-face)
      (,consts . 'font-lock-constant-face)

      ;; defun names: (defun NAME ...)
      (,(rx "(" (0+ space) "defun" (1+ space)
            (group (1+ (or word (any "-_:*?<>")))))
       1 'autocad-lisp-user-function-face)

      ;; Parameters and locals declared in the defun arglist.
      (,(rx "(" (0+ space) "defun" (1+ space)
            (1+ (or word (any "-_:*?<>"))) (1+ space)
            "(" (group (*? anything)) ")")
       (1 'autocad-lisp-variable-face keep))
      ))
  "Base font-lock keywords for AutoLISP.")

(defun autocad-lisp--indent-function (indent-point state)
  "Indent function for `autocad-lisp-mode`."
  (let ((normal-indent (lisp-indent-function indent-point state)))
    (save-excursion
      (goto-char (elt state 1))
      (when (looking-at-p (rx "(" (0+ space) (group (1+ (or word (any "-_:*?<>"))))))
        (let* ((sym (intern-soft (match-string-no-properties 1)))
               (spec (and sym (cdr (assq sym autocad-lisp-indent-spec)))))
          (cond
           ((eq spec 'defun) (lisp-indent-defform state indent-point))
           ((integerp spec) (lisp-indent-specform spec state indent-point normal-indent))
           (t normal-indent)))))))

(defun autocad-lisp--imenu-create-index ()
  "Imenu index for AutoLISP defuns."
  (let ((index '()))
    (save-excursion
      (goto-char (point-min))
      (let ((re (rx "(" (0+ space) "defun" (1+ space)
                    (group (1+ (or word (any "-_:*?<>")))))))
        (while (re-search-forward re nil t)
          (let ((name (match-string-no-properties 1))
                (pos (match-beginning 0)))
            (push (cons name pos) index)))))
    (nreverse index)))

(defun autocad-lisp--completion-at-point ()
  "Provide completion for common AutoLISP/Visual LISP symbols."
  (when (and (not (autocad--in-string-or-comment-p))
             (or (looking-back (rx symbol-start (0+ (or word (any "-_:*?<>")))) (line-beginning-position))
                 (looking-at (rx symbol-start))))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (car bounds))
           (end (cdr bounds)))
      (when bounds
        (list beg end
              (delete-dups (append (autocad-lisp-known-function-symbols)
                                   (cl-loop for key being the hash-keys
                                            of (autocad--user-defined-functions)
                                            collect key)
                                   autocad-lisp--constants
                                   autocad-lisp-undeclared-ignore-symbols))
              :exclusive 'no)))))

(defvar autocad-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'comment-region)
    (define-key map (kbd "C-c C-u") #'uncomment-region)
    map)
  "Keymap for `autocad-lisp-mode`.")

;;;###autoload
(define-derived-mode autocad-lisp-mode lisp-mode "AutoCAD-LISP"
  "Major mode for editing AutoLISP / Visual LISP source files."
  :group 'autocad
  (setq-local lisp-indent-function #'autocad-lisp--indent-function)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  ;; Make Emacs understand ;| ... |; as block comments.
  (setq-local syntax-propertize-function autocad-lisp--syntax-propertize-rules)
  ;; Ensure existing text is propertized.
  (syntax-propertize (point-max))
  (setq-local imenu-create-index-function #'autocad-lisp--imenu-create-index)
  (setq-local completion-at-point-functions
              (list #'autocad-lisp--completion-at-point))
  ;; Font lock
  (setq-local font-lock-defaults
              (list
               (append autocad-lisp--font-lock-keywords)
               nil nil
               ;; SYNTAX-ALIST (MUST be quoted)
               '((?_ . "w") (?* . "w") (?- . "w") (?: . "w") (?> . "w") (?< . "w") (?? . "w"))
               nil))
  (imenu-add-to-menubar "Defs"))

;; --- DCL mode ---------------------------------------------------------------

(defvar autocad-dcl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'comment-region)
    (define-key map (kbd "C-c C-u") #'uncomment-region)
    map)
  "Keymap for `autocad-dcl-mode`.")

(defvar autocad-dcl-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; DCL: // comments
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    ;; treat '_' and '-' as word constituents in identifiers
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    st)
  "Syntax table for `autocad-dcl-mode`.")

(defconst autocad-dcl--font-lock-keywords
  `(
    (,(regexp-opt autocad-dcl--keywords 'symbols) . font-lock-keyword-face)
    ;; tile names like :row, :column, etc.
    (,(rx ":" (group (1+ (or word (any "_-"))))) 0 font-lock-builtin-face)
    ;; attribute assignments: key = "value";
    (,(rx (group (1+ (or word (any "_-")))) (0+ space) "=")
     1 font-lock-variable-name-face))
  "Font-lock keywords for DCL.")

;;;###autoload
(define-derived-mode autocad-dcl-mode prog-mode "AutoCAD-DCL"
  "Major mode for editing AutoCAD DCL dialog definition files."
  :group 'autocad
  :syntax-table autocad-dcl-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults (list autocad-dcl--font-lock-keywords))
  (setq-local indent-line-function #'indent-relative)
  (setq-local electric-indent-chars '(?\n ?\; ?\} ?\)))
  (setq-local tab-width 2))

;; --- DWG mode (binary) ------------------------------------------------------

;;;###autoload
(define-derived-mode autocad-dwg-mode special-mode "AutoCAD-DWG"
  "Mode for viewing DWG files (binary). Opens in `hexl-mode`."
  :group 'autocad
  (setq buffer-read-only t)
  (when (fboundp 'hexl-mode)
    (hexl-mode 1)))

;; --- Auto mode associations -------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lsp\\'" . autocad-lisp-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dcl\\'" . autocad-dcl-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dwg\\'" . autocad-dwg-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vlx\\'" . autocad-lisp-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fas\\'" . autocad-lisp-mode))

(provide 'autocad)
;;; autocad.el ends here
