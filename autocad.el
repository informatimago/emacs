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
;; Usage:
;;   (require 'autocad)
;;   ;; optional:
;;   (setq autocad-lisp-warn-undeclared-variables t)
;;
;; Author:
;;   ChatGPT (OpenAI GPT-5.3) <chatgpt@openai.com>
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

(defconst autocad-lisp--core-builtin-symbols
  '(
    ;; control / function definition
    "defun" "lambda" "quote" "setq" "set" "setvar" "getvar" "command" "command-s"
    "progn" "if" "cond" "and" "or" "not" "while" "repeat" "foreach" "eval"
    "apply" "mapcar" "function"

    ;; types & predicates
    "type" "atom" "listp" "numberp" "stringp" "vl-consp" "vl-listp" "vl-stringp"
    "vl-symbolp" "boundp" "null" "eq" "equal" "=" "<" "<=" ">" ">="

    ;; arithmetic
    "+" "-" "*" "/" "1+" "1-" "abs" "fix" "float" "max" "min" "sqrt" "sin" "cos"
    "atan" "log" "exp" "rem"

    ;; list
    "car" "cdr" "caar" "cadr" "cdar" "cddr" "caadr" "caddr" "cadadr"
    "cons" "append" "reverse" "length" "member" "assoc" "subst" "vl-remove"
    "vl-remove-if" "vl-remove-if-not" "vl-sort" "vl-position"

    ;; strings
    "strcat" "strlen" "substr" "vl-string-search" "vl-string-translate"
    "vl-string-left-trim" "vl-string-right-trim" "vl-princ-to-string"

    ;; io / printing
    "princ" "prin1" "print" "prompt" "terpri" "getstring" "getint" "getreal"
    "getkword" "getpoint" "getdist" "getangle" "getfiled" "getenv" "setenv"

    ;; files
    "open" "close" "read-line" "write-line" "read-char" "vl-file-directory-p"
    "vl-file-copy" "vl-file-delete" "findfile" "vl-filename-directory"
    "vl-filename-base" "vl-filename-extension" "vl-mkdir"

    ;; selection / entities / DXF
    "entget" "entmod" "entupd" "entdel" "entnext" "entlast" "entmake"
    "ssget" "ssadd" "ssdel" "sslength" "ssname"
    "tblsearch" "tblnext" "tbllist"

    ;; errors
    "error" "*error*" "vl-catch-all-apply" "vl-catch-all-error-message"
    "vl-catch-all-error-p"

    ;; COM / Visual LISP common
    "vl-load-com" "vlax-get" "vlax-put" "vlax-invoke" "vlax-method-applicable-p"
    "vlax-property-available-p" "vlax-ename->vla-object" "vlax-vla-object->ename"
    "vla-get-ActiveDocument" "vla-get-Application"
    "vlax-for" "vlax-release-object"

    ;; common CAD helpers
    "trans" "polar" "angle" "distance" "rtos" "atof" "itoa" "atoi"
    "osnap" "grread" "initget"
    )
  "A broad list of common AutoLISP / Visual LISP builtins for highlighting/completion.")

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

;; --- Utility: symbol / sexp scanning ---------------------------------------

(defun autocad--in-string-or-comment-p (&optional pos)
  "Non-nil if POS (or point) is in string or comment."
  (let* ((ppss (syntax-ppss (or pos (point)))))
    (or (nth 3 ppss) (nth 4 ppss))))

(defun autocad--symbol-at (pos)
  "Return symbol name at POS, or nil."
  (save-excursion
    (goto-char pos)
    (let ((sym (symbol-at-point)))
      (when sym (symbol-name sym)))))

(defun autocad--symbol-before-point-is-function-position-p ()
  "Non-nil if the symbol at point is in function position, i.e., immediately after '(' with only whitespace."
  (save-excursion
    (let ((end (point)))
      (skip-chars-backward " \t\n")
      (when (eq (char-before) ?\()
        ;; ( <symbol>
        t))))

(defun autocad--current-defun-boundaries ()
  "Return (START . END) of the current defun, or nil."
  (save-excursion
    (when (ignore-errors (beginning-of-defun) t)
      (let ((start (point)))
        (when (ignore-errors (end-of-defun) t)
          (cons start (point)))))))

(defun autocad--parse-defun-decls (defun-start)
  "Parse defun at DEFUN-START and return a hash-set of declared vars.
Declared = parameters + locals after \"/\" in the arglist."
  (let ((decl (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char defun-start)
      (when (looking-at-p (rx "(" (0+ space) "defun" symbol-end))
        ;; Move to arglist: (defun NAME ( ... ) ...)
        (condition-case _
            (progn
              (forward-char 1)          ; (
              (forward-sexp 1)          ; defun
              (forward-sexp 1)          ; name
              (skip-chars-forward " \t\n")
              (when (looking-at-p "(")
                (let ((arglist-start (point)))
                  (forward-sexp 1)
                  (let* ((arglist (buffer-substring-no-properties
                                   (1+ arglist-start) (1- (point))))
                         (tokens (split-string arglist "[ \t\n]+" t))
                         (seen-slash nil))
                    (dolist (tok tokens)
                      (cond
                       ((string= tok "/") (setq seen-slash t))
                       ;; ignore &optional style markers if any
                       ((string-prefix-p "&" tok) nil)
                       ;; Strip quoting and oddities
                       (t
                        (setq tok (replace-regexp-in-string (rx (any "'" "`" ",")) "" tok))
                        (when (and (not (string-empty-p tok))
                                   ;; basic var token validation
                                   (string-match-p (rx bos (1+ (or word (any "-_:*?<>"))) eos) tok))
                          ;; both params and locals count as declared for our purpose
                          (puthash tok t decl)))))))))
          (error nil))))
    decl))

(defun autocad--known-symbol-p (sym)
  "Non-nil if SYM is a known builtin/constant/ignored."
  (or (member sym autocad-lisp--core-builtin-symbols)
      (member sym autocad-lisp--constants)
      (member sym autocad-lisp-undeclared-ignore-symbols)
      (member sym autocad-lisp-extra-known-symbols)
      ;; AutoCAD command function definitions often named c:FOO (case-insensitive)
      (string-match-p (rx bos (or "c:" "C:")) sym)
      ;; Common dynamic/special vars are *like-this*
      (string-match-p (rx bos "*" (+ (not (any space "\t" "\n" "(" ")"))) "*" eos) sym)))

;; Cache declared vars per defun start position (buffer-local).
(defvar-local autocad--decl-cache (make-hash-table :test 'eq))
(defvar-local autocad--decl-cache-tick 0)

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

;; --- AutoLISP font-lock / indentation / imenu / completion ------------------

(defconst autocad-lisp--font-lock-keywords
  (let* ((builtins (regexp-opt autocad-lisp--core-builtin-symbols 'symbols))
         (consts   (regexp-opt autocad-lisp--constants 'symbols))
         (vl-obj   (rx symbol-start (or "vla-" "vlax-" "vl-") (1+ (or word (any "-_"))) symbol-end)))
    `(
      ;; builtins + common vl* prefixes
      (,builtins . font-lock-builtin-face)
      (,vl-obj . font-lock-builtin-face)
      (,consts . font-lock-constant-face)

      ;; defun names: (defun NAME ...)
      (,(rx "(" (0+ space) "defun" (1+ space)
            (group (1+ (or word (any "-_:*?<>")))))
       1 font-lock-function-name-face)

      ;; Command defs: (defun c:foo ...)
      (,(rx "(" (0+ space) "defun" (1+ space)
            (group (or "c:" "C:") (1+ (or word (any "-_")))))
       1 font-lock-keyword-face)

      ;; Strings handled by syntax table; comments too.
      ))
  "Base font-lock keywords for AutoLISP.")

(defun autocad-lisp--indent-function (indent-point state)
  "Indent function for `autocad-lisp-mode`."
  (let ((normal-indent (lisp-indent-function indent-point state)))
    ;; Prefer Lisp's indentation, but add per-symbol overrides.
    (save-excursion
      (goto-char (elt state 1))
      (when (looking-at-p (rx "(" (0+ space) (group (1+ (or word (any "-_:*?<>"))))))
        (let* ((sym (intern-soft (match-string-no-properties 1)))
               (spec (and sym (cdr (assq sym autocad-lisp-indent-spec)))))
          (cond
           ((eq spec 'defun) (lisp-indent-defform state indent-point))
           ((integerp spec) (lisp-indent-specform spec state indent-point))
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
              (delete-dups (append autocad-lisp--core-builtin-symbols
                                   autocad-lisp--constants
                                   autocad-lisp-extra-known-symbols))
              :exclusive 'no)))))

(defvar autocad-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; A couple of useful keys; tweak to taste.
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
  (setq-local imenu-create-index-function #'autocad-lisp--imenu-create-index)
  (setq-local completion-at-point-functions
              (list #'autocad-lisp--completion-at-point))
  ;; Font lock
  (setq-local font-lock-defaults
              (list
               (append
                autocad-lisp--font-lock-keywords
                (when autocad-lisp-warn-undeclared-variables
                  `((autocad--undeclared-symbol-matcher
                     0 'autocad-warning-undeclared-variable prepend))))
               nil nil ((?_ . "w") (?* . "w") (?- . "w") (?: . "w") (?> . "w") (?< . "w") (?? . "w"))
               nil))
  ;; Make c:foo commands stand out in imenu, too
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

;; Some users also want these:
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vlx\\'" . autocad-lisp-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fas\\'" . autocad-lisp-mode))

(provide 'autocad)
;;; autocad.el ends here
