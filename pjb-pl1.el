;;; pjb-pl1.el --- major mode for editing PL/1 source in Emacs

;; Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
;;               2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;               Free Software Foundation, Inc.

;; Author: Pascal J. Bourguignon <pjb@informatimago.com> (transformed into pjb-pl1.el Tue Sep  7 17:24:03 CEST 2010)
;; Author: Espen Skoglund <esk@gnu.org> (as pascal.el)
;; Keywords: languages

;; T-h-i-s- -f-i-l-e- -i-s- -p-a-r-t- -o-f- -G-N-U- -E-m-a-c-s-.-

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; USAGE
;; =====

;; Emacs should enter PL/1 mode when you find a PL/1 source file.
;; When you have entered PL/1 mode, you may get more info by pressing
;; C-h m. You may also get online help describing various functions by:
;; C-h f <Name of function you want described>

;; If you want to customize PL/1 mode to fit you better, you may add
;; these lines (the values of the variables presented here are the defaults):
;;
;; ;; User customization for Pl1 mode
;; (setq pl1-indent-level       3
;;       pl1-case-indent        2
;;       pl1-auto-newline       nil
;;       pl1-tab-always-indent  t
;;       pl1-auto-endcomments   t
;;       pl1-auto-lineup        '(all)
;;       pl1-toggle-completions nil
;;       pl1-type-keywords      '("array" "file" "packed" "char"
;;                                   "integer" "real" "string" "record")
;;       pl1-start-keywords     '("begin" "end" "function" "procedure"
;;                                   "repeat" "until" "while" "read" "readln"
;;                                   "reset" "rewrite" "write" "writeln")
;;       pl1-separator-keywords '("downto" "else" "mod" "div" "then"))

;; KNOWN BUGS / BUGREPORTS
;; =======================
;; As far as I know, there are no bugs in the current version of this
;; package.  This may not be true however, since I never use this mode
;; myself and therefore would never notice them anyway.   If you do
;; find any bugs, you may submit them to: esk@gnu.org as well as to
;; bug-gnu-emacs@gnu.org.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup pl1 nil
  "Major mode for editing PL/1 source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defvar pl1-mode-abbrev-table nil
  "Abbrev table in use in Pl1-mode buffers.")
(define-abbrev-table 'pl1-mode-abbrev-table ())

(defvar pl1-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";"        'electric-pl1-semi-or-dot)
    (define-key map "."        'electric-pl1-semi-or-dot)
    (define-key map ":"        'electric-pl1-colon)
    (define-key map "="        'electric-pl1-equal)
    (define-key map "#"        'electric-pl1-hash)
    (define-key map "\r"       'electric-pl1-terminate-line)
    (define-key map "\t"       'electric-pl1-tab)
    (define-key map "\M-\t"    'pl1-complete-word)
    (define-key map "\M-?"     'pl1-show-completions)
    (define-key map "\177"     'backward-delete-char-untabify)
    (define-key map "\M-\C-h"  'pl1-mark-defun)
    (define-key map "\C-c\C-b" 'pl1-insert-block)
    (define-key map "\M-*"     'pl1-star-comment)
    (define-key map "\C-c\C-c" 'pl1-comment-area)
    (define-key map "\C-c\C-u" 'pl1-uncomment-area)
    (define-key map "\M-\C-a"  'pl1-beg-of-defun)
    (define-key map "\M-\C-e"  'pl1-end-of-defun)
    (define-key map "\C-c\C-d" 'pl1-goto-defun)
    (define-key map "\C-c\C-o" 'pl1-outline-mode)
    ;; A command to change the whole buffer won't be used terribly
    ;; often, so no need for a key binding.
    ;; (define-key map "\C-cd"    'pl1-downcase-keywords)
    ;; (define-key map "\C-cu"    'pl1-upcase-keywords)
    ;; (define-key map "\C-cc"    'pl1-capitalize-keywords)
    map)
  "Keymap used in PL/1 mode.")

(defvar pl1-imenu-generic-expression
  '((nil "^[ \t]*\\([a-zA-Z0-9_.:]+\\)[ \t\n\r\f\v]*:[ \t\n\r\f\v]*\\(proc\\|procedure\\|entry\\)[ \t\n\r\f\v]*;" 1))
  "Imenu expression for pl1-mode.  See `imenu-generic-expression'.")

(defvar pl1-keywords
  '("a" "abnormal" "activate" "addbuff" "alias" "aligned" "allocate"
    "anycondition" "area" "ascii" "assignable" "asgn" "asm" "assembler" "attach"
    "attention" "attn" "automatic" "b" "b1" "b2" "b3" "b4" "backwards" "based"
    "begin" "bigendian" "binary" "bit" "bkwd" "blksize" "buffered" "buffers"
    "buffoff" "bufnd" "bufni" "bufsp" "builtin" "by" "byaddr" "byvalue" "bx" "c"
    "call" "cdecl" "cell" "character" "chargraphic" "check" "close" "cobol"
    "column" "complex" "cplx" "connected" "condition" "consecutive" "constant"
    "ctlasa" "ctl360" "controlled" "ctl" "conversion" "copy" "d" "db" "data"
    "date" "dcl" "declare" "deactivate" "decimal" "dft" "default" "delay" "delete"
    "define" "defined" "descriptor" "descriptors" "detach" "dimension" "direct"
    "display" "do" "downthru" "e" "edit" "else" "endendfile" "endpage" "entry"
    "environment" "error" "event" "exclusive" "exit" "exports" "external" "f" "fb"
    "fs" "fbs" "fetch" "fetchable" "file" "finish" "fixed" "fixedoverflow" "fofl"
    "float" "flush" "free" "forever" "fortran" "format" "from" "fromalien" "g"
    "generic" "genkey" "get" "go" "goto" "graphic" "gx" "handle" "hexadec" "i"
    "ieee" "if" "ignore" "imported" "in" "include" "indexarea" "indexed" "initial"
    "inline" "input" "inter" "interactive" "internal" "into" "invalidop"
    "irreducible" "iterate" "key" "keyed" "keyfrom" "keylength" "keyloc" "keyto"
    "label" "leave" "limited" "like" "line" "linesize" "linkage" "list"
    "littleendian" "local" "locate" "loop" "m" "main" "name" "ncp" "nochargraphic"
    "nocheck" "noconversion" "nodescriptor" "noexecops" "nofixedoverflow" "nofofl"
    "nolock" "nonassignable" "nonasgn" "nonconnected" "none" "nonvarying"
    "non_quick" "no_quick_blocks" "noinit" "noinline" "noinvalidop" "nooverflow"
    "noofl" "noprint" "normal" "nosize" "nosubscriptrange" "nosubrg"
    "nostringrange" "nostrg" "nostringsize" "nostrz" "note" "nounderflow" "noufl"
    "nowrite" "nozerodivide" "nozdiv" "offset" "on" "open" "optional" "options"
    "optlink" "order" "ordinal" "otherwise" "output" "overflow" "ofl" "p"
    "package" "packed_decimal" "packed" "page" "pagesize" "parm" "parameter"
    "password" "pending" "picture" "pointer" "ptr" "position" "precision" "print"
    "priority" "procedure" "put" "r" "range" "read" "real" "record" "recsize"
    "recursive" "reducible" "reentrant" "refer" "regional" "release" "rename"
    "reorder" "repeat" "replace" "reply" "reread" "reserved" "reserves" "resignal"
    "retcode" "return" "returns" "reuse" "revert" "rewrite" "scalarvarying"
    "select" "separate_static" "set" "sequential" "seql" "signal" "signed" "sis"
    "size" "skip" "snap" "static" "stdcall" "storage" "stop" "stream" "string"
    "stringrange" "strg" "stringsize" "strz" "stringvalue" "structure" "sub"
    "subscriptrange" "subrg" "support" "system" "task" "then" "thread" "title"
    "to" "total" "tp" "transient" "transmit" "trkofl" "tstack" "type" "u"
    "unaligned" "unbuffered" "unconnected" "undefinedfile" "undf" "underflow"
    "ufl" "union" "unlock" "unsigned" "until" "update" "upthru" "v" "validate"
    "value" "variable" "varying" "varyingz" "vb" "vbs" "vs" "vsam" "wait" "when"
    "widechar" "winmain" "while" "write" "wx" "x" "xn" "xu" "zerodivide" "zdiv"))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
(defconst pl1-symbol-re           "\\<[a-zA-Z$%_][a-zA-Z$%_0-9.]*\\>")
(defconst pl1-beg-block-re        "\\<\\(begin\\|proc\\|procedure\\|do\\)\\>")
(defconst pl1-end-block-re        "\\<\\(end\\)\\>")
(defconst pl1-declaration-re      "\\<\\(dcl\\|declare\\)\\>")
(defconst pl1-progbeg-re          "\\<\\proc\\|procedure\\|dcl\\|declare\\>")
(defconst pl1-defun-re            "\\<\\(proc\\|procedure\\)\\>")
(defconst pl1-sub-block-re        "\\<\\(if\\|else\\|do\\)\\>")
(defconst pl1-noindent-re         "\\<\\(else\\)\\>")
(defconst pl1-nosemi-re           "\\<\\(then\\|while\\)\\>")
(defconst pl1-autoindent-lines-re "\\<\\(end\\|begin\\|while\\|else\\)\\>")

;;; Strings used to mark beginning and end of excluded text
(defconst pl1-exclude-str-start "/*-----\\/----- EXCLUDED -----\\/-----")
(defconst pl1-exclude-str-end    " -----/\\----- EXCLUDED -----/\\-----*/")

(defparameter pl1-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "."   st)
    (modify-syntax-entry ?/ "<14" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?+ "."    st)
    (modify-syntax-entry ?- "."    st)
    (modify-syntax-entry ?= "."    st)
    (modify-syntax-entry ?< "."    st)
    (modify-syntax-entry ?> "."    st)
    (modify-syntax-entry ?& "."    st)
    (modify-syntax-entry ?| "."    st)
    (modify-syntax-entry ?_ "_"    st)
    (modify-syntax-entry ?$ "_"    st)
    (modify-syntax-entry ?% "_"    st)
    (modify-syntax-entry ?\" "\""  st)
    st)
  "Syntax table in use in pl1-mode buffers.")



(defconst pl1-font-lock-keywords
  (purecopy
   (list
    '("^[ \t]*\\([a-zA-Z0-9_.:]+\\)[ \t\n\r\f\v]*:[ \t\n\r\f\v]*\\(proc\\|procedure\\|entry\\)[ \t\n\r\f\v]*;"
      (1 font-lock-function-name-face t)
      (2 font-lock-keyword-face))
    (cons (concat "\\<\\(char\\|character\\|dec\\|decimal\\|real\\|complex\\|cmplx\\|file\\|"
                  "bit\\|bin\\|binary\\|picture\\|precision\\|prec\\|fixed\\|float\\|"
                  "pointer\\|ptr\\|unaligned\\|aligned\\|builtin\\|structure\\|external\\|"
                  "varying\\|varyingz\\|variable\\|constant\\|based\\|static\\|init\\)\\>")
          'font-lock-type-face)
    '("\\<\\(entry\\)\\>[ \t\n\r\f\v]*[(,]"
      1 font-lock-type-face)
    '("\\<\\(returns\\)\\>[ \t\n\r\f\v]*("
      1 font-lock-type-face)
    ;; '("\\<\\(label\\|external\\|forward\\)\\>" . font-lock-constant-face)
    '("\\<\\([A-Za-z_%$0-9]+\\)[ \t]*:" 1 font-lock-function-name-face)
    (cons (concat "\\<\\("
                  (join (set-difference pl1-keywords
                                        '("proc" "procedure" "entry"
                                          "char" "character" "dec" "decimal" "bin" "binary"
                                          "real" "complex" "cmplx" "float" "file"
                                          "variable" "constant" "varying" "varyingz")) "\\|")
                  "\\)\\>")
          'font-lock-keyword-face)
    '("\\<\\(go[ \t\n\r\f\v]*to\\|call\\)\\>[ \t\n\r\f\v]*\\([A-Za-z_%$0-9]+\\)[ \t\n\r\f\v]*;"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face t))))
  "Additional expressions to highlight in Pl1 mode.")
(put 'pl1-mode 'font-lock-defaults '(pl1-font-lock-keywords nil t))

(defcustom pl1-indent-level 3
  "*Indentation of Pl1 statements with respect to containing block."
  :type 'integer
  :group 'pl1)

(defcustom pl1-case-indent 2
  "*Indentation for case statements."
  :type 'integer
  :group 'pl1)

(defcustom pl1-auto-newline nil
  "*Non-nil means automatically insert newlines in certain cases.
These include after semicolons and after the punctuation mark after an `end'."
  :type 'boolean
  :group 'pl1)

(defcustom pl1-indent-nested-functions t
  "*Non-nil means nested functions are indented."
  :type 'boolean
  :group 'pl1)

(defcustom pl1-tab-always-indent t
  "*Non-nil means TAB in Pl1 mode should always reindent the current line.
If this is nil, TAB inserts a tab if it is at the end of the line
and follows non-whitespace text."
  :type 'boolean
  :group 'pl1)

(defcustom pl1-auto-endcomments t
  "*Non-nil means automatically insert comments after certain `end's.
Specifically, this is done after the ends of cases statements and functions.
The name of the function or case is included between the braces."
  :type 'boolean
  :group 'pl1)

(defcustom pl1-auto-lineup '(all)
  "*List of contexts where auto lineup of :'s or ='s should be done.
Elements can be of type: 'paramlist', 'declaration' or 'case', which will
do auto lineup in parameterlist, declarations or case-statements
respectively. The word 'all' will do all lineups. '(case paramlist) for
instance will do lineup in case-statements and parameterlist, while '(all)
will do all lineups."
  :type '(set :extra-offset 8
              (const :tag "Everything" all)
              (const :tag "Parameter lists" paramlist)
              (const :tag "Decalrations" declaration)
              (const :tag "Case statements" case))
  :group 'pl1)

(defcustom pl1-toggle-completions nil
  "*Non-nil means \\<pl1-mode-map>\\[pl1-complete-word] should try all possible completions one by one.
Repeated use of \\[pl1-complete-word] will show you all of them.
Normally, when there is more than one possible completion,
it displays a list of all possible completions."
  :type 'boolean
  :group 'pl1)

(defcustom pl1-type-keywords
  '("array" "file" "packed" "char" "integer" "real" "string" "record")
  "*Keywords for types used when completing a word in a declaration or parmlist.
These include integer, real, char, etc.
The types defined within the Pl1 program
are handled in another way, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'pl1)

(defcustom pl1-start-keywords
  '("begin" "end" "function" "procedure" "repeat" "until" "while"
    "read" "readln" "reset" "rewrite" "write" "writeln")
  "*Keywords to complete when standing at the first word of a statement.
These are keywords such as begin, repeat, until, readln.
The procedures and variables defined within the Pl1 program
are handled in another way, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'pl1)

(defcustom pl1-separator-keywords
  '("downto" "else" "mod" "div" "then")
  "*Keywords to complete when NOT standing at the first word of a statement.
These are keywords such as downto, else, mod, then.
Variables and function names defined within the Pl1 program
are handled in another way, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'pl1)


;;;
;;;  Macros
;;;

(defsubst pl1-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst pl1-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defun pl1-declaration-end ()
  (let ((nest 1))
    (while (and (> nest 0)
                (re-search-forward
                 "[:=]\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)"
                 (save-excursion (end-of-line 2) (point)) t))
      (cond ((match-beginning 1) (setq nest (1+ nest)))
            ((match-beginning 2) (setq nest (1- nest)))
            ((looking-at "[^(\n]+)") (setq nest 0))))))


(defun pl1-declaration-beg ()
  (let ((nest 1))
    (while (and (> nest 0)
                (re-search-backward "[:=]\\|\\<\\(type\\|var\\|label\\|const\\)\\>\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" (pl1-get-beg-of-line 0) t))
      (cond ((match-beginning 1) (setq nest 0))
            ((match-beginning 2) (setq nest (1- nest)))
            ((match-beginning 3) (setq nest (1+ nest)))))
    (= nest 0)))


(defsubst pl1-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (pl1-get-beg-of-line) (point)))))


;;;###autoload
(defun pl1-mode ()
  "Major mode for editing Pl1 code. \\<pl1-mode-map>
TAB indents for Pl1 code.  Delete converts tabs to spaces as it moves back.

\\[pl1-complete-word] completes the word around current point with respect \
to position in code
\\[pl1-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[pl1-mark-defun]\t- Mark function.
\\[pl1-insert-block]\t- insert begin ... end;
\\[pl1-star-comment]\t- insert (* ... *)
\\[pl1-comment-area]\t- Put marked area in a comment, fixing nested comments.
\\[pl1-uncomment-area]\t- Uncomment an area commented with \
\\[pl1-comment-area].
\\[pl1-beg-of-defun]\t- Move to beginning of current function.
\\[pl1-end-of-defun]\t- Move to end of current function.
\\[pl1-goto-defun]\t- Goto function prompted for in the minibuffer.
\\[pl1-outline-mode]\t- Enter `pl1-outline-mode'.

Variables controlling indentation/edit style:

 pl1-indent-level (default 3)
    Indentation of Pl1 statements with respect to containing block.
 pl1-case-indent (default 2)
    Indentation for case statements.
 pl1-auto-newline (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 pl1-indent-nested-functions (default t)
    Non-nil means nested functions are indented.
 pl1-tab-always-indent (default t)
    Non-nil means TAB in Pl1 mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 pl1-auto-endcomments (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 pl1-auto-lineup (default t)
    List of contexts where auto lineup of :'s or ='s should be done.

See also the user variables pl1-type-keywords, pl1-start-keywords and
pl1-separator-keywords.

Turning on Pl1 mode calls the value of the variable pl1-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map pl1-mode-map)
  (setq major-mode 'pl1-mode)
  (setq mode-name "Pl1")
  (setq local-abbrev-table pl1-mode-abbrev-table)
  (set-syntax-table pl1-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'pl1-indent-line)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'pl1-indent-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'blink-matching-paren-dont-ignore-comments)
  (setq blink-matching-paren-dont-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *\\|{ *")
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pl1-font-lock-keywords nil t))
  ;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression pl1-imenu-generic-expression)
  (setq imenu-case-fold-search t)
  (run-mode-hooks 'pl1-mode-hook))



;;;
;;;  Electric functions
;;;
(defun electric-pl1-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at pl1-autoindent-lines-re)
        (pl1-indent-line)))
  (delete-horizontal-space) ; Removes trailing whitespaces
  (newline)
  ;; Indent next line
  (pl1-indent-line)
  ;; Maybe we should set some endcomments
  (if pl1-auto-endcomments
      (pl1-set-auto-comments))
  ;; Check if we shall indent inside comment
  (let ((setstar nil))
    (save-excursion
      (forward-line -1)
      (skip-chars-forward " \t")
      (cond ((looking-at "\\*[ \t]+)")
             ;; Delete region between `*' and `)' if there is only whitespaces.
             (forward-char 1)
             (delete-horizontal-space))
            ((and (looking-at "(\\*\\|\\*[^)]")
                  (not (save-excursion
                         (search-forward "*)" (pl1-get-end-of-line) t))))
             (setq setstar t))))
    ;; If last line was a star comment line then this one shall be too.
    (if (null setstar)
        (pl1-indent-line)
      (insert "*  "))))


(defun electric-pl1-semi-or-dot ()
  "Insert `;' or `.' character and reindent the line."
  (interactive)
  (insert last-command-event)
  (save-excursion
    (beginning-of-line)
    (pl1-indent-line))
  (if pl1-auto-newline
      (electric-pl1-terminate-line)))

(defun electric-pl1-colon ()
  "Insert `:' and do all indentions except line indent on this line."
  (interactive)
  (insert last-command-event)
  ;; Do nothing if within string.
  (if (pl1-within-string)
      ()
    (save-excursion
      (beginning-of-line)
      (pl1-indent-line))
    (let ((pl1-tab-always-indent nil))
      (pl1-indent-command))))

(defun electric-pl1-equal ()
  "Insert `=', and do indention if within type declaration."
  (interactive)
  (insert last-command-event)
  (if (eq (car (pl1-calculate-indent)) 'declaration)
      (let ((pl1-tab-always-indent nil))
        (pl1-indent-command))))

(defun electric-pl1-hash ()
  "Insert `#', and indent to column 0 if this is a CPP directive."
  (interactive)
  (insert last-command-event)
  (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
      (save-excursion (beginning-of-line)
                      (delete-horizontal-space))))

(defun electric-pl1-tab ()
  "Function called when TAB is pressed in Pl1 mode."
  (interactive)
  ;; Do nothing if within a string or in a CPP directive.
  (if (or (pl1-within-string)
          (and (not (bolp))
               (save-excursion (beginning-of-line) (eq (following-char) ?#))))
      (insert "\t")
    ;; If pl1-tab-always-indent, indent the beginning of the line.
    (if pl1-tab-always-indent
        (save-excursion
          (beginning-of-line)
          (pl1-indent-line))
      (if (save-excursion
            (skip-chars-backward " \t")
            (bolp))
          (pl1-indent-line)
        (insert "\t")))
    (pl1-indent-command)))



;;;
;;; Interactive functions
;;;
(defun pl1-insert-block ()
  "Insert Pl1 begin ... end; block in the code with right indentation."
  (interactive)
  (insert "begin")
  (electric-pl1-terminate-line)
  (save-excursion
    (newline)
    (insert "end;")
    (beginning-of-line)
    (pl1-indent-line)))

(defun pl1-star-comment ()
  "Insert Pl1 star comment at point."
  (interactive)
  (pl1-indent-line)
  (insert "(*")
  (electric-pl1-terminate-line)
  (save-excursion
    (electric-pl1-terminate-line)
    (delete-horizontal-space)
    (insert ")"))
  (insert "  "))

(defun pl1-mark-defun ()
  "Mark the current pl1 function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (pl1-end-of-defun)
  (push-mark (point))
  (pl1-beg-of-defun)
  (when (featurep 'xemacs)
    (zmacs-activate-region)))

(defun pl1-comment-area (start end)
  "Put the region into a Pl1 comment.
The comments that are in this area are \"deformed\":
`*)' becomes `!(*' and `}' becomes `!{'.
These deformed comments are returned to normal if you use
\\[pl1-uncomment-area] to undo the commenting.

The commented area starts with `pl1-exclude-str-start', and ends with
`pl1-include-str-end'.  But if you change these variables,
\\[pl1-uncomment-area] won't recognize the comments."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
             (not (save-excursion (skip-chars-backward " \t") (bolp))))
        (forward-line 1)
      (beginning-of-line))
    (insert pl1-exclude-str-end)
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert pl1-exclude-str-start)
    (newline)
    ;; Replace end-comments within commented area
    (goto-char end)
    (save-excursion
      (while (re-search-backward "\\*)" start t)
        (replace-match "!(*" t t)))
    (save-excursion
      (while (re-search-backward "}" start t)
        (replace-match "!{" t t)))))

(defun pl1-uncomment-area ()
  "Uncomment a commented area; change deformed comments back to normal.
This command does nothing if the pointer is not in a commented
area.  See also `pl1-comment-area'."
  (interactive)
  (save-excursion
    (let ((start (point))
          (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
        (setq start (progn (search-backward pl1-exclude-str-start nil t)
                           (point)))
        (setq end (progn (search-forward pl1-exclude-str-end nil t)
                         (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
          (message "Not standing within commented area.")
        (progn
          ;; Remove endcomment
          (goto-char end)
          (beginning-of-line)
          (let ((pos (point)))
            (end-of-line)
            (delete-region pos (1+ (point))))
          ;; Change comments back to normal
          (save-excursion
            (while (re-search-backward "!{" start t)
              (replace-match "}" t t)))
          (save-excursion
            (while (re-search-backward "!(\\*" start t)
              (replace-match "*)" t t)))
          ;; Remove startcomment
          (goto-char start)
          (beginning-of-line)
          (let ((pos (point)))
            (end-of-line)
            (delete-region pos (1+ (point)))))))))

(defun pl1-beg-of-defun ()
  "Move backward to the beginning of the current function or procedure."
  (interactive)
  (catch 'found
    (if (not (looking-at (concat "\\s \\|\\s)\\|" pl1-defun-re)))
        (forward-sexp 1))
    (let ((nest 0) (max -1) (func 0)
          (reg (concat pl1-beg-block-re "\\|"
                       pl1-end-block-re "\\|"
                       pl1-defun-re)))
      (while (re-search-backward reg nil 'move)
        (cond ((let ((state (save-excursion
                              (parse-partial-sexp (point-min) (point)))))
                 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
               ())
              ((match-end 1)                       ; begin|case|record|repeat
               (if (and (looking-at "\\<record\\>") (>= max 0))
                   (setq func (1- func)))
               (setq nest (1+ nest)
                     max (max nest max)))
              ((match-end 2)                       ; end|until
               (if (and (= nest max) (>= max 0))
                   (setq func (1+ func)))
               (setq nest (1- nest)))
              ((match-end 3)                       ; function|procedure
               (if (= 0 func)
                   (throw 'found t)
                 (setq func (1- func)))))))
    nil))

(defun pl1-end-of-defun ()
  "Move forward to the end of the current function or procedure."
  (interactive)
  (if (looking-at "\\s ")
      (forward-sexp 1))
  (if (not (looking-at pl1-defun-re))
      (pl1-beg-of-defun))
  (forward-char 1)
  (let ((nest 0) (func 1)
        (reg (concat pl1-beg-block-re "\\|"
                     pl1-end-block-re "\\|"
                     pl1-defun-re)))
    (while (and (/= func 0)
                (re-search-forward reg nil 'move))
      (cond ((let ((state (save-excursion
                              (parse-partial-sexp (point-min) (point)))))
                 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
               ())
            ((match-end 1)
             (setq nest (1+ nest))
             (if (save-excursion
                   (goto-char (match-beginning 0))
                   (looking-at "\\<record\\>"))
                 (setq func (1+ func))))
            ((match-end 2)
             (setq nest (1- nest))
             (if (= nest 0)
                 (setq func (1- func))))
            ((match-end 3)
             (setq func (1+ func))))))
  (forward-line 1))

(defun pl1-end-of-statement ()
  "Move forward to end of current statement."
  (interactive)
  (let ((parse-sexp-ignore-comments t)
        (nest 0) pos
        (regexp (concat "\\(" pl1-beg-block-re "\\)\\|\\("
                        pl1-end-block-re "\\)")))
    (if (not (looking-at "[ \t\n\r\f\v]")) (forward-sexp -1))
    (or (looking-at pl1-beg-block-re)
        ;; Skip to end of statement
        (setq pos (catch 'found
                    (while t
                      (forward-sexp 1)
                      (cond ((looking-at "[ \t]*;")
                             (skip-chars-forward "^;")
                             (forward-char 1)
                             (throw 'found (point)))
                            ((save-excursion
                               (forward-sexp -1)
                               (looking-at pl1-beg-block-re))
                             (goto-char (match-beginning 0))
                             (throw 'found nil))
                            ((eobp)
                             (throw 'found (point))))))))
    (if (not pos)
        ;; Skip a whole block
        (catch 'found
          (while t
            (re-search-forward regexp nil 'move)
            (setq nest (if (match-end 1)
                           (1+ nest)
                         (1- nest)))
            (cond ((eobp)
                   (throw 'found (point)))
                  ((= 0 nest)
                   (throw 'found (pl1-end-of-statement))))))
      pos)))

(defun pl1-downcase-keywords ()
  "Downcase all Pl1 keywords in the buffer."
  (interactive)
  (pl1-change-keywords 'downcase-word))

(defun pl1-upcase-keywords ()
  "Upcase all Pl1 keywords in the buffer."
  (interactive)
  (pl1-change-keywords 'upcase-word))

(defun pl1-capitalize-keywords ()
  "Capitalize all Pl1 keywords in the buffer."
  (interactive)
  (pl1-change-keywords 'capitalize-word))

;; Change the keywords according to argument.
(defun pl1-change-keywords (change-word)
  (save-excursion
    (let ((keyword-re (concat "\\<\\("
                              (mapconcat 'identity pl1-keywords "\\|")
                              "\\)\\>")))
      (goto-char (point-min))
      (while (re-search-forward keyword-re nil t)
        (funcall change-word -1)))))



;;;
;;; Other functions
;;;
(defun pl1-set-auto-comments ()
  "Insert `{ case }' or `{ NAME }' on this line if appropriate.
Insert `{ case }' if there is an `end' on the line which
ends a case block.  Insert `{ NAME }' if there is an `end'
on the line which ends a function or procedure named NAME."
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (if (and (looking-at "\\<end;")
             (not (save-excursion
                    (end-of-line)
                    (search-backward "{" (pl1-get-beg-of-line) t))))
        (let ((type (car (pl1-calculate-indent))))
          (if (eq type 'declaration)
              ()
            (if (eq type 'case)
                ;; This is a case block
                (progn
                  (end-of-line)
                  (delete-horizontal-space)
                  (insert " { case }"))
              (let ((nest 1))
                ;; Check if this is the end of a function
                (save-excursion
                  (while (not (or (looking-at pl1-defun-re) (bobp)))
                    (backward-sexp 1)
                    (cond ((looking-at pl1-beg-block-re)
                           (setq nest (1- nest)))
                          ((looking-at pl1-end-block-re)
                           (setq nest (1+ nest)))))
                  (if (bobp)
                      (setq nest 1)))
                (if (zerop nest)
                    (progn
                      (end-of-line)
                      (delete-horizontal-space)
                      (insert " { ")
                      (let (b e)
                        (save-excursion
                          (setq b (progn (pl1-beg-of-defun)
                                         (skip-chars-forward "^ \t")
                                         (skip-chars-forward " \t")
                                         (point))
                                e (progn (skip-chars-forward "a-zA-Z0-9_")
                                         (point))))
                        (insert-buffer-substring (current-buffer) b e))
                      (insert " }"))))))))))



;;;
;;; Indentation
;;;
(defconst pl1-indent-alist
  '((block . (+ ind pl1-indent-level))
    (case . (+ ind pl1-case-indent))
    (caseblock . ind) (cpp . 0)
    (declaration . (+ ind pl1-indent-level))
    (paramlist . (pl1-indent-paramlist t))
    (comment . (pl1-indent-comment))
    (defun . ind) (contexp . ind)
    (unknown . ind) (string . 0) (progbeg . 0)))

(defun pl1-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (pl1-calculate-indent))
         (type (car indent-str)))
    (cond ((and (eq type 'paramlist)
                (or (memq 'all pl1-auto-lineup)
                    (memq 'paramlist pl1-auto-lineup)))
           (pl1-indent-paramlist)
           (pl1-indent-paramlist))
          ((and (eq type 'declaration)
                (or (memq 'all pl1-auto-lineup)
                    (memq 'declaration  pl1-auto-lineup)))
           (pl1-indent-declaration))
          ((and (eq type 'case) (not (looking-at "^[ \t]*$"))
                (or (memq 'all pl1-auto-lineup)
                    (memq 'case pl1-auto-lineup)))
           (pl1-indent-case)))
    (if (looking-at "[ \t]+$")
        (skip-chars-forward " \t"))))

(defun pl1-indent-line ()
  "Indent current line as a Pl1 statement."
  (let* ((indent-str (pl1-calculate-indent))
         (type (car indent-str))
         (ind (car (cdr indent-str))))
    ;; Labels should not be indented.
    (if (and (looking-at "^[0-9a-zA-Z]+[ \t]*:[^=]")
             (not (eq type 'declaration)))
        (search-forward ":" nil t))
    (delete-horizontal-space)
    (cond (; Some things should not be indented
           (or (and (eq type 'declaration) (looking-at pl1-declaration-re))
               (eq type 'cpp))
           ())
          (; Other things should have no extra indent
           (looking-at pl1-noindent-re)
           (indent-to ind))
          (; Nested functions should be indented
           (looking-at pl1-defun-re)
           (if (and pl1-indent-nested-functions
                    (eq type 'defun))
               (indent-to (+ ind pl1-indent-level))
             (indent-to ind)))
          (; But most lines are treated this way
           (indent-to (eval (cdr (assoc type pl1-indent-alist))))
           ))))

(defun pl1-calculate-indent ()
  "Calculate the indent of the current Pl1 line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
           (oldpos (point))
           (state (save-excursion (parse-partial-sexp (point-min) (point))))
           (nest 0) (par 0) (complete (looking-at "[ \t]*end\\>"))
           (elsed (looking-at "[ \t]*else\\>")) (funccnt 0)
           (did-func (looking-at "[ \t]*\\(procedure\\|function\\)\\>"))
           (type (catch 'nesting
                   ;; Check if inside a string, comment or parenthesis
                   (cond ((nth 3 state) (throw 'nesting 'string))
                         ((nth 4 state) (throw 'nesting 'comment))
                         ((> (car state) 0)
                          (goto-char (scan-lists (point) -1 (car state)))
                          (setq par (1+ (current-column))))
                         ((save-excursion (beginning-of-line)
                                          (eq (following-char) ?#))
                          (throw 'nesting 'cpp)))
                   ;; Loop until correct indent is found
                   (while t
                     (backward-sexp 1)
                     (cond (;--Escape from case statements
                            (and (looking-at "[A-Za-z0-9]+[ \t]*:[^=]")
                                 (not complete)
                                 (save-excursion (skip-chars-backward " \t")
                                                 (bolp))
                                 (= (save-excursion
                                      (end-of-line) (backward-sexp) (point))
                                    (point))
                                 (> (save-excursion (goto-char oldpos)
                                                    (beginning-of-line)
                                                    (point))
                                    (point)))
                            (throw 'nesting 'caseblock))
                           (;--Beginning of program
                            (looking-at pl1-progbeg-re)
                            (throw 'nesting 'progbeg))
                           (;--No known statements
                            (bobp)
                            (throw 'nesting 'progbeg))
                           (;--Nest block outwards
                            (looking-at pl1-beg-block-re)
                            (if (= nest 0)
                                (cond ((looking-at "case\\>")
                                       (throw 'nesting 'case))
                                      ((looking-at "record\\>")
                                       (throw 'nesting 'declaration))
                                      (t (throw 'nesting 'block)))
                              (if (and (looking-at "record\\>") (= nest 1))
                                  (setq funccnt (1- funccnt)))
                              (setq nest (1- nest))))
                           (;--Nest block inwards
                            (looking-at pl1-end-block-re)
                            (if (and (looking-at "end\\s ")
                                     elsed (not complete))
                                (throw 'nesting 'block))
                            (if (= nest 0)
                                (setq funccnt (1+ funccnt)))
                            (setq complete t
                                  nest (1+ nest)))
                           (;--Defun (or parameter list)
                            (and (looking-at pl1-defun-re)
                                 (progn (setq funccnt (1- funccnt)
                                              did-func t)
                                        (or (bolp) (< funccnt 0))))
                            ;; Prevent searching whole buffer
                            (if (and (bolp) (>= funccnt 0))
                                (throw 'nesting 'progbeg))
                            (if (= 0 par)
                                (throw 'nesting 'defun)
                              (setq par 0)
                              (let ((n 0))
                                (while (re-search-forward
                                        "\\(\\<record\\>\\)\\|\\<end\\>"
                                        oldpos t)
                                  (if (match-end 1)
                                      (setq n (1+ n)) (setq n (1- n))))
                                (if (> n 0)
                                    (throw 'nesting 'declaration)
                                  (throw 'nesting 'paramlist)))))
                           (;--Declaration part
                            (and (looking-at pl1-declaration-re)
                                 (not did-func)
                                 (= funccnt 0))
                            (if (save-excursion
                                  (goto-char oldpos)
                                  (forward-line -1)
                                  (looking-at "^[ \t]*$"))
                                (throw 'nesting 'unknown)
                              (throw 'nesting 'declaration)))
                           (;--If, else or while statement
                            (and (not complete)
                                 (looking-at pl1-sub-block-re))
                            (throw 'nesting 'block))
                           (;--Found complete statement
                            (save-excursion (forward-sexp 1)
                                            (= (following-char) ?\;))
                            (setq complete t))
                           )))))

      ;; Return type of block and indent level.
      (if (> par 0)                               ; Unclosed Parenthesis
          (list 'contexp par)
        (list type (pl1-indent-level))))))

(defun pl1-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case-statements or records."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^=]")
        (search-forward ":" nil t)
      (if (looking-at ".*=[ \t]*record\\>")
          (search-forward "=" nil t)))
    (skip-chars-forward " \t")
    (current-column)))

(defun pl1-indent-comment ()
  "Return indent for current comment."
  (save-excursion
    (re-search-backward "\\((\\*\\)\\|{" nil t)
    (if (match-beginning 1)
        (1+ (current-column))
      (current-column))))

(defun pl1-indent-case ()
  "Indent within case statements."
  (let ((savepos (point-marker))
        (end (prog2
                 (end-of-line)
                 (point-marker)
               (re-search-backward "\\<case\\>" nil t)))
        (beg (point))
        (ind 0))
    ;; Get right indent
    (while (< (point) end)
      (if (re-search-forward
           "^[ \t]*[^ \t,:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
           (marker-position end) 'move)
          (forward-char -1))
      (if (< (point) end)
          (progn
            (delete-horizontal-space)
            (if (> (current-column) ind)
                (setq ind (current-column)))
            (pl1-end-of-statement))))
    (goto-char beg)
    ;; Indent all case statements
    (while (< (point) end)
      (if (re-search-forward
           "^[ \t]*[^][ \t,\\.:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
           (marker-position end) 'move)
          (forward-char -1))
      (indent-to (1+ ind))
      (if (/= (following-char) ?:)
          ()
        (forward-char 1)
        (delete-horizontal-space)
        (insert " "))
      (pl1-end-of-statement))
    (goto-char savepos)))

(defun pl1-indent-paramlist (&optional arg)
  "Indent current line in parameterlist.
If optional arg is non-nil, just return the
indent of the current line in parameterlist."
  (save-excursion
    (let* ((oldpos (point))
           (stpos (progn (goto-char (scan-lists (point) -1 1)) (point)))
           (stcol (1+ (current-column)))
           (edpos (progn (pl1-declaration-end)
                         (search-backward ")" (pl1-get-beg-of-line) t)
                         (point)))
           (usevar (re-search-backward "\\<var\\>" stpos t)))
      (if arg (progn
                ;; If arg, just return indent
                (goto-char oldpos)
                (beginning-of-line)
                (if (or (not usevar) (looking-at "[ \t]*var\\>"))
                    stcol (+ 4 stcol)))
        (goto-char stpos)
        (forward-char 1)
        (delete-horizontal-space)
        (if (and usevar (not (looking-at "var\\>")))
            (indent-to (+ 4 stcol)))
        (pl1-indent-declaration nil stpos edpos)))))

(defun pl1-indent-declaration (&optional arg start end)
  "Indent current lines as declaration, lining up the `:'s or `='s."
  (let ((pos (point-marker)))
    (if (and (not (or arg start)) (not (pl1-declaration-beg)))
        ()
      (let ((lineup (if (or (looking-at "\\<var\\>\\|\\<record\\>") arg start)
                        ":" "="))
            (stpos (if start start
                       (forward-word 2) (backward-word 1) (point)))
            (edpos (set-marker (make-marker)
                               (if end end
                                 (max (progn (pl1-declaration-end)
                                             (point))
                                      pos))))
            ind)

        (goto-char stpos)
        ;; Indent lines in record block
        (if arg
            (while (<= (point) edpos)
              (beginning-of-line)
              (delete-horizontal-space)
              (if (looking-at "end\\>")
                  (indent-to arg)
                (indent-to (+ arg pl1-indent-level)))
              (forward-line 1)))

        ;; Do lineup
        (setq ind (pl1-get-lineup-indent stpos edpos lineup))
        (goto-char stpos)
        (while (and (<= (point) edpos) (not (eobp)))
          (if (search-forward lineup (pl1-get-end-of-line) 'move)
              (forward-char -1))
          (delete-horizontal-space)
          (indent-to ind)
          (if (not (looking-at lineup))
              (forward-line 1) ; No more indent if there is no : or =
            (forward-char 1)
            (delete-horizontal-space)
            (insert " ")
            ;; Indent record block
            (if (looking-at "record\\>")
                (pl1-indent-declaration (current-column)))
            (forward-line 1)))))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char pos))))

;  "Return the indent level that will line up several lines within the region
;from b to e nicely. The lineup string is str."
(defun pl1-get-lineup-indent (b e str)
  (save-excursion
    (let ((ind 0)
          (reg (concat str "\\|\\(\\<record\\>\\)\\|" pl1-defun-re)))
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
        (and (re-search-forward reg (min e (pl1-get-end-of-line 2)) 'move)
             (cond ((match-beginning 1)
                    ;; Skip record blocks
                    (pl1-declaration-end))
                   ((match-beginning 2)
                    ;; We have entered a new procedure.  Exit.
                    (goto-char e))
                   (t
                    (goto-char (match-beginning 0))
                    (skip-chars-backward " \t")
                    (if (> (current-column) ind)
                        (setq ind (current-column)))
                    (goto-char (match-end 0))
                    (end-of-line)
                    ))))
      ;; In case no lineup was found
      (if (> ind 0)
          (1+ ind)
        ;; No lineup-string found
        (goto-char b)
        (end-of-line)
        (skip-chars-backward " \t")
        (1+ (current-column))))))



;;;
;;; Completion

(defun pl1-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
        (if (or (> (1+ diff) (length str1))
                (> (1+ diff) (length str2)))
            (throw 'done diff))
        (or (equal (aref str1 diff) (aref str2 diff))
            (throw 'done diff))
        (setq diff (1+ diff))))))

;; Calculate all possible completions for functions if argument is `function',
;; completions for procedures if argument is `procedure' or both functions and
;; procedures otherwise.

(defun pl1-func-completion (type pl1-str)
  ;; Build regular expression for function/procedure names
  (save-excursion
    (if (string= pl1-str "")
        (setq pl1-str "[a-zA-Z_]"))
    (let ((pl1-str (concat (cond
                               ((eq type 'procedure) "\\<\\(procedure\\)\\s +")
                               ((eq type 'function) "\\<\\(function\\)\\s +")
                               (t "\\<\\(function\\|procedure\\)\\s +"))
                              "\\<\\(" pl1-str "[a-zA-Z0-9_.]*\\)\\>"))
          (pl1-all ())
          match)

      (if (not (looking-at "\\<\\(function\\|procedure\\)\\>"))
          (re-search-backward "\\<\\(function\\|procedure\\)\\>" nil t))
      (forward-char 1)

      ;; Search through all reachable functions
      (while (pl1-beg-of-defun)
        (if (re-search-forward pl1-str (pl1-get-end-of-line) t)
            (progn (setq match (buffer-substring (match-beginning 2)
                                                 (match-end 2)))
                   (push match pl1-all)))
        (goto-char (match-beginning 0)))

      pl1-all)))

(defun pl1-get-completion-decl (pl1-str)
  ;; Macro for searching through current declaration (var, type or const)
  ;; for matches of `str' and adding the occurrence to `all'
  (let ((end (save-excursion (pl1-declaration-end)
                             (point)))
        (pl1-all ())
        match)
    ;; Traverse lines
    (while (< (point) end)
      (if (re-search-forward "[:=]" (pl1-get-end-of-line) t)
          ;; Traverse current line
          (while (and (re-search-backward
                       (concat "\\((\\|\\<\\(var\\|type\\|const\\)\\>\\)\\|"
                               pl1-symbol-re)
                       (pl1-get-beg-of-line) t)
                      (not (match-end 1)))
            (setq match (buffer-substring (match-beginning 0) (match-end 0)))
            (if (string-match (concat "\\<" pl1-str) match)
                (push match pl1-all))))
      (if (re-search-forward "\\<record\\>" (pl1-get-end-of-line) t)
          (pl1-declaration-end)
        (forward-line 1)))

    pl1-all))

(defun pl1-type-completion (pl1-str)
  "Calculate all possible completions for types."
  (let ((start (point))
        (pl1-all ())
        goon)
    ;; Search for all reachable type declarations
    (while (or (pl1-beg-of-defun)
               (setq goon (not goon)))
      (save-excursion
        (if (and (< start (prog1 (save-excursion (pl1-end-of-defun)
                                                 (point))
                            (forward-char 1)))
                 (re-search-forward
                  "\\<type\\>\\|\\<\\(begin\\|function\\|procedure\\)\\>"
                  start t)
                 (not (match-end 1)))
            ;; Check current type declaration
            (setq pl1-all
                  (nconc (pl1-get-completion-decl pl1-str)
                         pl1-all)))))

    pl1-all))

(defun pl1-var-completion (prefix)
  "Calculate all possible completions for variables (or constants)."
  (save-excursion
    (let ((start (point))
          (pl1-all ())
          goon twice)
      ;; Search for all reachable var declarations
      (while (or (pl1-beg-of-defun)
                 (setq goon (not goon)))
        (save-excursion
          (if (> start (prog1 (save-excursion (pl1-end-of-defun)
                                              (point))))
              ()                        ; Declarations not reachable
            (if (search-forward "(" (pl1-get-end-of-line) t)
                ;; Check parameterlist
                ;; FIXME: pl1-get-completion-decl doesn't understand
                ;; the var declarations in parameter lists :-(
                (setq pl1-all
                      (nconc (pl1-get-completion-decl prefix)
                             pl1-all)))
            (setq twice 2)
            (while (>= (setq twice (1- twice)) 0)
              (cond
               ((and (re-search-forward
                      (concat "\\<\\(var\\|const\\)\\>\\|"
                              "\\<\\(begin\\|function\\|procedure\\)\\>")
                      start t)
                     (not (match-end 2)))
                ;; Check var/const declarations
                (setq pl1-all
                      (nconc (pl1-get-completion-decl prefix)
                             pl1-all)))
               ((match-end 2)
                (setq twice 0)))))))
      pl1-all)))


(defun pl1-keyword-completion (keyword-list pl1-str)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (let ((pl1-all ()))
    (dolist (s keyword-list)
      (if (string-match (concat "\\<" pl1-str) s)
          (push s pl1-all)))
    pl1-all))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defvar pl1-completion-cache nil)

(defun pl1-completion (pl1-str pl1-pred pl1-flag)
  (let ((all (car pl1-completion-cache)))
    ;; Check the cache's freshness.
    (unless (and pl1-completion-cache
                 (pjb-string-prefix-p (nth 1 pl1-completion-cache) pl1-str)
                 (eq (current-buffer) (nth 2 pl1-completion-cache))
                 (eq (field-beginning) (nth 3 pl1-completion-cache)))
      (let ((state (car (pl1-calculate-indent))))
        (setq all
              ;; Determine what should be completed
              (cond
               (              ;--Within a declaration or parameterlist
                (or (eq state 'declaration) (eq state 'paramlist)
                    (and (eq state 'defun)
                         (save-excursion
                           (re-search-backward ")[ \t]*:"
                                               (pl1-get-beg-of-line) t))))
                (if (or (eq state 'paramlist) (eq state 'defun))
                    (pl1-beg-of-defun))
                (nconc
                 (pl1-type-completion pl1-str)
                 (pl1-keyword-completion pl1-type-keywords pl1-str)))
               (                        ;--Starting a new statement
                (and (not (eq state 'contexp))
                     (save-excursion
                       (skip-chars-backward "a-zA-Z0-9_.")
                       (backward-sexp 1)
                       (or (looking-at pl1-nosemi-re)
                           (progn
                             (forward-sexp 1)
                             (looking-at "\\s *\\(;\\|:[^=]\\)")))))
                (nconc
                 (pl1-var-completion pl1-str)
                 (pl1-func-completion 'procedure pl1-str)
                 (pl1-keyword-completion pl1-start-keywords pl1-str)))
               (t                       ;--Anywhere else
                (nconc
                 (pl1-var-completion pl1-str)
                 (pl1-func-completion 'function pl1-str)
                 (pl1-keyword-completion pl1-separator-keywords
                                            pl1-str)))))

        (setq pl1-completion-cache
              (list all pl1-str (current-buffer) (field-beginning)))))

    ;; Now we have built a list of all matches. Give response to caller
    (complete-with-action pl1-flag all pl1-str pl1-pred)))

(defvar pl1-last-word-numb 0)
(defvar pl1-last-word-shown nil)
(defvar pl1-last-completions nil)

(defun pl1-complete-word ()
  "Complete word at current point.
\(See also `pl1-toggle-completions', `pl1-type-keywords',
`pl1-start-keywords' and `pl1-separator-keywords'.)"
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
         (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point))))

    ;; Toggle-completions inserts whole labels
    (if pl1-toggle-completions
        (let* ((pl1-str (buffer-substring b e))
               (allcomp (if (and pl1-toggle-completions
                                 (string= pl1-last-word-shown pl1-str))
                            pl1-last-completions
                          (all-completions pl1-str 'pl1-completion))))
          ;; Update entry number in list
          (setq pl1-last-completions allcomp
                pl1-last-word-numb
                (if (>= pl1-last-word-numb (1- (length allcomp)))
                    0
                  (1+ pl1-last-word-numb)))
          (setq pl1-last-word-shown (elt allcomp pl1-last-word-numb))
          ;; Display next match or same string if no match was found
          (if allcomp
              (progn
                (goto-char e)
                (insert-before-markers pl1-last-word-shown)
                (delete-region b e))
            (message "(No match)")))
      ;; The other form of completion does not necessarily do that.
      (completion-in-region b e 'pl1-completion))))

(defun pl1-show-completions ()
  "Show all possible completions at current point."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
         (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
         (pl1-str (buffer-substring b e))
         (allcomp (if (and pl1-toggle-completions
                           (string= pl1-last-word-shown pl1-str))
                      pl1-last-completions
                    (all-completions pl1-str 'pl1-completion))))
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list allcomp pl1-str))
    ;; Wait for a keypress. Then delete *Completion*  window
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Completions*")))))


(defun pl1-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
                        (skip-chars-backward " \t")
                        (skip-chars-backward "a-zA-Z0-9_")
                        (point))
                      (progn
                        (skip-chars-forward "a-zA-Z0-9_")
                        (point)))))

(defun pl1-build-defun-re (str &optional arg)
  "Return function/procedure starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "\\)\\>")
    (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun pl1-comp-defun (pl1-str pl1-pred pl1-flag)
  (save-excursion
    (let ((pl1-all nil))

      ;; Build regular expression for functions
      (let ((pl1-str (pl1-build-defun-re (if (string= pl1-str "")
                                                   "[a-zA-Z_]"
                                                 pl1-str))))
        (goto-char (point-min))

        ;; Build a list of all possible completions
        (while (re-search-forward pl1-str nil t)
          (push (match-string 2) pl1-all)))

      ;; Now we have built a list of all matches. Give response to caller
      (complete-with-action pl1-flag pl1-all pl1-str pl1-pred))))

(defun pl1-goto-defun ()
  "Move to specified Pl1 function/procedure.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (pl1-get-default-symbol))
         (default (if (pl1-comp-defun default nil 'lambda)
                      default ""))
         (label
          ;; Do completion with default
          (completing-read (if (not (string= default ""))
                               (concat "Label (default " default "): ")
                             "Label: ")
                           ;; Complete with the defuns found in the
                           ;; current-buffer.
                           (lexical-let ((buf (current-buffer)))
                             (lambda (s p a)
                               (with-current-buffer buf
                                 (pl1-comp-defun s p a))))
                           nil t "")))
    ;; If there was no response on prompt, use default value
    (if (string= label "")
        (setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
        (progn
          (goto-char (point-min))
          (re-search-forward (pl1-build-defun-re label t))
          (beginning-of-line)))))



;;;
;;; Pl1-outline-mode
;;;
(defvar pl1-outline-map
  (let ((map (make-sparse-keymap)))
    (if (fboundp 'set-keymap-name)
        (set-keymap-name pl1-outline-map 'pl1-outline-map))
    (define-key map "\M-\C-a"  'pl1-outline-prev-defun)
    (define-key map "\M-\C-e"  'pl1-outline-next-defun)
    (define-key map "\C-c\C-d" 'pl1-outline-goto-defun)
    (define-key map "\C-c\C-s" 'pl1-show-all)
    (define-key map "\C-c\C-h" 'pl1-hide-other-defuns)
    map)
  "Keymap used in Pl1 Outline mode.")

(define-obsolete-function-alias 'pl1-outline 'pl1-outline-mode "22.1")
(define-minor-mode pl1-outline-mode
  "Outline-line minor mode for Pl1 mode.
When in Pl1 Outline mode, portions
of the text being edited may be made invisible. \\<pl1-outline-map>

Pl1 Outline mode provides some additional commands.

\\[pl1-outline-prev-defun]\
\t- Move to previous function/procedure, hiding everything else.
\\[pl1-outline-next-defun]\
\t- Move to next function/procedure, hiding everything else.
\\[pl1-outline-goto-defun]\
\t- Goto function/procedure prompted for in minibuffer,
\t  hide all other functions.
\\[pl1-show-all]\t- Show the whole buffer.
\\[pl1-hide-other-defuns]\
\t- Hide everything but the current function (function under the cursor).
\\[pl1-outline]\t- Leave pl1-outline-mode."
  :init-value nil :lighter " Outl" :keymap pl1-outline-map
  (add-to-invisibility-spec '(pl1 . t))
  (unless pl1-outline-mode
    (pl1-show-all)))

(defun pl1-outline-change (b e pl1-flag)
  (save-excursion
    ;; This used to use selective display so the boundaries used by the
    ;; callers didn't have to be precise, since it just looked for \n or \^M
    ;; and switched them.
    (goto-char b) (setq b (line-end-position))
    (goto-char e) (setq e (line-end-position)))
  (when (> e b)
    ;; We could try and optimize this in the case where the region is
    ;; already hidden.  But I'm not sure it's worth the trouble.
    (remove-overlays b e 'invisible 'pl1)
    (when (eq pl1-flag ?\^M)
      (let ((ol (make-overlay b e nil t nil)))
        (overlay-put ol 'invisible 'pl1)
        (overlay-put ol 'evaporate t)))))

(defun pl1-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (pl1-outline-change (point-min) (point-max) ?\n))

(defun pl1-hide-other-defuns ()
  "Show only the current defun."
  (interactive)
  (save-excursion
    (let ((beg (progn (if (not (looking-at "\\(function\\|procedure\\)\\>"))
                          (pl1-beg-of-defun))
                      (point)))
          (end (progn (pl1-end-of-defun)
                      (backward-sexp 1)
                      (search-forward "\n\\|\^M" nil t)
                      (point)))
          (opoint (point-min)))
      (goto-char (point-min))

      ;; Hide all functions before current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" beg 'move)
        (pl1-outline-change opoint (1- (match-beginning 0)) ?\^M)
        (setq opoint (point))
        ;; Functions may be nested
        (if (> (progn (pl1-end-of-defun) (point)) beg)
            (goto-char opoint)))
      (if (> beg opoint)
          (pl1-outline-change opoint (1- beg) ?\^M))

      ;; Show current function
      (pl1-outline-change beg end ?\n)
      ;; Hide nested functions
      (forward-char 1)
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" end 'move)
        (setq opoint (point))
        (pl1-end-of-defun)
        (pl1-outline-change opoint (point) ?\^M))

      (goto-char end)
      (setq opoint end)

      ;; Hide all function after current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" nil 'move)
        (pl1-outline-change opoint (1- (match-beginning 0)) ?\^M)
        (setq opoint (point))
        (pl1-end-of-defun))
      (pl1-outline-change opoint (point-max) ?\^M)

      ;; Hide main program
      (if (< (progn (forward-line -1) (point)) end)
          (progn
            (goto-char beg)
            (pl1-end-of-defun)
            (backward-sexp 1)
            (pl1-outline-change (point) (point-max) ?\^M))))))

(defun pl1-outline-next-defun ()
  "Move to next function/procedure, hiding all others."
  (interactive)
  (pl1-end-of-defun)
  (pl1-hide-other-defuns))

(defun pl1-outline-prev-defun ()
  "Move to previous function/procedure, hiding all others."
  (interactive)
  (pl1-beg-of-defun)
  (pl1-hide-other-defuns))

(defun pl1-outline-goto-defun ()
  "Move to specified function/procedure, hiding all others."
  (interactive)
  (pl1-goto-defun)
  (pl1-hide-other-defuns))

(provide 'pl1)

;;; pjb-pl1.el ends here
