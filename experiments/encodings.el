;;; -*- coding:utf-8 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language & character encoding stuff:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (standard-display-european 1) ;;is semi-obsolete, but it works better than:
;; (standard-display-8bit 128 255)

(set-input-mode nil nil t nil) ;; INTERRUPT FLOW META [QUIT]
;; (setq meta-prefix-char nil) ; to split ESC from M- 

(when (fboundp 'unify-8859-on-encoding-mode)
  (unify-8859-on-encoding-mode 1))
(when (fboundp 'unify-8859-on-decoding-mode)
  (unify-8859-on-decoding-mode 1))

(setq default-enable-multibyte-characters      t
      unibyte-display-via-language-environment nil)

;; (setq my-latin (if (assoc-ignore-case "Latin-9" language-info-alist) 9 1))
;; ;; For coding-system we don't specify *-unix to allow it to load DOS files.
;; (cond
;;   ((= my-latin 1) (setq my-lenv     "Latin-1"
;;                         my-encoding 'iso-8859-1
;;                         x-encoding  "iso8859-1"))
;;   ((= my-latin 9) (setq my-lenv     "Latin-9"
;;                         my-encoding 'iso-8859-15
;;                         x-encoding  "iso8859-15"))
;;   (t (error "Invalid value for my-latin variable.")))

(progn
  (case system-type
    (darwin 
     (set-language-environment                "utf-8")
     (prefer-coding-system                    'utf-8-unix)
     (set-default-coding-systems              'utf-8-unix)
     (set-keyboard-coding-system              'utf-8-unix)
     (set-terminal-coding-system              'utf-8-unix)
     (set-clipboard-coding-system             'utf-8-unix)
     (set-selection-coding-system             'utf-8-unix)
     (setq default-buffer-file-coding-system  'utf-8-unix
           default-file-name-coding-system    'utf-8-unix
           default-terminal-coding-system     'utf-8-unix
           default-keyboard-coding-system     'utf-8-unix
           default-sendmail-coding-system     'utf-8-unix
           default-process-coding-system      '(utf-8-unix . utf-8-unix))
     (modify-coding-system-alist 'process ".*shell\\'"     'utf-8-unix)
     (modify-coding-system-alist 'process "\\*shell\\*\\'" 'utf-8-unix)
     (modify-coding-system-alist 'process ".*lisp\\'"      'utf-8-unix))
    (otherwise
     (set-language-environment                "utf-8")
     (prefer-coding-system                    'utf-8-unix)
     (set-default-coding-systems              'utf-8-unix)
     (set-keyboard-coding-system              'iso-8859-1-unix)
     (set-terminal-coding-system              'iso-8859-1-unix)
     (set-clipboard-coding-system             'utf-8-unix)
     (set-selection-coding-system             'utf-8-unix)
     (setq default-buffer-file-coding-system  'utf-8-unix
           default-file-name-coding-system    'utf-8-unix
           default-terminal-coding-system     'iso-8859-1-unix
           default-keyboard-coding-system     'iso-8859-1-unix
           default-sendmail-coding-system     'utf-8-unix
           default-process-coding-system      '(utf-8-unix . utf-8-unix))
     (modify-coding-system-alist 'process ".*shell\\'"     'utf-8-unix)
     (modify-coding-system-alist 'process "\\*shell\\*\\'" 'utf-8-unix)
     (modify-coding-system-alist 'process ".*lisp\\'"      'utf-8-unix)))

  (dolist (cs coding-system-list nil)
    (modify-coding-system-alist 'file (format "\\.%s\\'" cs) cs)))

;; and/or set locales like LC_ALL, LC_CTYPE, LANG to contain UTF-8 as for
;; example: LANG=de_DE.UTF-8. Modern Emacsen, I think 21.3 at least,
;; derive their mode of operation from this.

;; You can start your text files à la: ;;; -*- mode: Text; coding: utf-8 -*-
;; Once you've done that you can C-x RET r:
;; revert-buffer-with-coding-system.


;; 	(set-language-environment		'German)
;; 	(setq default-file-name-coding-system	'utf-8)
;; 	(setq file-name-coding-system		'utf-8)
;; 	(setq default-buffer-file-coding-system 'iso-latin-9-unix))
;; 	(set-default-coding-systems		'mac-roman-unix)
;; 	;(setq mac-keyboard-text-encoding	 kTextEncodingISOLatin1)
;; 	(set-keyboard-coding-system		'sjis-mac)
;; 	(set-clipboard-coding-system		'sjis-mac)
;; 	(prefer-coding-system			'mac-roman-unix)
;; 	(modify-coding-system-alist	 'file "\\.tex\\'" 'iso-latin-9-unix)
;; 	(modify-coding-system-alist	 'process
;; "\\*[Ss][Hh][Ee][Ll][Ll].*\\'"  'utf-8-unix)
;; 	;(set-buffer-process-coding-system	'utf-8 'utf8)

(when (boundp 'aquamacs-version)
  (setf mac-command-modifier 'meta
        mac-option-modifier  'alt
        one-buffer-one-frame nil
        initial-frame-alist '((background-color . "#ddffee")
                              (left . 76)
                              (top . 20)
                              (width . 80)
                              (height . 60))
        default-frame-alist (append initial-frame-alist default-frame-alist)
        cursor-type 'box)
  (smart-frame-positioning-mode nil)
  (cua-mode 0)
  (when (load "scroll-bar" nil t)
    (defun scroll-bar-columns (side)
      "Return the width, measured in columns, of the vertical scrollbar on SIDE.
SIDE must be the symbol `left' or `right'."
      (let* ((wsb   (window-scroll-bars))
             (vtype (nth 2 wsb))
             (cols  (nth 1 wsb)))
        (cond
          ((not (memq side '(left right nil)))
           (error "`left' or `right' expected instead of %S" side))
          ((and (eq vtype side) cols))
          ((eq (frame-parameter nil 'vertical-scroll-bars) side)
           ;; nil means it's a non-toolkit scroll bar, and its width in
           ;; columns is 14 pixels rounded up.
           (ceiling (or (frame-parameter nil 'scroll-bar-width) 14)
                    (frame-char-width)))
          (0))))))



;; global key map - generic
(.EMACS "setting global key map")


;; feature simple is not provided on emacs < 22, so we use load-library:
(load-library "simple") (define-key ctl-x-map              "." nil)
(require 'iso-transl)   (define-key iso-transl-ctl-x-8-map "E" [342604])



;; For control, there's no distinction between shift and plain!
;; The only control keys are: @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
;; \C-^ is free.
;; Cannot use \C-i because it's TAB
;; Cannot use \C-m because it's CR
;; Cannot use \C-[ because it's ESC!
;; Cannot use \C-{ or \C-}  (no distinction between shift and plain)
;;
;; [?\C-c f5]
;; [(control c) f5]
;; (kbd "C-c <f5>")
;;
;; C-z is used now by elscreen.



(defun disabled ()
  (interactive)
  (beep))

(defun insert-sharp-brace ()
  (interactive)
  (insert "#[]")
  (forward-char -1))

(defun reset-movement-keypad ()
  "Locally set the keys <insert>, <suppr>, <home>, <end>, <prior> and <next>."
  (interactive)
  (local-set-key (kbd "<home>")        'beginning-of-buffer)
  (local-set-key (kbd "<end>")         'end-of-buffer)
  (local-set-key (kbd "<prior>")       'scroll-down)
  (local-set-key (kbd "<next>")        'scroll-up))

(defun swap-brackets-parens ()
  (interactive)
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\] ?\)))

(defun normal-brackets-parens ()
  (interactive)
  (keyboard-translate ?\( ?\()
  (keyboard-translate ?\) ?\))
  (keyboard-translate ?\[ ?\[)
  (keyboard-translate ?\] ?\]))

(defun translate-powerbook-keyboard ()
  (interactive)
  (keyboard-translate ?\§ ?\`)
  (keyboard-translate ?\± ?\~))


(defmacro define-force-justification (direction)
  `(defun ,(intern (format "force-set-justification-%s" direction)) (start end)
     (interactive "r")
     (let ((mode major-mode))
       (text-mode)
       (,(intern (format "set-justification-%s" direction))  start end)
       (funcall mode))))
(define-force-justification left)
(define-force-justification center)
(define-force-justification right)
(define-force-justification full)

;; Advance key map setting: get a sane keyboard when loading this file fails.
(global-set-key (kbd "C-x RET C-h")   'describe-prefix-bindings)
;; (global-set-key (kbd "C-x 5 o")     'other-frame-non-excluded)

(global-set-key (kbd "<home>")        'beginning-of-buffer)
(global-set-key (kbd "<end>")         'end-of-buffer)
(global-set-key (kbd "<prior>")       'scroll-down)
(global-set-key (kbd "<next>")        'scroll-up)

(global-set-key (kbd "C-c C-s")       'search-forward-regexp)
(global-set-key (kbd "C-c C-r")       'search-backward-regexp)

(global-set-key (kbd "<f5>")          'set-justification-left)
(global-set-key (kbd "<f6>")          'set-justification-full)
(global-set-key (kbd "<f7>")          'set-justification-right)
(global-set-key (kbd "C-c <f5>")      'force-set-justification-left)
(global-set-key (kbd "C-c <f6>")      'force-set-justification-full)
(global-set-key (kbd "C-c <f7>")      'force-set-justification-right)
(global-set-key (kbd "C-c .")         'forward-sexp)
(global-set-key (kbd "C-c ,")         'backward-sexp)
(global-set-key (kbd "C-x DEL")       'disabled)
(global-set-key (kbd "C-<delete>")    'disabled)
(global-set-key (kbd "C-<backspace>") 'disabled)
(global-set-key (kbd "A-x")           'insert-sharp-brace)
(global-set-key (kbd "M-[")           'insert-parentheses)
(global-set-key (kbd "M-]")           'move-past-close-and-reindent)
(global-set-key "\M-["                'insert-parentheses)
(global-set-key "\M-]"                'move-past-close-and-reindent)

(global-set-key (kbd "C-<f9>")  (lambda()(interactive)(set-input-method 'chinese-py-b5)))
(global-set-key (kbd "C-<f10>") (lambda()(interactive)(set-input-method 'cyrillic-yawerty)))
(global-set-key (kbd "C-<f11>") (lambda()(interactive)(set-input-method 'greek)))
(global-set-key (kbd "C-<f12>") (lambda()(interactive)(set-input-method 'hebrew)))
;; (autoload 'hebr-switch  "hebwork"  "Toggle Hebrew mode.")
;; (global-set-key (kbd "C-<f12>") 'hebr-switch)

(when (and (fboundp 'pjb-search-backward-region)
           (fboundp 'pjb-search-forward-region))
  (global-set-key (kbd "<f9>")   'pjb-search-backward-region)
  (global-set-key (kbd "<f10>")  'pjb-search-forward-region))

(when (and (fboundp 's2p-calculette)
           (fboundp 's2p-calculette-to-lisp))
  (global-set-key (kbd "C-c =")  's2p-calculette)
  (global-set-key (kbd "C-c +")  's2p-calculette-to-lisp))

(global-set-key (kbd "C-c _")    'google-search-region)
(when (fboundp 'invoke-ding-dictionary)
  (global-set-key (kbd "C-c -")   'invoke-ding-dictionary))

;; (delete-selection-mode t)
(if (fboundp 'delete-region-and-yank)
    (global-set-key (kbd "C-y")  'delete-region-and-yank) 
    (global-set-key (kbd "C-y")  'yank))

;; A strange configuration with a narrow frame...
(when (< (frame-parameter (selected-frame) 'width) 42)
  (global-set-key (kbd "C-h") 'backward-delete-char-untabify))


(case window-system
  ((nil)
   (.EMACS "Setting terminal keyboard")
   (global-set-key "OF"    (function end-of-buffer))
   (global-set-key "OH"    (function beginning-of-buffer))
   (global-unset-key "[")
   (global-set-key "[15~"  (function set-justification-left)) ; <f5>
   (global-set-key "[17~"  (function set-justification-center)) ; <f6>
   (global-set-key "[18~"  (function set-justification-right)) ; <f7>
   (global-set-key "[19~"  (lambda()(interactive)(beep))) ; <f8>
   (global-set-key "[20~"  (lambda()(interactive)(beep))) ; <f9>
   (global-set-key "[21~"  (lambda()(interactive)(beep))) ; <f10>
   (global-set-key "[23~"  (lambda()(interactive)(beep))) ; <f11>
   (global-set-key "[24~"  (lambda()(interactive)(beep))) ; <f12>
   (set-keyboard-coding-system 'iso-8859-15)
   (normal-erase-is-backspace-mode 0)
   (.EMACS "C-h = %S" (key-binding "\C-h"))
   (.EMACS "DEL = %S" (key-binding "\C-?")))
  ((x)
   (.EMACS "Setting X keyboard")
   (define-key global-map [(delete)]    "\C-d")
   (make-face-bold 'bold-italic))
  ((mac)
   (.EMACS "Setting Macintosh keyboard")
   (setq *window-manager-y-offset* (+ 24 24))
   (set-keyboard-coding-system 'mac-roman)
   (setq mac-command-key-is-meta t
         mac-reverse-ctrl-meta   nil)
   (translate-powerbook-keyboard)))




(defvar scroll-page-delimiter "")
(make-local-variable 'scroll-page-delimiter)
(setf scroll-page-delimiter "Software Design Notes")

(defun scroll-page-up ()
  (interactive)
  (if (re-search-forward scroll-page-delimiter nil t)
      (progn
        (goto-char (match-beginning 0))
        (recenter 0)
        (forward-line 1))
      (message ".EMACS: Last page")))

(defun scroll-page-down ()
  (interactive)
  (if (re-search-backward scroll-page-delimiter nil t 2)
      (progn
        (goto-char (match-beginning 0))
        (recenter 0)
        (forward-line 1))
      (message ".EMACS: First page")))

(defvar scroll-page-mode nil)
(make-local-variable 'scroll-page-mode)

(defun scroll-page-mode ()
  (interactive)
  (if scroll-page-mode
      (progn
        (local-set-key (kbd "<next>")  'scroll-up)
        (local-set-key (kbd "<prior>") 'scroll-down)
        (setf scroll-page-mode nil))
      (progn
        (local-set-key (kbd "<next>")  'scroll-page-up)
        (local-set-key (kbd "<prior>") 'scroll-page-down)
        (setf scroll-page-mode t))))





(standard-display-ascii #o200 (vector (decode-char 'ucs #x253c)))
(standard-display-ascii #o201 (vector (decode-char 'ucs #x251c)))
(standard-display-ascii #o202 (vector (decode-char 'ucs #x252c)))
(standard-display-ascii #o203 (vector (decode-char 'ucs #x250c)))
(standard-display-ascii #o204 (vector (decode-char 'ucs #x2524)))
(standard-display-ascii #o205 (vector (decode-char 'ucs #x2502)))
(standard-display-ascii #o206 (vector (decode-char 'ucs #x2510)))
(standard-display-ascii #o210 (vector (decode-char 'ucs #x2534)))
(standard-display-ascii #o211 (vector (decode-char 'ucs #x2514)))
(standard-display-ascii #o212 (vector (decode-char 'ucs #x2500)))
(standard-display-ascii #o214 (vector (decode-char 'ucs #x2518)))
(standard-display-ascii #o220 [? ])
(standard-display-ascii #o221 [?\` ])
(standard-display-ascii #o222 [?\'])
(standard-display-ascii #o223 [?\"])
(standard-display-ascii #o224 [?\"])
(standard-display-ascii #o225 "* ")
(standard-display-ascii #o226 "--")
(standard-display-ascii #o227 " -- ")


;; some more global key map are defined after loading my personal files below.



(defun erc-meat ()
  (interactive)
  (reset-movement-keypad))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up faces:

;; (case window-system
;;   ((x) (unless pjb:+pvs-is-running+
;;          ;; Some new Colors for Font-lock.
;;          (require 'font-lock)
;;          (setq font-lock-use-default-fonts nil)
;;          (setq font-lock-use-default-colors nil)
;;          (copy-face 'default  'font-lock-string-face)
;;          (set-face-foreground 'font-lock-string-face "DarkOrchid3")
;;          (copy-face 'italic   'font-lock-comment-face)
;;          ;; (set-face-background 'font-lock-comment-face "honeydew")
;;          (set-face-foreground 'font-lock-comment-face "DarkSeaGreen4")
;;          (copy-face 'bold     'font-lock-function-name-face)
;;          (set-face-foreground 'font-lock-function-name-face "MediumBlue")
;;          (copy-face 'default  'font-lock-keyword-face)
;;          (set-face-foreground 'font-lock-keyword-face "SteelBlue")
;;          (copy-face 'default  'font-lock-type-face)
;;          (set-face-foreground 'font-lock-type-face "DarkOliveGreen")
;;          (set-face-foreground 'bold-italic "Blue")
;;          (set-face-background 'modeline "LightSteelBlue")
;;          (set-face-foreground 'modeline "red") ))
;;   ((mac) (progn
;;            (set-face-foreground 'font-lock-string-face "darkturquoise") ))
;;   ((nil) (progn
;;            ;; (set-foreground-color "black")
;;            ;; (set-background-color "white")
;;            (set-face-foreground 'modeline "black")
;;            (set-face-background 'modeline "green") )))


;;; (set-face-foreground 'font-lock-builtin-face            "Black")
;;; (set-face-foreground 'font-lock-comment-face            "Black")
;;; (set-face-foreground 'font-lock-constant-face           "Black")
;;; (set-face-foreground 'font-lock-doc-face                "Black")
;;; (set-face-foreground 'font-lock-function-name-face      "Black")
;;; (set-face-foreground 'font-lock-keyword-face            "Black")
;;; (set-face-foreground 'font-lock-reference-face          "Black")
;;; (set-face-foreground 'font-lock-string-face             "Black")
;;; (set-face-foreground 'font-lock-syntactic-face-function "Black")
;;; (set-face-foreground 'font-lock-type-face               "Black")
;;; (set-face-foreground 'font-lock-variable-name-face      "Black")
;;; (set-face-foreground 'font-lock-warning-face            "Black")

