;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-loader.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;DESCRIPTION
;;;;
;;;;    Load PJB Emacs sources, in declared order.
;;;;
;;;;    Phase 6 (MODERNIZATION.md) replaced the home-grown sexp walker
;;;;    plus topological sort plus per-startup `find-file' visit of every
;;;;    source file with this hand-maintained ordered list.  If you add a
;;;;    new module, append it to `pjb-loader-sources' (or
;;;;    `pjb-loader-extra-sources' for the heavy / non-light set) and
;;;;    keep dependencies before dependents.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;LEGAL
;;;;    GPL — Copyright Pascal J. Bourguignon 2010 - 2026
;;;;**************************************************************************
(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Path-helper compatibility shims.
;;
;; Pascal's personal startup (~/rc/emacs.el) defines `root', `home', `rc'
;; and `pjb-emacs-source' so that paths adapt to the various MS-Windows /
;; msys2 locations: HOME may be /c/users/FOO seen from msys2 or c:/users/FOO
;; seen from MS-Windows, and the system root may live under c:/msys64/ —
;; likewise for the rc directory.  When this library is loaded standalone,
;; without that startup, provide defaults valid on a normal Unix system,
;; where the filesystem root is "/" and HOME is ~.  Each is guarded by
;; `fboundp' so the personal, platform-aware definitions always win.
;;
;; `rc' designates Pascal's private ~/rc/ directory and must NOT be
;; referenced from this public library, so no default is provided for it.
;; ---------------------------------------------------------------------------

(defvar pjb-loader-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory holding this library (where the pjb-* sources live).")

(defun pjb-loader--join-pathname (directory path)
  "Join DIRECTORY and PATH with exactly one slash, collapsing duplicates.
Either argument may independently have — or lack — a leading or trailing
slash; the result (with ~ and .. resolved) never contains a spurious //."
  (expand-file-name
   (replace-regexp-in-string "/\\{2,\\}" "/" (concat directory "/" path))))

(unless (fboundp 'root)
  (defun root (path)
    "Default (Unix) `root': the filesystem root is \"/\", so PATH is returned as is."
    path))

(unless (fboundp 'home)
  (defun home (path)
    "Default (Unix) `home': resolve PATH under the user's HOME (~)."
    (pjb-loader--join-pathname "~/" path)))

(unless (fboundp 'pjb-emacs-source)
  (defun pjb-emacs-source (path)
    "Default `pjb-emacs-source': resolve PATH relative to this library's directory."
    (pjb-loader--join-pathname pjb-loader-directory path)))

(defvar pjb-loader-noerror nil
  "When non-nil, `pjb-loader-load-all' catches and reports errors from
each loaded file instead of aborting the whole load.")

(defvar pjb-loader-silent nil
  "When non-nil, `pjb-loader-load-all' loads each file silently
\(no \"Loading X...\" echo).")

(defvar pjb-loader-light nil
  "When non-nil, `pjb-loader' loads only the minimal set of pjb sources,
skipping the heavier optional modules in `pjb-loader-extra-sources'.")

;; ---------------------------------------------------------------------------
;; The ordered source list.  Dependencies must come before dependents.
;; ---------------------------------------------------------------------------

(defvar pjb-loader-sources
  '("emacs-uptime.el"
    "pjb-advices.el"
    "pjb-asdf.el"
    "pjb-asm7090.el"
    "pjb-blink.el"
    "pjb-constants.el"
    "pjb-color.el"
    "pjb-cl-magic.el"
    "pjb-cl.el"
    "pjb-class.el"
    "pjb-dodo.el"
    "pjb-emacs.el"
    "pjb-emacs-balance-windows.el"
    "pjb-insert-image.el"
    "pjb-milliways.el"
    "pjb-caps-mode.el"
    "pjb-echo-keys.el"
    "pjb-erc.el"
    "pjb-erc-filter.el"
    "pjb-erc-speak.el"
    "pjb-eval.el"
    "pjb-euro.el"
    "pjb-font.el"
    "pjb-html.el"
    "pjb-list.el"
    "pjb-gnus.el"
    "pjb-mail.el"
    "pjb-object.el"
    "pjb-page.el"
    "pjb-pgp.el"
    "pjb-queue.el"
    "pjb-ruby.el"
    "pjb-shell.el"
    "pjb-sources.el"
    "pjb-clelp.el"
    "pjb-state-coding.el"
    "pjb-strings.el"
    "pjb-utilities.el"
    "pjb-work.el"
    "pjb-xresources.el"
    "pjb-thi.el"
    "pjb-c-style.el"
    "pjb-searches.el"
    "pjb-termbin.el"
    "pjb-java.el"
    "android-classes.el"
    "pjb-pl1.el")
  "Ordered list of pjb-* source files always loaded by `pjb-loader-load-all'.")

(defvar pjb-loader-extra-sources
  '("pjb-unicode.el"
    "pjb-computer-paper.el"
    "pjb-dot.el"
    "pjb-graph.el"
    "pjb-cl-faces.el"
    "pjb-cl-magic-lambda-lists.el"
    "pjb-i2p-expression.el"
    "pjb-s2p-expression.el"
    "pjb-layers.el"
    "pjb-roman.el"
    "pjb-secouer.el"
    "pjb-server.el"
    "pjb-transpose.el"
    "pjb-worldfact.el"
    "pjb-banks.el"
    "pjb-bourse.el"
    "pjb-selftrade.el")
  "Heavier / optional pjb-* sources loaded only when `pjb-loader-light' is nil.")

;; ---------------------------------------------------------------------------
;; Loading.
;; ---------------------------------------------------------------------------

(defun pjb-loader--load-one (path)
  "Load PATH, honouring `pjb-loader-noerror' / `pjb-loader-silent'."
  (if pjb-loader-noerror
      (condition-case err
          (load path nil pjb-loader-silent)
        (error (message "pjb-loader ERROR loading %s: %S" path err)))
    (load path nil pjb-loader-silent)))

(defun pjb-loader-load-all ()
  "Load every file in `pjb-loader-sources' (and, unless `pjb-loader-light',
also `pjb-loader-extra-sources'), in declared order."
  (interactive)
  (mapc #'pjb-loader--load-one pjb-loader-sources)
  (unless pjb-loader-light
    (mapc #'pjb-loader--load-one pjb-loader-extra-sources)))

;; Run on load (preserving the historical behaviour).
(pjb-loader-load-all)

(provide 'pjb-loader)
;;;; THE END ;;;;
