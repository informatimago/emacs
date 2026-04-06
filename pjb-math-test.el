;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-math-test.el --- Tests for pjb-math
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; pjb-math.el calls (set-greek-bindings "C-c g") at top level, which
;; fails in `emacs -Q --batch' because there is no usable local
;; keymap.  We pin that documented reality here: the file cannot be
;; loaded in batch mode.  Phase 2 should make the top-level call
;; conditional on `noninteractive'.
(defvar pjb-math-test--load-error
  (condition-case err (require 'pjb-math) (error err)))

(ert-deftest pjb-math/load-currently-fails-in-batch ()
  "Pin the current top-level-side-effect crash so Phase 2 catches the fix."
  (should pjb-math-test--load-error))

(ert-deftest pjb-math/symbols-are-defined-when-loaded ()
  (skip-unless (null pjb-math-test--load-error))
  (should (functionp 'set-math-bindings))
  (should (functionp 'set-greek-bindings)))

;;;; pjb-math-test.el                  --                     --          ;;;;
