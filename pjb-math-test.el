;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-math-test.el --- Tests for pjb-math
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-math)

;; pjb-math.el's only public surface is two interactive helpers that
;; install local key bindings.  We just verify the symbols exist after
;; load (the top-level keymap installation is now guarded by
;; `noninteractive', so loading no longer crashes in batch mode).

(ert-deftest pjb-math/feature-provided ()
  (should (featurep 'pjb-math)))

(ert-deftest pjb-math/symbols-defined ()
  (should (functionp 'set-math-bindings))
  (should (functionp 'set-greek-bindings)))

;;;; pjb-math-test.el                  --                     --          ;;;;
