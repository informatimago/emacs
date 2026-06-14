;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;***
;;;;FILE:               pjb-test-driver.el
;;;;DESCRIPTION
;;;;    Test driver that loads and runs all pjb-* tests.
;;;;    Run this file with: C-x C-f pjb-test-driver.el RET M-x eval-buffer RET
;;;;LEGAL
;;;;    GPL
;;;;    Copyright Pascal Bourguignon
;;;;***
(require 'ert)

;; =============================================================================
;; Test Driver - Run all pjb-*.el tests
;; =============================================================================

(defgroup pjb-test nil
  "Test suite for all pjb-*.el files")

(defun pjb-test-all ()
  "Run all pjb-*.el tests defined in this directory."
  (interactive)
  (let ((test-count (ert-run-tests-bell
                     (ert-tselect t) t t t nil)))
    (if (eql test-count 0)
        (message "No tests were selected.")
      (message "%d tests completed.")
      (setq this-command nil) ; Avoid loop on repeat
      (ert-run-tests-bell
       (ert-tselect t) t t t nil))))

;; =============================================================================
;; Test Module Loaders - Load each test file
;; =============================================================================

(defun pjb-test-load-all ()
  "Load all pjb-*-test.el files in this directory."
  (interactive)
  (let ((test-files
         (list
          (pjb-emacs-source "pjb-animate-test.el")
          (pjb-emacs-source "pjb-advices-test.el")
          (pjb-emacs-source "pjb-ansi-color-test.el")
          (pjb-emacs-source "pjb-asdf-test.el")
          (pjb-emacs-source "pjb-banks-test.el")
          (pjb-emacs-source "pjb-banks-old-test.el")
          (pjb-emacs-source "pjb-browser-test.el")
          (pjb-emacs-source "pjb-c-style-test.el")
          (pjb-emacs-source "pjb-caps-mode-test.el")
          (pjb-emacs-source "pjb-cl-test.el")
          (pjb-emacs-source "pjb-class-test.el")
          (pjb-emacs-source "pjb-color-test.el")
          (pjb-emacs-source "pjb-comint-test.el")
          (pjb-emacs-source "pjb-constants-test.el")
          (pjb-emacs-source "pjb-date-test.el")
          (pjb-emacs-source "pjb-describe-test.el")
          (pjb-emacs-source "pjb-dodo-test.el")
          (pjb-emacs-source "pjb-dot-test.el")
          (pjb-emacs-source "pjb-eval-test.el")
          (pjb-emacs-source "pjb-echo-keys-test.el")
          (pjb-emacs-source "pjb-emacs-patches-test.el")
          (pjb-emacs-source "pjb-emacs-test.el")
          (pjb-emacs-source "pjb-emacs-balance-windows-test.el")
          (pjb-emacs-source "pjb-erc-test.el")
          (pjb-emacs-source "pjb-erc-filter-test.el")
          (pjb-emacs-source "pjb-erc-speak-test.el")
          (pjb-emacs-source "pjb-euro-test.el")
          (pjb-emacs-source "pjb-find-tag-hook-test.el")
          (pjb-emacs-source "pjb-font-test.el")
          (pjb-emacs-source "pjb-frame-server-test.el")
          (pjb-emacs-source "pjb-gnus-test.el")
          (pjb-emacs-source "pjb-google-translate-test.el")
          (pjb-emacs-source "pjb-graph-test.el")
          (pjb-emacs-source "pjb-html-test.el")
          (pjb-emacs-source "pjb-i2p-expression-test.el")
          (pjb-emacs-source "pjb-image-minor-mode-test.el")
          (pjb-emacs-source "pjb-insert-image-test.el")
          (pjb-emacs-source "pjb-java-generate-test.el")
          (pjb-emacs-source "pjb-java-test.el")
          (pjb-emacs-source "pjb-kafka-test.el")
          (pjb-emacs-source "pjb-layers-test.el")
          (pjb-emacs-source "pjb-loader-test.el")
          (pjb-emacs-source "pjb-mail-test.el")
          (pjb-emacs-source "pjb-make-depends-test.el")
          (pjb-emacs-source "pjb-math-test.el")
          (pjb-emacs-source "pjb-milliways-test.el")
          (pjb-emacs-source "pjb-objc-edit-test.el")
          (pjb-emacs-source "pjb-objc-gen-test.el")
          (pjb-emacs-source "pjb-objc-ide-test.el")
          (pjb-emacs-source "pjb-objc-mode-test.el")
          (pjb-emacs-source "pjb-object-test.el")
          (pjb-emacs-source "pjb-oldies-test.el")
          (pjb-emacs-source "pjb-page-test.el")
          (pjb-emacs-source "pjb-patch-test.el")
          (pjb-emacs-source "pjb-pmatch-test.el")
          (pjb-emacs-source "pjb-queue-test.el")
          (pjb-emacs-source "pjb-roman-test.el")
          (pjb-emacs-source "pjb-searches-test.el")
          (pjb-emacs-source "pjb-selftrade-test.el")
          (pjb-emacs-source "pjb-server-test.el")
          (pjb-emacs-source "pjb-shell-test.el")
          (pjb-emacs-source "pjb-sources-test.el")
          (pjb-emacs-source "pjb-speak-test.el")
          (pjb-emacs-source "pjb-state-coding-test.el")
          (pjb-emacs-source "pjb-strings-test.el")
          (pjb-emacs-source "pjb-termbin-test.el")
          (pjb-emacs-source "pjb-thi-test.el")
          (pjb-emacs-source "pjb-tla-test.el")
          (pjb-emacs-source "pjb-transpose-test.el")
          (pjb-emacs-source "pjb-utilities-test.el")
          (pjb-emacs-source "pjb-vm-test.el")
          (pjb-emacs-source "pjb-vm-kill-file-test.el")
          (pjb-emacs-source "pjb-work-test.el")
          (pjb-emacs-source "pjb-xcode-test.el")
          (pjb-emacs-source "pjb-xml-test.el")
          (pjb-emacs-source "pjb-xresources-test.el")
          (pjb-emacs-source "pjb-zone-test.el")
          """")))
    (dolist (file test-files)
      (when (and file (file-exists-p file))
        (load file nil t)
        (princ (format "Loaded: %s\n" file))))))

;; =============================================================================
;; Main Test Execution
;; =============================================================================

;; Define a test that checks the driver works
(ert-deftest test/pjb-test--driver-loads ()
  "Test that the test driver exists and is callable."
  (assert (fboundp 'pjb-test-load-all))
  (assert (fboundp 'pjb-test-all))
  :success)

;; =============================================================================
;; Auto-load tests on demand
;; =============================================================================

;;;###autoload
(defmacro pjb-run-tests (&optional interactive-p)
  "Run all pjb-*.el tests, loading them first if needed.
INTERACTIVE-P: if non-nil, make interactive command."
  (if interactive-p
      `(progn (pjb-test-load-all) (pjb-test-all))
    `(progn (pjb-test-load-all) (ert-run-tests-bell (ert-tselect t) t t t nil))))

;; Execute on load
(pjb-test-load-all)

(provide 'pjb-test-driver)
;;;; THE END ;;;;
