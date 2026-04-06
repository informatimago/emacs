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
       (ert-tselect t) t t t nil)))))

;; =============================================================================
;; Test Module Loaders - Load each test file
;; =============================================================================

(defun pjb-test-load-all ()
  "Load all pjb-*-test.el files in this directory."
  (interactive)
  (let ((test-files
         (list
          "/Users/pjb/src/public/emacs/pjb-animate-test.el"
          "/Users/pjb/src/public/emacs/pjb-advices-test.el"
          "/Users/pjb/src/public/emacs/pjb-ansi-color-test.el"
          "/Users/pjb/src/public/emacs/pjb-asdf-test.el"
          "/Users/pjb/src/public/emacs/pjb-banks-test.el"
          "/Users/pjb/src/public/emacs/pjb-banks-old-test.el"
          "/Users/pjb/src/public/emacs/pjb-browser-test.el"
          "/Users/pjb/src/public/emacs/pjb-c-style-test.el"
          "/Users/pjb/src/public/emacs/pjb-caps-mode-test.el"
          "/Users/pjb/src/public/emacs/pjb-cl-test.el"
          "/Users/pjb/src/public/emacs/pjb-class-test.el"
          "/Users/pjb/src/public/emacs/pjb-color-test.el"
          "/Users/pjb/src/public/emacs/pjb-comint-test.el"
          "/Users/pjb/src/public/emacs/pjb-constants-test.el"
          "/Users/pjb/src/public/emacs/pjb-date-test.el"
          "/Users/pjb/src/public/emacs/pjb-describe-test.el"
          "/Users/pjb/src/public/emacs/pjb-dodo-test.el"
          "/Users/pjb/src/public/emacs/pjb-dot-test.el"
          "/Users/pjb/src/public/emacs/pjb-eval-test.el"
          "/Users/pjb/src/public/emacs/pjb-echo-keys-test.el"
          "/Users/pjb/src/public/emacs/pjb-emacs-patches-test.el"
          "/Users/pjb/src/public/emacs/pjb-emacs-test.el"
          "/Users/pjb/src/public/emacs/pjb-emacs-balance-windows-test.el"
          "/Users/pjb/src/public/emacs/pjb-erc-test.el"
          "/Users/pjb/src/public/emacs/pjb-erc-filter-test.el"
          "/Users/pjb/src/public/emacs/pjb-erc-speak-test.el"
          "/Users/pjb/src/public/emacs/pjb-euro-test.el"
          "/Users/pjb/src/public/emacs/pjb-find-tag-hook-test.el"
          "/Users/pjb/src/public/emacs/pjb-font-test.el"
          "/Users/pjb/src/public/emacs/pjb-frame-server-test.el"
          "/Users/pjb/src/public/emacs/pjb-gnus-test.el"
          "/Users/pjb/src/public/emacs/pjb-google-translate-test.el"
          "/Users/pjb/src/public/emacs/pjb-graph-test.el"
          "/Users/pjb/src/public/emacs/pjb-html-test.el"
          "/Users/pjb/src/public/emacs/pjb-i2p-expression-test.el"
          "/Users/pjb/src/public/emacs/pjb-image-minor-mode-test.el"
          "/Users/pjb/src/public/emacs/pjb-insert-image-test.el"
          "/Users/pjb/src/public/emacs/pjb-java-generate-test.el"
          "/Users/pjb/src/public/emacs/pjb-java-test.el"
          "/Users/pjb/src/public/emacs/pjb-kafka-test.el"
          "/Users/pjb/src/public/emacs/pjb-layers-test.el"
          "/Users/pjb/src/public/emacs/pjb-loader-test.el"
          "/Users/pjb/src/public/emacs/pjb-mail-test.el"
          "/Users/pjb/src/public/emacs/pjb-make-depends-test.el"
          "/Users/pjb/src/public/emacs/pjb-math-test.el"
          "/Users/pjb/src/public/emacs/pjb-milliways-test.el"
          "/Users/pjb/src/public/emacs/pjb-objc-edit-test.el"
          "/Users/pjb/src/public/emacs/pjb-objc-gen-test.el"
          "/Users/pjb/src/public/emacs/pjb-objc-ide-test.el"
          "/Users/pjb/src/public/emacs/pjb-objc-mode-test.el"
          "/Users/pjb/src/public/emacs/pjb-object-test.el"
          "/Users/pjb/src/public/emacs/pjb-oldies-test.el"
          "/Users/pjb/src/public/emacs/pjb-page-test.el"
          "/Users/pjb/src/public/emacs/pjb-patch-test.el"
          "/Users/pjb/src/public/emacs/pjb-pmatch-test.el"
          "/Users/pjb/src/public/emacs/pjb-queue-test.el"
          "/Users/pjb/src/public/emacs/pjb-roman-test.el"
          "/Users/pjb/src/public/emacs/pjb-searches-test.el"
          "/Users/pjb/src/public/emacs/pjb-selftrade-test.el"
          "/Users/pjb/src/public/emacs/pjb-server-test.el"
          "/Users/pjb/src/public/emacs/pjb-shell-test.el"
          "/Users/pjb/src/public/emacs/pjb-sources-test.el"
          "/Users/pjb/src/public/emacs/pjb-speak-test.el"
          "/Users/pjb/src/public/emacs/pjb-state-coding-test.el"
          "/Users/pjb/src/public/emacs/pjb-strings-test.el"
          "/Users/pjb/src/public/emacs/pjb-termbin-test.el"
          "/Users/pjb/src/public/emacs/pjb-thi-test.el"
          "/Users/pjb/src/public/emacs/pjb-tla-test.el"
          "/Users/pjb/src/public/emacs/pjb-transpose-test.el"
          "/Users/pjb/src/public/emacs/pjb-utilities-test.el"
          "/Users/pjb/src/public/emacs/pjb-vm-test.el"
          "/Users/pjb/src/public/emacs/pjb-vm-kill-file-test.el"
          "/Users/pjb/src/public/emacs/pjb-work-test.el"
          "/Users/pjb/src/public/emacs/pjb-xcode-test.el"
          "/Users/pjb/src/public/emacs/pjb-xml-test.el"
          "/Users/pjb/src/public/emacs/pjb-xresources-test.el"
          "/Users/pjb/src/public/emacs/pjb-zone-test.el"
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
