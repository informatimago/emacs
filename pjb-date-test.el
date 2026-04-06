;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-date-test.el --- Tests for pjb-date
(require 'ert)
(require 'cl) ;; pjb-date.el uses unprefixed `parse-integer' from cl
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-date)

(ert-deftest pjb-date/format-rfc822-date-returns-string ()
  (should (stringp (format-rfc822-date '(0 0)))))

(ert-deftest pjb-date/format-rfc822-date-known-epoch ()
  ;; The epoch 0 should format with year 1970.
  (let ((s (format-rfc822-date '(0 0))))
    (should (string-match-p "1970" s))))

;; Pin reality: parse-iso8601-date in pjb-date.el calls the unprefixed
;; `parse-integer', which the modern `cl' compatibility shim no longer
;; provides — only `cl-parse-integer' exists.  The function therefore
;; signals `void-function' until Phase 2 renames the call site.
(ert-deftest pjb-date/parse-iso8601-date-currently-broken ()
  (should-error (parse-iso8601-date "20150101T000000")
                :type 'void-function))

;;;; pjb-date-test.el                  --                     --          ;;;;
