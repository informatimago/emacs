;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-date-test.el --- Tests for pjb-date
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-date)

(ert-deftest pjb-date/format-rfc822-date-returns-string ()
  (should (stringp (format-rfc822-date '(0 0)))))

(ert-deftest pjb-date/format-rfc822-date-known-epoch ()
  ;; The epoch 0 should format with year 1970.
  (let ((s (format-rfc822-date '(0 0))))
    (should (string-match-p "1970" s))))

(ert-deftest pjb-date/parse-iso8601-date-returns-time ()
  (let ((time (parse-iso8601-date "20150101T000000")))
    (should (consp time))))

(ert-deftest pjb-date/parse-iso8601-roundtrip-year ()
  (let* ((time (parse-iso8601-date "20150615T120000"))
         (decoded (decode-time time)))
    (should (= 2015 (nth 5 decoded)))
    (should (= 6 (nth 4 decoded)))
    (should (= 15 (nth 3 decoded)))))

(ert-deftest pjb-date/parse-iso8601-hms ()
  (let* ((time (parse-iso8601-date "20200101T133045"))
         (decoded (decode-time time)))
    (should (= 13 (nth 2 decoded)))
    (should (= 30 (nth 1 decoded)))
    (should (= 45 (nth 0 decoded)))))

;;;; pjb-date-test.el                  --                     --          ;;;;
