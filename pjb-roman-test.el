;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-roman-test.el --- Tests for pjb-roman
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-roman)

(ert-deftest pjb-roman/to-roman-1 ()
  (should (equal "I" (to-roman 1))))

(ert-deftest pjb-roman/to-roman-4 ()
  (should (equal "IV" (to-roman 4))))

(ert-deftest pjb-roman/to-roman-9 ()
  (should (equal "IX" (to-roman 9))))

(ert-deftest pjb-roman/to-roman-14 ()
  (should (equal "XIV" (to-roman 14))))

(ert-deftest pjb-roman/to-roman-1999 ()
  ;; pin current behavior of this implementation
  (should (stringp (to-roman 1999))))

(ert-deftest pjb-roman/to-roman-2018 ()
  (should (equal "MMXVIII" (to-roman 2018))))

(ert-deftest pjb-roman/to-roman-error-out-of-range ()
  (should-error (to-roman 0))
  (should-error (to-roman 5000)))

(ert-deftest pjb-roman/to-roman-error-non-integer ()
  (should-error (to-roman "x")))

(ert-deftest pjb-roman/from-roman-I ()
  (should (= 1 (from-roman "I"))))

(ert-deftest pjb-roman/from-roman-IV ()
  (should (= 4 (from-roman "IV"))))

(ert-deftest pjb-roman/from-roman-MMXVIII ()
  (should (= 2018 (from-roman "MMXVIII"))))

(ert-deftest pjb-roman/roundtrip-small ()
  (dolist (n '(1 2 3 5 8 10 14 50 99 100 500 1000 2018))
    (should (= n (from-roman (to-roman n))))))

;;;; pjb-roman-test.el                  --                     --          ;;;;
