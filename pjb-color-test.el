;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-color-test.el --- Tests for pjb-color
(require 'ert)
(load-file (expand-file-name "pjb-color.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest pjb-color-test/48-value-to-name ()
  (should (string= "#000000000000" (color-48-value-to-name '(0 0 0))))
  (should (string= "#ffffffffffff" (color-48-value-to-name '(65535 65535 65535))))
  (should (string= "#111122223333"
                   (color-48-value-to-name '(#x1111 #x2222 #x3333)))))

(ert-deftest pjb-color-test/24-value-to-name ()
  (should (string= "#000000" (color-24-value-to-name '(0 0 0))))
  (should (string= "#ffffff" (color-24-value-to-name '(255 255 255))))
  (should (string= "#112233" (color-24-value-to-name '(#x11 #x22 #x33)))))

(ert-deftest pjb-color-test/24-to-48 ()
  (should (equal '(0 0 0) (color-24-to-48 '(0 0 0))))
  (should (equal '(256 512 768) (color-24-to-48 '(1 2 3))))
  (should (equal '(65280 65280 65280) (color-24-to-48 '(255 255 255)))))

(ert-deftest pjb-color-test/48-to-24 ()
  (should (equal '(0 0 0) (color-48-to-24 '(0 0 0))))
  (should (equal '(1 2 3) (color-48-to-24 '(256 512 768))))
  (should (equal '(255 255 255) (color-48-to-24 '(65535 65535 65535)))))

(ert-deftest pjb-color-test/24-48-roundtrip ()
  (should (equal '(10 20 30) (color-48-to-24 (color-24-to-48 '(10 20 30))))))

(ert-deftest pjb-color-test/lighter-alias ()
  (should (equal (color-48-lighter '(0 0 0))
                 (lighter '(0 0 0)))))

(ert-deftest pjb-color-test/darker-alias ()
  (should (equal (color-48-darker '(65535 65535 65535))
                 (darker '(65535 65535 65535)))))

(ert-deftest pjb-color-test/color-value-to-name-alias ()
  (should (equal (color-48-value-to-name '(0 0 0))
                 (color-value-to-name '(0 0 0)))))

(ert-deftest pjb-color-test/lighter-bad-factor ()
  (should-error (color-48-lighter '(0 0 0) 0.0))
  (should-error (color-48-lighter '(0 0 0) 1.0)))

(ert-deftest pjb-color-test/darker-bad-factor ()
  (should-error (color-48-darker '(65535 65535 65535) 0.0))
  (should-error (color-48-darker '(65535 65535 65535) 1.0)))

(provide 'pjb-color-test)
