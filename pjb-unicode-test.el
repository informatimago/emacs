;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-unicode-test.el --- Tests for pjb-unicode
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-unicode)

(ert-deftest pjb-unicode-test/get-map-known ()
  (let ((m (pjb-unicode-get-map 'mathematical-monospace)))
    (should (listp m))
    (should (= 3 (length m)))))

(ert-deftest pjb-unicode-test/get-map-unknown ()
  (should (null (pjb-unicode-get-map 'no-such-map))))

(ert-deftest pjb-unicode-test/map-character-ranges-digits ()
  (let* ((ranges (pjb-unicode-get-map 'mathematical-monospace))
         (out (pjb-unicode-map-character-ranges "0" ranges)))
    (should (= 1 (length out)))
    (should (= (+ ?0 #x1d7c6) (aref out 0)))))

(ert-deftest pjb-unicode-test/map-character-ranges-uppercase ()
  (let* ((ranges (pjb-unicode-get-map 'mathematical-monospace))
         (out (pjb-unicode-map-character-ranges "A" ranges)))
    (should (= (+ ?A #x1d62f) (aref out 0)))))

(ert-deftest pjb-unicode-test/map-character-ranges-lowercase ()
  (let* ((ranges (pjb-unicode-get-map 'mathematical-monospace))
         (out (pjb-unicode-map-character-ranges "a" ranges)))
    (should (= (+ ?a #x1d629) (aref out 0)))))

(ert-deftest pjb-unicode-test/map-character-ranges-passthrough ()
  (let* ((ranges (pjb-unicode-get-map 'mathematical-monospace))
         (out (pjb-unicode-map-character-ranges " " ranges)))
    (should (= ?\s (aref out 0)))))

(ert-deftest pjb-unicode-test/map-character-ranges-mixed ()
  (let* ((ranges (pjb-unicode-get-map 'mathematical-monospace))
         (out (pjb-unicode-map-character-ranges "A0a " ranges)))
    (should (= 4 (length out)))
    (should (= ?\s (aref out 3)))))

(ert-deftest pjb-unicode-test/ensure-list-list ()
  (should (equal '(1 2) (ensure-list '(1 2)))))

(ert-deftest pjb-unicode-test/ensure-list-atom ()
  (should (equal '(3) (ensure-list 3))))

(ert-deftest pjb-unicode-test/clock-single ()
  (let ((s (pjb-unicode-clock 3)))
    (should (= 1 (length s)))))

(ert-deftest pjb-unicode-test/clock-list ()
  (let ((s (pjb-unicode-clock '(1 2 3))))
    (should (= 3 (length s)))))

(ert-deftest pjb-unicode-test/clock-number ()
  (let ((s (pjb-unicode-clock-number 12)))
    (should (= 2 (length s)))))

(provide 'pjb-unicode-test)
