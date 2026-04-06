;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-state-coding-test.el --- Tests pinning the public API of pjb-state-coding.el

(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
;; pjb-state-coding.el has no (provide ...) form.
(load-file (expand-file-name
            "pjb-state-coding.el"
            (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest pjb-state-coding/bits-for-count-small ()
  (should (= 1 (bits-for-count 0)))
  (should (= 1 (bits-for-count 1)))
  (should (= 2 (bits-for-count 2)))
  (should (= 2 (bits-for-count 3)))
  (should (= 3 (bits-for-count 4)))
  (should (= 3 (bits-for-count 7)))
  (should (= 4 (bits-for-count 8))))

(ert-deftest pjb-state-coding/make-mask-with-set-bits ()
  (should (equal '(2 1 0) (make-mask-with-set-bits 3)))
  (should (equal '(0)     (make-mask-with-set-bits 1)))
  (should (equal nil      (make-mask-with-set-bits 0))))

(ert-deftest pjb-state-coding/make-mask-empty ()
  (should (equal nil (make-mask)))
  (should (equal nil (make-mask nil))))

(ert-deftest pjb-state-coding/make-mask-from-integer ()
  (should (equal '(2 0)     (make-mask 5)))
  (should (equal nil        (make-mask 0)))
  (should (equal '(0)       (make-mask 1))))

(ert-deftest pjb-state-coding/integer-to-mask ()
  (should (equal '(2 0)     (integer-to-mask 5)))
  (should (equal nil        (integer-to-mask 0)))
  (should (equal '(3 2 1 0) (integer-to-mask 15))))

(ert-deftest pjb-state-coding/mask-to-integer ()
  (should (= 0  (mask-to-integer nil)))
  (should (= 5  (mask-to-integer '(2 0))))
  (should (= 15 (mask-to-integer '(3 2 1 0)))))

(ert-deftest pjb-state-coding/mask-high-bit ()
  (should (= 2 (mask-high-bit '(2 0))))
  (should (eq nil (mask-high-bit nil))))

(ert-deftest pjb-state-coding/mask-set-and-clear-bit ()
  (should (equal '(2 1 0) (mask-set-bit '(2 0) 1)))
  ;; setting an already-set bit returns the mask unchanged
  (should (equal '(2 0)   (mask-set-bit '(2 0) 2)))
  (should (equal '(2 0)   (mask-clear-bit '(2 1 0) 1))))

(ert-deftest pjb-state-coding/mask-or ()
  (should (equal '(2 1 0) (mask-or '(2 0) '(1))))
  (should (equal '(2 0)   (mask-or '(2 0) '(2 0)))))

(ert-deftest pjb-state-coding/mask-and ()
  (should (equal '(2 0) (mask-and '(2 1 0) '(2 0))))
  (should (equal nil    (mask-and '(2 0) '(1)))))

(ert-deftest pjb-state-coding/mask-shift ()
  (should (equal '(5 3) (mask-shift '(2 0) 3)))
  (should (equal '(1 -1) (mask-shift '(2 0) -1))))

(ert-deftest pjb-state-coding/remove-element ()
  (should (equal '(1 3)   (remove-element '(1 2 3) 2)))
  (should (equal '(1 2 3) (remove-element '(1 2 3) 99)))
  (should (equal nil      (remove-element nil 1))))

(ert-deftest pjb-state-coding/int-to-hex ()
  (should (equal "0" (int-to-hex 0)))
  (should (equal "f" (int-to-hex 15)))
  (should (equal "ff" (int-to-hex 255))))

(ert-deftest pjb-state-coding/mask-to-hex ()
  (should (equal "0x00000005" (mask-to-hex '(2 0))))
  (should (equal "0x00000000" (mask-to-hex nil))))

(ert-deftest pjb-state-coding/substates-and-abstract ()
  (should (equal '(("A") ("B"))
                 (substates '("X" :abstract ("A") ("B")))))
  (should (state-is-abstract '("X" :abstract ("A"))))
  (should-not (state-is-abstract '("X" ("A")))))

;;; pjb-state-coding-test.el ends here
