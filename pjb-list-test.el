;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-list-test.el --- Tests for pjb-list
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-list)

(ert-deftest pjb-list/ensure-list-list ()
  (should (equal '(1 2 3) (ensure-list '(1 2 3)))))

(ert-deftest pjb-list/ensure-list-atom ()
  (should (equal '(42) (ensure-list 42))))

(ert-deftest pjb-list/iota-default ()
  (should (equal '(0 1 2 3 4) (iota 5))))

(ert-deftest pjb-list/iota-with-start-step ()
  (should (equal '(10 12 14 16) (iota 4 10 2))))

(ert-deftest pjb-list/iota-zero ()
  (should (null (iota 0))))

(ert-deftest pjb-list/flatten ()
  (should (equal '(1 2 3 4 5) (flatten '(1 (2 (3 4)) 5))))
  (should (null (flatten nil))))

(ert-deftest pjb-list/maptree ()
  (should (equal '(2 (4 6) 8)
                 (maptree (lambda (x) (* 2 x)) '(1 (2 3) 4)))))

(ert-deftest pjb-list/depth ()
  (should (= 0 (depth 'atom)))
  (should (= 1 (depth '(a b c))))
  (should (= 3 (depth '(a (b (c)))))))

(ert-deftest pjb-list/list-insert-separator ()
  (should (equal '(a x b x c) (list-insert-separator '(a b c) 'x)))
  (should (null (list-insert-separator nil 'x))))

(ert-deftest pjb-list/assoc-val ()
  (should (equal 2 (assoc-val 'b '((a . 1) (b . 2) (c . 3)))))
  (should (null (assoc-val 'z '((a . 1))))))

(ert-deftest pjb-list/list-remove-elements ()
  (should (equal '(b d f)
                 (list-remove-elements '(a b c d e f) '(a c e)))))

(ert-deftest pjb-list/alist->plist ()
  (should (equal '(a 1 b 2) (alist->plist '((a . 1) (b . 2))))))

(ert-deftest pjb-list/plist->alist ()
  (should (equal '((a . 1) (b . 2)) (plist->alist '(a 1 b 2)))))

(ert-deftest pjb-list/equiv ()
  (should (equiv))
  (should (equiv t t t))
  (should (equiv nil nil))
  (should-not (equiv t nil)))

;;;; pjb-list-test.el                  --                     --          ;;;;
