;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;***
;;;;FILE:               pjb-utilities-test.el
;;;;DESCRIPTION
;;;;    Unit tests for pjb-utilities.el
;;;;LEGAL
;;;;    LGPL
;;;;    Copyright Pascal Bourguignon
;;;;***
(require 'pjb-utilities)
(require 'ert)

(defgroup pjb-utilities-tests nil
  "Test suite for pjb-utilities")

;;;; pjb-or tests
(ert-deftest test/pjb-utilities--pjb-or-first-true ()
  "Test pjb-or returns first non-nil argument."
  (assert (equal (pjb-or nil nil t) t))
  (assert (equal (pjb-or 1 2 3) 1))
  (assert (equal (pjb-or nil nil 42) 42))
  :success)

(ert-deftest test/pjb-utilities--pjb-or-all-nil ()
  "Test pjb-or returns nil when all args are nil."
  (assert (eq (pjb-or) nil))
  (assert (eq (pjb-or nil) nil))
  (assert (eq (pjb-or nil nil) nil))
  :success)

(ert-deftest test/pjb-utilities--pjb-or-symbol ()
  "Test pjb-or with symbols."
  (assert (eq (pjb-or nil nil 'foo) 'foo))
  (assert (eq (pjb-or 'bar) 'bar))
  :success)

(ert-deftest test/pjb-utilities--recursive-apply-basic ()
  "Test recursive-apply with simple flat lists."
  (assert (equal (recursive-apply '+ (list 1 2 3) (list 4 5 6)) (list 5 7 9)))
  (assert (equal (recursive-apply '- (list 10 20) (list 3 5)) (list 7 15)))
  :success)

(ert-deftest test/pjb-utilities--recursive-apply-nested ()
  "Test recursive-apply with nested lists."
  (assert (equal (recursive-apply '+ (list (list 1 2) (list 3 4)) 
                                      (list (list 1 1) (list 2 2)))
                (list (list 2 3) (list 5 6))))
  :success)

(ert-deftest test/pjb-utilities--recursive-apply-empty ()
  "Test recursive-apply with empty lists."
  (assert (equal (recursive-apply '+ nil nil) nil))
  (assert (equal (recursive-apply '+ (list 1) (list 2)) (list 3)))
  :success)

(ert-deftest test/pjb-utilities--recursive-apply-nested-mixed-depth ()
  "Test recursive-apply with nested lists of different depths."
  (assert (equal (recursive-apply '* (list 2 (list 3 4)) (list 3 (list 2 5)))
                (list 6 (list 6 20))))
  :success)

(ert-deftest test/pjb-utilities--padd-basic ()
  "Test padd performs element-wise addition."
  (assert (equal (padd (list 1 2 3) (list 4 5 6)) (list 5 7 9)))
  (assert (equal (padd (list 10 20) (list 3 5)) (list 13 25)))
  :success)

(ert-deftest test/pjb-utilities--psub-basic ()
  "Test psub performs element-wise subtraction."
  (assert (equal (psub (list 10 20 30) (list 1 2 3)) (list 9 18 27)))
  (assert (equal (psub (list 100) (list 50)) (list 50)))
  :success)

(ert-deftest test/pjb-utilities--pmul-basic ()
  "Test pmul performs element-wise multiplication."
  (assert (equal (pmul (list 2 3 4) (list 5 6 7)) (list 10 18 28)))
  (assert (equal (pmul (list 10) (list 3)) (list 30)))
  :success)

(ert-deftest test/pjb-utilities--pdiv-basic ()
  "Test pdiv performs element-wise division."
  (assert (equal (pdiv (list 10 20 30) (list 2 5 10)) (list 5 4 3)))
  (assert (equal (pdiv (list 100 200) (list 25 50)) (list 4 4)))
  :success)

(ert-deftest test/pjb-utilities--pmod-basic ()
  "Test pmod performs element-wise modulo."
  (assert (equal (pmod (list 10 15 20) (list 3 5 7)) (list 1 0 6)))
  (assert (equal (pmod (list 7 14) (list 3 4)) (list 1 2)))
  :success)

(ert-deftest test/pjb-utilities--recursive-apply-lambda ()
  "Test recursive-apply with custom lambda function."
  (assert (equal (recursive-apply (lambda (a b) (cons a b)) 
                                    (list 1 2 3)
                                    (list 10 20 30))
                (list '(1 . 10) '(2 . 20) '(3 . 30))))
  :success)

(provide 'pjb-utilities-tests)
;;;; THE END ;;;;
