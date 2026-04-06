;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-pmatch-test.el --- Tests pinning the public API of pjb-pmatch.el

(require 'ert)
(require 'cl) ;; pjb-pmatch.el still uses unprefixed `do' / `loop' / `assert'
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-pmatch)

(ert-deftest pjb-pmatch/match-state-empty ()
  (let ((ms (make-match-state)))
    (should (equal nil (match-state-dict ms)))
    (should-not (match-state-failed-p ms))))

(ert-deftest pjb-pmatch/make-match-state-with-dict ()
  (let ((ms (make-match-state :dict '((x . 1)))))
    (should (equal '((x . 1)) (match-state-dict ms)))
    (should-not (match-state-failed-p ms))))

(ert-deftest pjb-pmatch/match-state-failed-p ()
  (should     (match-state-failed-p '(:failed (:different a b))))
  (should-not (match-state-failed-p '((x . 1)))))

(ert-deftest pjb-pmatch/match-state-retry-strips-failure ()
  (should (equal '((x . 1))
                 (match-state-retry '(:failed (:different a b) (x . 1)))))
  (should (equal '((x . 1))
                 (match-state-retry '((x . 1))))))

(ert-deftest pjb-pmatch/match-anonymous-variable-success ()
  (should-not (match-state-failed-p
               (match '(!av eats an !av) '(John eats an apple)))))

(ert-deftest pjb-pmatch/match-anonymous-variable-rejects-non-symbol ()
  (should (match-state-failed-p
           (match '(!av eats an !av) '(42 eats an apple)))))

(ert-deftest pjb-pmatch/match-literal-mismatch ()
  (should (equal '(:failed (:different an a))
                 (match '(!av eats an !av) '(John eats a banana)))))

(ert-deftest pjb-pmatch/match-anonymous-constant ()
  (should-not (match-state-failed-p
               (match '(I ate !ac kg of apples)
                      '(I ate 42 kg of apples)))))

(ert-deftest pjb-pmatch/match-named-variable-binds ()
  (let ((ms (match '(a (!v x) c) '(a b c))))
    (should-not (match-state-failed-p ms))
    (should (equal 'b (cdr (assoc 'x (match-state-dict ms)))))))

(ert-deftest pjb-pmatch/match-named-constant-binds ()
  (let ((ms (match '(I ate (!c n) kg) '(I ate 42 kg))))
    (should-not (match-state-failed-p ms))
    (should (equal 42 (cdr (assoc 'n (match-state-dict ms)))))))

(ert-deftest pjb-pmatch/match-named-variable-rejects-non-symbol ()
  (should (match-state-failed-p
           (match '(a (!v x) c) '(a 42 c)))))

(ert-deftest pjb-pmatch/collect-variables ()
  (let ((vars (collect-variables '((!v x) (!c y) z))))
    (should (member 'x vars))
    (should (member 'y vars))
    (should-not (member 'z vars))))

;;; pjb-pmatch-test.el ends here
