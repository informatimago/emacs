;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-queue-test.el --- Tests pinning the public API of pjb-queue.el

(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-queue)

(ert-deftest pjb-queue/new-is-queue ()
  (should (pjb-queue-p (pjb-queue-new))))

(ert-deftest pjb-queue/new-has-zero-length ()
  (should (= 0 (pjb-queue-length (pjb-queue-new)))))

(ert-deftest pjb-queue/enqueue-one-length ()
  (let ((q (pjb-queue-new)))
    (pjb-queue-enqueue q 'a)
    (should (= 1 (pjb-queue-length q)))))

(ert-deftest pjb-queue/enqueue-one-first-and-last ()
  (let ((q (pjb-queue-new)))
    (pjb-queue-enqueue q 'a)
    (should (eq 'a (pjb-queue-first-element q)))
    (should (eq 'a (pjb-queue-last-element q)))))

(ert-deftest pjb-queue/enqueue-many-fifo-order ()
  (let ((q (pjb-queue-new)))
    (pjb-queue-enqueue q 'a)
    (pjb-queue-enqueue q 'b)
    (pjb-queue-enqueue q 'c)
    (should (= 3 (pjb-queue-length q)))
    (should (eq 'a (pjb-queue-first-element q)))
    (should (eq 'c (pjb-queue-last-element q)))
    (should (pjb-queue-p q))))

(ert-deftest pjb-queue/dequeue-returns-first ()
  (let ((q (pjb-queue-new)))
    (pjb-queue-enqueue q 'a)
    (pjb-queue-enqueue q 'b)
    (should (eq 'a (pjb-queue-dequeue q)))
    (should (= 1 (pjb-queue-length q)))
    (should (eq 'b (pjb-queue-first-element q)))))

(ert-deftest pjb-queue/dequeue-all ()
  (let ((q (pjb-queue-new)))
    (pjb-queue-enqueue q 1)
    (pjb-queue-enqueue q 2)
    (pjb-queue-enqueue q 3)
    (should (equal 1 (pjb-queue-dequeue q)))
    (should (equal 2 (pjb-queue-dequeue q)))
    (should (equal 3 (pjb-queue-dequeue q)))
    (should (= 0 (pjb-queue-length q)))
    (should (pjb-queue-p q))))

(ert-deftest pjb-queue/requeue-empty ()
  (let ((q (pjb-queue-new)))
    (pjb-queue-requeue q 'x)
    (should (= 1 (pjb-queue-length q)))
    (should (eq 'x (pjb-queue-first-element q)))
    (should (eq 'x (pjb-queue-last-element q)))))

(ert-deftest pjb-queue/requeue-prepends ()
  (let ((q (pjb-queue-new)))
    (pjb-queue-enqueue q 'a)
    (pjb-queue-enqueue q 'b)
    (pjb-queue-requeue q 'z)
    (should (= 3 (pjb-queue-length q)))
    (should (eq 'z (pjb-queue-first-element q)))
    (should (eq 'b (pjb-queue-last-element q)))))

(ert-deftest pjb-queue/enqueue-returns-queue ()
  (let ((q (pjb-queue-new)))
    (should (eq q (pjb-queue-enqueue q 'a)))))

(ert-deftest pjb-queue/requeue-returns-queue ()
  (let ((q (pjb-queue-new)))
    (should (eq q (pjb-queue-requeue q 'a)))))

(ert-deftest pjb-queue/predicate-rejects-non-queue ()
  (should-not (pjb-queue-p nil))
  (should-not (pjb-queue-p '(not-a-queue)))
  (should-not (pjb-queue-p 42)))

(provide 'pjb-queue-test)
;;; pjb-queue-test.el ends here
