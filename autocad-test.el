;;;; -*- mode:emacs-lisp;coding:utf-8; lexical-binding:t -*-

(require 'ert)
(require 'autocad)

(defun autocad-test--face-at (text needle)
  "Return the face at the first occurrence of NEEDLE in TEXT."
  (with-temp-buffer
    (insert text)
    (autocad-lisp-mode)
    (font-lock-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward needle)
    (get-text-property (match-beginning 0) 'face)))

(ert-deftest test/autocad-lisp-mode--fontifies-language-levels ()
  (let ((source "(defun foo (x / y)\n  (setq y (car x))\n  (vlax-get obj Name)\n  (foo y))\n"))
    (should (eq (autocad-test--face-at source "setq")
                'autocad-lisp-special-operator-face))
    (should (eq (autocad-test--face-at source "car")
                'autocad-lisp-autolisp-function-face))
    (should (eq (autocad-test--face-at source "vlax-get")
                'autocad-lisp-visual-lisp-function-face))
    (should (eq (autocad-test--face-at source "foo")
                'autocad-lisp-user-function-face))
    (should (eq (autocad-test--face-at source "x / y")
                'autocad-lisp-variable-face))))

