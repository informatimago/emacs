;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-pmatch-test.el --- Tests pinning the public API of pjb-pmatch.el

(require 'ert)
(require 'cl) ;; pjb-pmatch.el uses unprefixed `do' / `loop' / `assert' / `equalp'
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; pjb-pmatch.el runs its own home-grown self-test suite at top level,
;; using `handler-case' from full Common Lisp.  `handler-case' is not
;; in modern Emacs `cl' or `cl-lib', so loading the file aborts with
;; (void-function handler-case).  Phase 2 should either replace the
;; self-tests with ERT (we now have an ERT framework) and remove
;; `handler-case', or guard the self-test block with `noninteractive'.
(defvar pjb-pmatch-test--load-error
  (condition-case err (require 'pjb-pmatch) (error err)))

(ert-deftest pjb-pmatch/load-currently-fails ()
  "Pin the current load failure so the Phase 2 fix is detected."
  (should pjb-pmatch-test--load-error))

(ert-deftest pjb-pmatch/api-when-loaded ()
  (skip-unless (null pjb-pmatch-test--load-error))
  (should (functionp 'make-match-state))
  (should (functionp 'match-state-failed-p))
  (should (functionp 'match)))

;;; pjb-pmatch-test.el ends here
