;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-emacs-patches.el
;;;;DESCRIPTION
;;;;
;;;;    Phase 2 (MODERNIZATION.md) emptied this file.  It used to
;;;;    contain three core-function redefinitions, all of which are
;;;;    now dead or supersedable:
;;;;
;;;;      * `called-interactively-p' shim for emacs < 22  -- floor is 27.1.
;;;;      * `completion--twq-all' redefinition pinned to emacs 24.3.1.
;;;;      * `comment-region-internal' redefinition that forced
;;;;        `no-empty=nil' so empty lines also got commented.  The same
;;;;        effect is now obtained with the supported customisation
;;;;        `(setq-default comment-empty-lines t)' (added below).
;;;;
;;;;    The file is no longer on EMACS_SOURCES / pjb-loader's load list,
;;;;    but a stub remains so that any third-party `(require 'pjb-emacs-patches)'
;;;;    still succeeds.
;;;;LEGAL
;;;;    AGPL3
;;;;**************************************************************************

(setq-default comment-empty-lines t)

(provide 'pjb-emacs-patches)
;;;; THE END ;;;;
