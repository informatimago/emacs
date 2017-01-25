;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-vm.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    vm stuff -- obsolete.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-02-24 <PJB> Extracted from ~/rc/emacs-common.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(when (require 'vm nil t)
  (.EMACS "vm")
  (require 'vm-vars)
  (ignore-errors (load-library "vm-w3m"))



  ;; (add-hook 'vm-mode-hook              'mc-install-read-mode)
  ;; (add-hook 'vm-summary-mode-hook      'mc-install-read-mode)
  ;; (add-hook 'vm-virtual-mode-hook      'mc-install-read-mode)
  ;; (add-hook 'vm-mail-mode-hook         'mc-install-write-mode)
  ;; (add-hook 'vm-presentation-mode-hook 'mc-install-write-mode)
  (defun pjb-vm-summary-meat         () (when nil (set-frame-name "MAIL")))
  (defun pjb-vm-mail-meat            () (when nil (set-frame-name "COMPOSE")))
  (defun pjb-vm-reply-meat           () (inactivate-input-method))
  (defun pjb-vm-arrived-message-meat () (pjb-vm-kill-subject-regexp "\\[SPAM\\]"))
  (add-hook 'vm-summary-mode-hook    'pjb-vm-summary-meat)
  (add-hook 'vm-mail-mode-hook       'pjb-vm-mail-meat)
  (add-hook 'vm-reply-hook           'pjb-vm-reply-meat)
  (add-hook 'vm-arrived-message-hook 'pjb-vm-arrived-message-meat)


  (defun pjb-vm-delete-spam (count)
    (interactive "p")
    (vm-save-message "~/mail/spam.mbox" count)
    (pjb-vm-delete-message count))

  (defun pjb-vm-delete-message (count)
    (interactive "p")
    (vm-delete-message count)
    (vm-next-message))

  (defun pjb-vm-visit-folder-meat ()
    (define-key vm-mode-map (kbd "d")     'pjb-vm-delete-message)
    (define-key vm-mode-map (kbd "M-d")   'pjb-vm-delete-spam)
    (define-key vm-mode-map (kbd "o")     'vm-save-message)
    (define-key vm-mode-map (kbd "r")     'vm-followup-include-text)
    (define-key vm-mode-map (kbd "s")     'vm-save-folder)
    (local-set-key          (kbd "c")     'vm-save-message))

  (add-hook 'vm-visit-folder-hook 'pjb-vm-visit-folder-meat)

  (unless (<= 23 emacs-major-version)
    (keyboard-translate (aref (kbd "M-S-d") 0) (aref (kbd "M-S-d") 0))
    (keyboard-translate (aref (kbd "M-D")   0) (aref (kbd "M-D")   0)))

  ;; (defun vm-from-biff ()
  ;;   (interactive)
  ;;   (select-frame (make-frame))
  ;;   (vm-register-frame (vm-selected-frame))
  ;;   (when vm-warp-mouse-to-new-frame
  ;;     (vm-warp-mouse-to-frame-maybe (vm-selected-frame)))
  ;;   (vm))

  (when (load "vm-sort" *pjb-load-noerror* *pjb-load-silent*)
    (defun vm-sort-compare-author (m1 m2)
      "Let's sort by domain first"
      (let ((s1 (vm-su-from m1))
            (s2 (vm-su-from m2))
            l1 d1 l2 d2)
        (let ((@-pos (position (character "@") s1)))
          (if @-pos
              (setf d1 (subseq s1 (1+ @-pos))
                    l1 (subseq s1 0 @-pos))
              (setf d1 ""
                    l1 s1)))
        (let ((@-pos (position (character "@") s2)))
          (if @-pos
              (setf d2 (subseq s2 (1+ @-pos))
                    l2 (subseq s2 0 @-pos))
              (setf d2 ""
                    l2 s2)))
        (cond ((string-equal s1 s2) '=)
              ((string-equal d1 d2)
               (cond ((string-lessp l1 l2) t)
                     ((string-equal l1 l2)
                      (let ((f1 (vm-su-full-name m1))
                            (f2 (vm-su-full-name m2)))
                        (cond ((string-lessp f1 f2) t)
                              ((string-lessp f1 f2) '=)
                              (t nil))))
                     (t nil)))
              ((string-lessp d1 d2) t)
              (t nil))))
    ) ;;when vm-sort



  ;; (catch :found
  ;;   (let ((version emacs-version)
  ;;         (next
  ;;          (lambda ()
  ;;            (cond
  ;;              ((null version)          (throw :found :default))
  ;;              ((= 0 (length version))
  ;;               (setf version nil)
  ;;               (concatenate 'string (NAMESTRING (USER-HOMEDIR-PATHNAME))
  ;;                            "bin/movemail"))
  ;;              (t (prog1
  ;;                     (format "/usr/local/libexec/emacs/%s/%s/movemail"
  ;;                       version system-configuration)
  ;;                   (string-match "^\\(\\([0-9][.0-9]*\\)\\.\\)?[0-9]+$" version)
  ;;                   (setq version (or (match-string 2 version) ""))))))))
  ;;     (do ((path (funcall next) (funcall next)))
  ;;         (nil)
  ;;       (when (file-exists-p path)
  ;;         (setq vm-movemail-program  path)
  ;;         (throw :found :one)))))

  ;; ;; movemail: No locks available for /larissa//var/spool/mail/pjb
  ;; ;; /usr/local/libexec/emacs/21.3/i686-pc-linux-gnu/movemail exited with code 1
  ;; (setq vm-movemail-program
  ;;       (concatenate 'string  (NAMESTRING (USER-HOMEDIR-PATHNAME)) "bin/movemail"))

;;; '(vm-imap-server-list (quote ("imap:imap.afaa.asso.fr:143:inbox:login:pjb:pari-fle")))



  ;; rmail -> vm
  ;;(defalias 'rmail 'vm)
  ;;(defalias 'rmail-input 'vm-visit-folder)
  ;;(defun rmail       () (interactive) (error "Use mail in a shell!"))
  ;;(defun vm          () (interactive) (error "Use mail in a shell!"))
  ;;(defun rmail-input () (interactive) (error "Use mail in a shell!"))

  ;; (defmacro advise-replace (fname parameters body)
  ;;   (let ((aname (intern (format "pjb-adrep-%s" fname))))
  ;;     `(progn
  ;;        (defadvice ,fname
  ;;            (around ,aname  first  ,parameters  activate)
  ;;          ,body)
  ;;        (ad-activate (quote ,fname)))
  ;;     )) ;;advise-replace
  ;; (put 'advise-replace      'lisp-indent-function 2)
  ;;
  ;;
  ;; (advise-replace rmail-sort-by-correspondent (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-author" "author")))
  ;;
  ;; (advise-replace rmail-sort-by-date          (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-date" "date")))
  ;;
  ;; (advise-replace rmail-sort-by-labels        (reverse)
  ;;   (error "Not implemented with VM."))
  ;;
  ;; (advise-replace rmail-sort-by-lines         (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-line-count" "line-count")))
  ;;
  ;; (advise-replace rmail-sort-by-recipient     (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-recipients" "recipients")))
  ;;
  ;; (advise-replace rmail-sort-by-subject       (reverse)
  ;;   (vm-sort-messages  (if reverse "reversed-subject" "subject")))


  ;; (defadvice vm-mime-attach-object
  ;;     (before pjb-removemime-vm-mime-attach-object nil activate)
  ;;   (save-restriction
  ;;     (pjb-mail-narrow-to-headers)
  ;;     (pjb-mail-remove-header "^\\(MIME-Version:\\|Content-\\)" t))
  ;;   )
  ;; (ad-activate 'vm-mime-attach-object)



  (when (require 'vm-pop nil t)
    (defun vm-pop-cleanup-region (start end)
      (setq end (vm-marker end))
      (save-excursion
        (goto-char start)
        ;; CRLF -> LF
        (while (and (< (point) end) (search-forward "\r\n"  end t))
          (replace-match "\n" t t))
        (goto-char start)
        (while (and (< (point) end) (search-forward "^\\(From .*\\)" end t))
          (message "inserting a new line before %S" (buffer-substring (match-beginning 0) (match-end 0)))
          (goto-char (match-beginning 0))
          (insert "\n\n")
          (forward-line))
        ;; (goto-char start)
        ;; chop leading dots
        ;; (while (and (< (point) end) (re-search-forward "^\\."  end t))
        ;;   (replace-match "" t t)
        ;;   (forward-char))
        )
      (set-marker end nil)))


  (defun vm (&optional folder read-only access-method)
    "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, message additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox normally causes any contents of the system mailbox to
be moved and appended to the resulting buffer.  You can disable this automatic fetching of mail by setting `vm-auto-get-new-mail' to nil.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' saves the buffered folder to disk, but does not expunge
deleted messages.  Use `###' to expunge deleted messages.

See the documentation for vm-mode for more information."
    (interactive (list nil current-prefix-arg))
    (vm-session-initialization)
    ;; set inhibit-local-variables non-nil to protect
    ;; against letter bombs.
    ;; set enable-local-variables to nil for newer Emacses
    (catch 'done
      ;; deduce the access method if none specified
      (if (null access-method)
          (let ((f (or folder vm-primary-inbox)))
            (cond ((and vm-recognize-imap-maildrops
                        ;; f could be a buffer
                        (stringp f)
                        (string-match vm-recognize-imap-maildrops f))
                   (setq access-method 'imap
                         folder f))
                  ((and vm-recognize-pop-maildrops
                        ;; f could be a buffer
                        (stringp f)
                        (string-match vm-recognize-pop-maildrops f))
                   (setq access-method 'pop
                         folder f)))))
      (let ((full-startup (not (bufferp folder)))
            (did-read-index-file nil)
            folder-buffer first-time totals-blurb
            folder-name remote-spec
            preserve-auto-save-file)
        (cond ((eq access-method 'pop)
               (setq remote-spec (vm-pop-find-spec-for-name folder))
               (if (null remote-spec)
                   (error "No such POP folder: %s" folder))
               (setq folder-name folder)
               ;; Prior to VM 7.11, we computed the cache filename
               ;; based on the full POP spec including the password
               ;; if it was in the spec.  This meant that every
               ;; time the user changed his password, we'd start
               ;; visiting the wrong (and probably nonexistent)
               ;; cache file.
               ;;
               ;; To fix this we do two things.  First, migrate the
               ;; user's caches to the filenames based in the POP
               ;; sepc without the password.  Second, we visit the
               ;; old password based filename if it still exists
               ;; after trying to migrate it.
               ;;
               ;; For VM 7.16 we apply the same logic to the access
               ;; methods, pop, pop-ssh and pop-ssl and to
               ;; authentication method and service port, which can
               ;; also change and lead us to visit a nonexistent
               ;; cache file.  The assumption is that these
               ;; properties of the connection can change and we'll
               ;; still be accessing the same mailbox on the
               ;; server.
               (let ((f-pass (vm-pop-make-filename-for-spec remote-spec))
                     (f-nopass (vm-pop-make-filename-for-spec remote-spec t))
                     (f-nospec (vm-pop-make-filename-for-spec remote-spec t t)))
                 (cond ((or (string= f-pass f-nospec)
                            (file-exists-p f-nospec))
                        nil )
                       ((file-exists-p f-pass)
                        ;; try to migrate
                        (condition-case nil
                            (rename-file f-pass f-nospec)
                          (error nil)))
                       ((file-exists-p f-nopass)
                        ;; try to migrate
                        (condition-case nil
                            (rename-file f-nopass f-nospec)
                          (error nil))))
                 ;; choose the one that exists, password version,
                 ;; nopass version and finally nopass+nospec
                 ;; version.
                 (cond ((file-exists-p f-pass)
                        (setq folder f-pass))
                       ((file-exists-p f-nopass)
                        (setq folder f-nopass))
                       (t
                        (setq folder f-nospec)))))
              ((eq access-method 'imap)
               (setq remote-spec folder
                     folder-name (or (nth 3 (vm-imap-parse-spec-to-list
                                             remote-spec))
                                     folder)
                     folder (vm-imap-make-filename-for-spec remote-spec))))
        (setq folder-buffer
              (if (bufferp folder)
                  folder
                  (let ((file (or folder (expand-file-name vm-primary-inbox
                                                           vm-folder-directory))))
                    (if (file-directory-p file)
                        ;; MH code perhaps... ?
                        (error "%s is a directory" file)
                        (or (vm-get-file-buffer file)
                            (let ((default-directory
                                   (or (and vm-folder-directory
                                            (expand-file-name vm-folder-directory))
                                       default-directory))
                                  (inhibit-local-variables t)
                                  (enable-local-variables nil)
                                  (enable-local-eval nil)
                                  ;; for Emacs/MULE
                                  (default-enable-multibyte-characters nil)
                                  ;; for XEmacs/Mule
                                  (coding-system-for-read
                                   (vm-line-ending-coding-system)))
                              (message "Reading %s..." file)
                              (prog1 (find-file-noselect file)
                                ;; update folder history
                                (let ((item (or remote-spec folder
                                                vm-primary-inbox)))
                                  (if (not (equal item (car vm-folder-history)))
                                      (setq vm-folder-history
                                            (cons item vm-folder-history))))
                                (message "Reading %s... done" file))))))))
        (set-buffer folder-buffer)
        (cond ((memq access-method '(pop imap))
               (if (not (equal folder-name (buffer-name)))
                   (rename-buffer folder-name t))))
        (if (and vm-fsfemacs-mule-p enable-multibyte-characters)
            (set-buffer-multibyte nil))
        ;; for MULE
        ;;
        ;; If the file coding system is not a no-conversion variant,
        ;; make it so by encoding all the text, then setting the
        ;; file coding system and decoding it.  This situation is
        ;; only possible if a file is visited and then vm-mode is
        ;; run on it afterwards.
        ;;
        ;; There are separate code blocks for FSF Emacs and XEmacs
        ;; because the coding systems have different names.
        (defvar buffer-file-coding-system)
        (if (and (or vm-xemacs-mule-p vm-xemacs-file-coding-p)
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-unix)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-dos)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'no-conversion-mac)))
                 (not (eq (get-coding-system buffer-file-coding-system)
                          (get-coding-system 'binary))))
            (let ((buffer-read-only nil)
                  (omodified (buffer-modified-p)))
              (unwind-protect
                   (progn
                     (encode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system)
                     (set-buffer-file-coding-system 'no-conversion nil)
                     (decode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system))
                (set-buffer-modified-p omodified))))
        (if (and vm-fsfemacs-mule-p (null buffer-file-coding-system))
            (set-buffer-file-coding-system 'raw-text nil))
        (if (and vm-fsfemacs-mule-p
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-unix)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-mac)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'raw-text-dos)))
                 (not (eq (coding-system-base buffer-file-coding-system)
                          (coding-system-base 'no-conversion))))
            (let ((buffer-read-only nil)
                  (omodified (buffer-modified-p)))
              (unwind-protect
                   (progn
                     (encode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system)
                     (set-buffer-file-coding-system 'raw-text nil)
                     (decode-coding-region (point-min) (point-max)
                                           buffer-file-coding-system))
                (set-buffer-modified-p omodified))))
        (vm-check-for-killed-summary)
        (vm-check-for-killed-presentation)
        ;; If the buffer's not modified then we know that there can be no
        ;; messages in the folder that are not on disk.
        (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
        (setq first-time (not (eq major-mode 'vm-mode))
              preserve-auto-save-file (and buffer-file-name
                                           (not (buffer-modified-p))
                                           (file-newer-than-file-p
                                            (make-auto-save-file-name)
                                            buffer-file-name)))
        ;; Force the folder to be read only if the auto
        ;; save file contains information the user might not
        ;; want overwritten, i.e. recover-file might be
        ;; desired.  What we want to avoid is an auto-save.
        ;; Making the folder read only will keep
        ;; subsequent actions from modifying the buffer in a
        ;; way that triggers an auto save.
        ;;
        ;; Also force the folder read-only if it was read only and
        ;; not already in vm-mode, since there's probably a good
        ;; reason for this.
        (setq vm-folder-read-only (or preserve-auto-save-file read-only
                                      (default-value 'vm-folder-read-only)
                                      (and first-time buffer-read-only)))
        ;; If this is not a VM mode buffer then some initialization
        ;; needs to be done
        (if first-time
            (progn
              (buffer-disable-undo (current-buffer))
              (abbrev-mode 0)
              (auto-fill-mode 0)
              ;; If an 8-bit message arrives undeclared the 8-bit
              ;; characters in it should be displayed using the
              ;; user's default face charset, rather than as octal
              ;; escapes.
              (vm-fsfemacs-nonmule-display-8bit-chars)
              (vm-mode-internal access-method)
              (cond ((eq access-method 'pop)
                     (vm-set-folder-pop-maildrop-spec remote-spec))
                    ((eq access-method 'imap)
                     (vm-set-folder-imap-maildrop-spec remote-spec)))
              ;; If the buffer is modified we don't know if the
              ;; folder format has been changed to be different
              ;; from index file, so don't read the index file in
              ;; that case.
              (if (not (buffer-modified-p))
                  (setq did-read-index-file (vm-read-index-file-maybe)))))

        ;; builds message list, reads attributes if they weren't
        ;; read from an index file.
        (vm-assimilate-new-messages nil (not did-read-index-file) nil t)

        (if (and first-time (not did-read-index-file))
            (progn
              (vm-gobble-visible-header-variables)
              (vm-gobble-bookmark)
              (vm-gobble-pop-retrieved)
              (vm-gobble-imap-retrieved)
              (vm-gobble-summary)
              (vm-gobble-labels)))

        (if first-time
            (vm-start-itimers-if-needed))

        ;; make a new frame if the user wants one.  reuse an
        ;; existing frame that is showing this folder.
        (if (and full-startup
                 ;; this so that "emacs -f vm" doesn't create a frame.
                 this-command)
            (apply 'vm-goto-new-folder-frame-maybe
                   (if folder '(folder) '(primary-folder folder))))

        ;; raise frame if requested and apply startup window
        ;; configuration.
        (if full-startup
            (let ((buffer-to-display (or vm-summary-buffer
                                         vm-presentation-buffer
                                         (current-buffer))))
              (vm-display buffer-to-display buffer-to-display
                          (list this-command)
                          (list (or this-command 'vm) 'startup))
              (if vm-raise-frame-at-startup
                  (vm-raise-frame))))

        ;; say this NOW, before the non-previewers read a message,
        ;; alter the new message count and confuse themselves.
        (if full-startup
            (progn
              ;; save blurb so we can repeat it later as necessary.
              (set-buffer folder-buffer)
              (setq totals-blurb (vm-emit-totals-blurb))
              (and buffer-file-name
                   (vm-store-folder-totals buffer-file-name (cdr vm-totals)))))

        (vm-thoughtfully-select-message)
        (vm-update-summary-and-mode-line)
        ;; need to do this after any frame creation because the
        ;; toolbar sets frame-specific height and width specifiers.
        (vm-toolbar-install-or-uninstall-toolbar)

        (and vm-use-menus (vm-menu-support-possible-p)
             (vm-menu-install-visited-folders-menu))

        (if full-startup
            (progn
              (if (and (vm-should-generate-summary)
                       ;; don't generate a summary if recover-file is
                       ;; likely to happen, since recover-file does
                       ;; not work in a summary buffer.
                       (not preserve-auto-save-file))
                  (vm-summarize t nil))
              ;; raise the summary frame if the user wants frames
              ;; raised and if there is a summary frame.
              (if (and vm-summary-buffer
                       vm-mutable-frames
                       vm-frame-per-summary
                       vm-raise-frame-at-startup)
                  (vm-raise-frame))
              ;; if vm-mutable-windows is nil, the startup
              ;; configuration can't be applied, so do
              ;; something to get a VM buffer on the screen
              (if vm-mutable-windows
                  (vm-display nil nil (list this-command)
                              (list (or this-command 'vm) 'startup))
                  (save-excursion
                    (switch-to-buffer (or vm-summary-buffer
                                          vm-presentation-buffer
                                          (current-buffer)))))))

        (if vm-message-list
            ;; don't decode MIME if recover-file is
            ;; likely to happen, since recover-file does
            ;; not work in a presentation buffer.
            (let ((vm-auto-decode-mime-messages
                   (and vm-auto-decode-mime-messages
                        (not preserve-auto-save-file))))
              (vm-preview-current-message)))

        (run-hooks 'vm-visit-folder-hook)

        ;; Warn user about auto save file, if appropriate.
        (if (and full-startup preserve-auto-save-file)
            (message
             (substitute-command-keys
              "Auto save file is newer; consider \\[recover-file].  FOLDER IS READ ONLY.")))
        ;; if we're not doing a full startup or if doing more would
        ;; trash the auto save file that we need to preserve,
        ;; stop here.
        (if (or (not full-startup) preserve-auto-save-file)
            (throw 'done t))

        (if full-startup
            (message totals-blurb))

        (if (and vm-auto-get-new-mail
                 (not vm-block-new-mail)
                 (not vm-folder-read-only))
            (progn
              (message "Checking for new mail for %s..."
                       (or buffer-file-name (buffer-name)))
              (if (vm-get-spooled-mail t)
                  (progn
                    (setq totals-blurb (vm-emit-totals-blurb))
                    (if (vm-thoughtfully-select-message)
                        (vm-preview-current-message)
                        (vm-update-summary-and-mode-line))))
              (message totals-blurb)))

        ;; Display copyright and copying info.
        (if (and (interactive-p) (not vm-startup-message-displayed))
            (progn
              (vm-display-startup-message)
              (if (not (input-pending-p))
                  (message totals-blurb)))))))

  (provide 'pjb-vm)) ;;when
