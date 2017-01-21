;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:                               pjb-pgp.el
;;;;LANGUAGE:                   emacs lisp
;;;;SYSTEM:                     UNIX
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;    This module defines emacs lisp functions to integrate pgp with emacs.
;;;;    It can be configured by a set of constants and a variable.
;;;;      variable pgp-signer    --> Specifies the key used to sign.
;;;;      variable pgp-command   --> Specifies the OpenPGP program to use.
;;;;    The public (interactive) functions are:
;;;;      pgp-encrypt-conventional-region (key)
;;;;      pgp-encrypt-conventional-buffer (key)
;;;;      pgp-decrypt-conventional-region (key)
;;;;      pgp-decrypt-conventional-buffer (key)
;;;;      pgp-sign-region (pass-phrase)
;;;;      pgp-sign-buffer (pass-phrase)
;;;;      pgp-check-sign-region ()
;;;;      pgp-check-sign-buffer ()
;;;;      pgp-sign-encrypt-region (pass-phrase recipients)
;;;;      pgp-sign-encrypt-buffer (pass-phrase recipients)
;;;;      pgp-just-encrypt-region (recipients)
;;;;      pgp-just-encrypt-buffer (recipients)
;;;;      pgp-decrypt-region (pass-phrase)
;;;;      pgp-decrypt-buffer (pass-phrase)
;;;;USAGE
;;;;    See the documentation strings of the items defined here.
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2001-12-20 <PJB> Added support for gpg. Made it the default OpenPGP used.
;;;;    2001-03-19 <PJB> Made it work on read-only buffers,
;;;;                     creating a temporary buffer for output.
;;;;    2001-02-23 <PJB> Creation.
;;;;BUGS
;;;;    * TODO: What if new output buffer has the same name as an existing one?
;;;;    * view-lossage will show the passwords. (recent-keys)
;;;;    * buffer-flush-undo should probably used to prevent undoing an encryption.
;;;;    * Still missing a pgp-wipe.
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2001 - 2011
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;******************************************************************************
(provide 'pgp)
(provide 'pjb-pgp)
(require 'pjb-strings)

(defvar pgp-signer nil
  "When nil, the PGP Default Signing Key is used.
Otherwise it must be a string containing the KeyID or the UserID to be used
to sign." )


(defvar pgp-command 'pgp-pgp-command
  "The name of the function to build pgp commands.
Choose either 'pgp-pgp-command or 'pgp-gpg-command, or implement your own.")

;; (setq pgp-command 'pgp-pgp-command)
;; (setq pgp-command 'pgp-gpg-command)



(defvar pgp-err-buffer-name
  "*PGP*"
  "The name of the OpenPGP result buffer.")

(defvar pgp$*out-buffer* nil "The last output buffer.")




(defun pgp-pgp-signer (signer)
  "PRIVATE"
  (if signer
      (format " -u %s" (shell-quote-argument signer))
    ""))


(defun pgp-pgp-recipients (recipients)
  "PRIVATE"
  (unsplit-string (mapcar 'shell-quote-argument (split-string recipients)) " "))


(defun pgp-pgp-command (selector &rest args)
  "Build the pgp command."
  (cond
   ((eq selector 'list-keys)
    "pgp +batchmode -kv"
    )
   ((eq selector 'add-keys)
    "pgp +batchmode -f"
    )
   ((eq selector 'encrypt-conventional)
    (format "PGPPASS=%s pgp +batchmode -fcat"
            (shell-quote-argument (nth 0 args)));; passphrase
    )
   ((eq selector 'decrypt-conventional)
    (format "PGPPASS=%s pgp +batchmode -f"
            (shell-quote-argument (nth 0 args)));; passphrase
    )
   ((eq selector 'sign)
    (format "PGPPASS=%s pgp +batchmode -fsat %s"
            (shell-quote-argument (nth 0 args));; passphrase
            (pgp-pgp-signer (nth 1 args)));; signer
    )
   ((eq selector 'check-sign)
    "pgp +batchmode -f >/dev/null " ;; stdout gets the signed data.
    )
   ((eq selector 'sign-encrypt)
    (format "PGPPASS=%s pgp +batchmode -fseat %s %s"
            (shell-quote-argument (nth 0 args));; passphrase
            (pgp-pgp-signer (nth 1 args));; signer
            (pgp-pgp-recipients (nth 2 args)));; recipients
    )
   ((eq selector 'encrypt)
    (format "pgp +batchmode -feat %s"
            (pgp-pgp-recipients (nth 0 args)));; recipients
    )
   ((eq selector 'decrypt)
    (format "PGPPASS=%s pgp +batchmode -f"
            (shell-quote-argument (nth 0 args)));; passphrase
    )
   (t (error "Unknown selector '%s'." selector))))




(defun pgp-gpg-signer (signer)
  "PRIVATE"
  (if signer
      (format " --local-user %s" (shell-quote-argument signer))
    ""))


(defun pgp-gpg-recipients (recipients)
  "PRIVATE"
  (apply 'concat
         (mapcar (lambda (elem)
                   (concat " --recipient " (shell-quote-argument elem)))
                 (split-string recipients))))


(defun pgp-gpg-command (selector &rest args)
  "Build the gpg command."
  (let* ((pipe-name (format "/tmp/pipe-%d-%s" (emacs-pid) (gensym "gpg-")))
         (path      (format "PATH=%S" (mapconcat (function identity) exec-path path-separator)))
         (prefix    (format ;; TODO: Here the password is put in args!
                     (if (eq system-type 'darwin)
                         "trap 'rm -f %s' 0 ; mkfifo -m 600 %s ; echo %%s >> %s & "
                         "trap 'rm -f %s' 0 ; mknod %s p ; echo %%s >> %s & ")
                     pipe-name pipe-name pipe-name))
         (gpg       "gpg --no-utf8-strings --batch --no-tty --textmode"))
    (cond
     ((eq selector 'list-keys)
      (concat path " " "gpg --no-utf8-strings --batch --no-tty --list-keys"))
     ((eq selector 'add-keys)
      (concat path " " "gpg --no-utf8-strings --batch --no-tty --import"))
     ((eq selector 'encrypt-conventional)
      (format (concat prefix
                      "%s %s --armor --symmetric "
                      " --passphrase-fd 6 6< %s")
              (shell-quote-argument (nth 0 args));; passphrase
              path gpg pipe-name))
     ((eq selector 'decrypt-conventional)
      (format (concat prefix
                      "%s %s --decrypt "
                      " --passphrase-fd 6 6< %s")
              (shell-quote-argument (nth 0 args));; passphrase
              path gpg pipe-name))
     ((eq selector 'sign)
      (format (concat prefix
                      "%s %s %s --clearsign "
                      " --passphrase-fd 6 6< %s")
              (shell-quote-argument (nth 0 args));; passphrase
              path gpg
              (pgp-gpg-signer (nth 1 args));; signer
              pipe-name))
     ((eq selector 'check-sign)
      (format "%s %s --verify" path gpg))
     ((eq selector 'sign-encrypt)
      (format (concat prefix
                      "%s %s --armor %s  %s "
                      " --sign --encrypt "
                      " --passphrase-fd 6 6< %s")
              (shell-quote-argument (nth 0 args));; passphrase
              path gpg
              (pgp-gpg-signer (nth 1 args));; signer
              (pgp-gpg-recipients (nth 2 args)) ;; recipients
              pipe-name))
     ((eq selector 'encrypt)
      (format "%s %s --armor  %s --encrypt  "
              path gpg
              (pgp-gpg-recipients (nth 0 args)))) ;; recipients
     ((eq selector 'decrypt)
      (format (concat prefix
                      "%s %s --decrypt "
                      " --passphrase-fd 6 6< %s")
              (shell-quote-argument (nth 0 args));; passphrase
              path gpg pipe-name))
     (t (error "Unknown selector '%s'." selector)))))



(defun pgp-prepare-err-buffer ()
  "Prepare the buffer where pgp stderr will be put."
  (save-excursion
    (switch-to-buffer (get-buffer-create pgp-err-buffer-name))
    (erase-buffer)))


(defun pgp-bury-err-buffer-if-empty ()
  "PRIVATE"
  (save-excursion
    (set-buffer  pgp-err-buffer-name)
    (when (= 0 (buffer-size))
      (bury-buffer)
      (switch-to-buffer pgp$*out-buffer*))))


(defun pgp-prepare-out-buffer (action)
  "Prepare the buffer where pgp stdout will be put.
If the current buffer is read-only, the we make a new temporary buffer,
otherwise we keep the current buffer."
  (setq pgp$*out-buffer*
        (if  buffer-read-only
            (get-buffer-create
             (format "*%s %s*" (buffer-name (current-buffer)) action))
          (current-buffer)))
  pgp$*out-buffer*)



;;(let ( (coding-system-for-read utf-8-unix)
;;       (coding-system-for-write iso-latin-1-unix) )


(defun pgp-remove-key-from-history (key)
  "Remove the given KEY from the minibuffer-history."
  (setq minibuffer-history (delete key minibuffer-history)))


(defun pgp-encrypt-conventional-range (key begin end)
  "Encrypt conventionaly the range BEGIN, END with the KEY."
  (pgp-remove-key-from-history key)
  (pgp-prepare-err-buffer)
  (let ((out-buffer (pgp-prepare-out-buffer "Encrypted")))
    (message "Encrypting...")
    (shell-command-on-region
     begin end
     (funcall pgp-command 'encrypt-conventional key)
     out-buffer (eq out-buffer (current-buffer))  pgp-err-buffer-name))
  (pgp-bury-err-buffer-if-empty))

(defun pgp-decrypt-conventional-range (key begin end)
  "Decrypt conventionaly the range BEGIN, END with the KEY."
  (pgp-remove-key-from-history key)
  (pgp-prepare-err-buffer)
  (let ((out-buffer (pgp-prepare-out-buffer "Decrypted")))
    (message "Decrypting...")
    (shell-command-on-region
     begin end
     (funcall pgp-command 'decrypt-conventional key)
     out-buffer (eq out-buffer (current-buffer)) pgp-err-buffer-name))
  (pgp-bury-err-buffer-if-empty))


(defun pgp-sign-range (pass-phrase begin end)
  "Sign the range BEGIN, END, with the signing key indicated by pgp-signer,
unlocked with the pass-phrase."
  (pgp-remove-key-from-history pass-phrase)
  (pgp-prepare-err-buffer)
  (let ((out-buffer (pgp-prepare-out-buffer "Signed")))
    (message "Signing...")
    (shell-command-on-region
     begin end
     (funcall pgp-command 'sign pass-phrase pgp-signer)
     out-buffer (eq out-buffer (current-buffer)) pgp-err-buffer-name))
  (pgp-bury-err-buffer-if-empty))


(defun pgp-check-sign-range (begin end)
  "Check the signature of the range BEGIN, END."
  (pgp-prepare-err-buffer)
  (message "Checking signature...")
  (shell-command-on-region
   begin end
   (funcall pgp-command 'check-sign)
   pgp-err-buffer-name nil pgp-err-buffer-name)
  (pgp-bury-err-buffer-if-empty))


(defun pgp-encrypt-pk-range (pass-phrase recipients begin end)
  "When pass-phrase is not nil, sign with the signing key indicated
by pgp-signer, unlocked with the PASS-PHRASE, and encrypt,
otherwise only encrypt the range BEGIN to END, for the RECIPIENTS."
  (pgp-remove-key-from-history pass-phrase)
  (pgp-prepare-err-buffer)
  (let ((out-buffer (pgp-prepare-out-buffer "PK Encrypted")))
    (message "Encrypting...")
    (if pass-phrase
        (shell-command-on-region
         begin end
         (funcall pgp-command 'sign-encrypt pass-phrase pgp-signer recipients)
         out-buffer (eq out-buffer (current-buffer)) pgp-err-buffer-name)
      (shell-command-on-region
       begin end
       (funcall pgp-command 'encrypt recipients)
       out-buffer (eq out-buffer (current-buffer)) pgp-err-buffer-name))
    (switch-to-buffer out-buffer))
  (pgp-bury-err-buffer-if-empty))


(defun pgp-decrypt-pk-range (pass-phrase begin end)
  "Decrypt the range BEGIN, END, for the keyid unlocked by PASS-PHRASE."
  (pgp-remove-key-from-history pass-phrase)
  (pgp-prepare-err-buffer)
  (let ((out-buffer (pgp-prepare-out-buffer "PK Decrypted")))
    (message "Decrypting...")
    (shell-command-on-region
     begin end
     (funcall pgp-command 'decrypt pass-phrase)
     out-buffer (eq out-buffer (current-buffer)) pgp-err-buffer-name)
    (switch-to-buffer out-buffer))
  (pgp-bury-err-buffer-if-empty))




(defun pgp-encrypt-conventional-region (key)
  "Encrypt conventionaly the region with the KEY."
  (interactive "sKey: ")
  (pgp-encrypt-conventional-range key (region-beginning) (region-end)))


(defun pgp-encrypt-conventional-buffer (key)
  "Encrypt conventionaly the buffer with the KEY."
  (interactive "sKey: ")
  (pgp-encrypt-conventional-range key (point-min) (point-max)))


(defun pgp-decrypt-conventional-region (key)
  "Decrypt conventionaly the region with the KEY."
  (interactive "sKey: ")
  (auto-save-mode -1);; Decrypted buffers must not be auto-saved!
  (pgp-decrypt-conventional-range key (region-beginning) (region-end)))


(defun pgp-decrypt-conventional-buffer (key)
  "Decrypt conventionaly the buffer with the KEY."
  (interactive "sKey: ")
  (auto-save-mode -1);; Decrypted buffers must not be auto-saved!
  (pgp-decrypt-conventional-range key (point-min) (point-max)))



(defun pgp-sign-region (pass-phrase)
  "Sign the region with the pgp-signer key, unlocked by the PASS-PHRASE."
  (interactive "sPass phrase: ")
  (pgp-sign-range pass-phrase (region-beginning) (region-end)))


(defun pgp-sign-buffer (pass-phrase)
  "Sign the buffer with the pgp-signer key, unlocked by the PASS-PHRASE."
  (interactive "sPass phrase: ")
  (pgp-sign-range pass-phrase (point-min) (point-max)))


(defun pgp-check-sign-region ()
  "Check the signature of the region."
  (interactive)
  (pgp-check-sign-range (region-beginning) (region-end)))


(defun pgp-check-sign-buffer ()
  "Check the signature of the buffer."
  (interactive)
  (pgp-check-sign-range (point-min) (point-max)))



(defun pgp-convert-to-utf-8 (start end)
  "PRIVATE
BUG:    The handling of output buffer (whether read-only or not) should be
        done before calling this function!
"
  (interactive "r")
  (shell-command-on-region start end "iconv -f ISO8859-1 -t UTF-8" t t))


(defun pgp-convert-from-utf-8 (start end)
  "PRIVATE
"
  (interactive "r")
  (shell-command-on-region start end "iconv -f UTF-8 -t ISO8859-1" t t))


(defun pgp-sign-encrypt-region (pass-phrase recipients &optional conv-utf-8)
  "Sign and encrypt the region, with pgp-signer key,
unlocked by the PASS-PHRASE, for the given RECIPIENTS."
  (interactive "sPass phrase:
sRecipients:
P")
  (when conv-utf-8
    (pgp-convert-to-utf-8 (region-beginning) (region-end)) )
  (pgp-encrypt-pk-range pass-phrase recipients (region-beginning) (region-end)))


(defun pgp-sign-encrypt-buffer (pass-phrase recipients &optional conv-utf-8)
  "Sign and encrypt the buffer, with pgp-signer key,
unlocked by the PASS-PHRASE, for the given RECIPIENTS."
  (interactive "sPass phrase:
sRecipients:
P")
  (when conv-utf-8
    (pgp-convert-to-utf-8 (point-min) (point-max)) )
  (pgp-encrypt-pk-range pass-phrase recipients (point-min) (point-max)))


(defun pgp-just-encrypt-region (recipients &optional conv-utf-8)
  "Encrypt the region, without signing, for the given RECIPIENTS."
  (interactive "sRecipients:
P")
  (when conv-utf-8
    (pgp-convert-to-utf-8 (region-beginning) (region-end)) )
  (pgp-encrypt-pk-range nil recipients (region-beginning) (region-end)))


(defun pgp-just-encrypt-buffer (recipients &optional conv-utf-8)
  "Encrypt the buffer, without signing, for the given RECIPIENTS."
  (interactive "sRecipients:
P")
  (when conv-utf-8
    (pgp-convert-to-utf-8 (point-min) (point-max)) )
  (pgp-encrypt-pk-range nil recipients (point-min) (point-max)))



(defun pgp-decrypt-region (pass-phrase &optional conv-utf-8)
  "Decrypt the region, for the keyid unlocked by the PASS-PHRASE."
  (interactive "sPass phrase:
P")
  (auto-save-mode -1);; Decrypted buffers must not be auto-saved!
  (pgp-decrypt-pk-range pass-phrase (region-beginning) (region-end))
  (when conv-utf-8
    (pgp-convert-from-utf-8 (point-min) (point-max)) ))


(defun pgp-decrypt-buffer (pass-phrase &optional conv-utf-8)
  "Decrypt the buffer, for the keyid unlocked by the PASS-PHRASE."
  (interactive "sPass phrase:
P")
  (auto-save-mode -1);; Decrypted buffers must not be auto-saved!
  (pgp-decrypt-pk-range pass-phrase (point-min) (point-max))
  (when conv-utf-8
    (pgp-convert-from-utf-8 (point-min) (point-max))))



(defun gunzip ()
  "Uncompress the current buffer."
  (interactive)
  (let ((out-buffer (pgp-prepare-out-buffer "Gunziped")))
    (message "Gunziping...")
    (shell-command-on-region
     (point-min) (point-max)
     "gunzip"
     out-buffer (eq out-buffer (current-buffer)) nil)
    (set-buffer out-buffer)
    (view-mode 1)
    (delete-other-windows (get-buffer-window out-buffer))))


(defun invoke-ding-dictionary ()
  "Retrieve the definition of the selected word with the Ding dictionary."
  (interactive)
  (save-excursion
  (shell-command "( ding -R -x > /dev/null < /dev/null 2>&1  & )" nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some experimental stuff:

(require 'mailheader)

(defun get-emails-in-public-key-ring ()
  "RETURN:  a list of emails registered in your OpenPGP public key ring."
  (split-string
   (shell-command-to-string
     (concat
      (funcall pgp-command 'list-keys)
      "| sed -n -e 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/' "
      "         -e 's/.*\(<.*@.*>\).*/\1/p' "
      "|sort -u"))))



(defun mail-get-recipients ()
  "RETURN: a list of emails set as recipients in current buffer mail
        be it as To:, CC: or BCC:."
  (save-excursion
    (goto-char 0)
    (remove-duplicates
    (mapcar (lambda (email)
              ;; let's clean-up the address.
              (cond
               ((string-match ".*\\(<.*@.*>\\).*" email 0)
                (substring email (match-beginning 1) (match-end 1)))
               ((string-match "@" email 0)
                (format "<%s>" email))
               (t email)))
            (apply 'append
                   (mapcar (lambda (ass)
                             ;; lets select the to, cc, and bcc headers
                             (if (member (car ass) '(to cc bcc))
                                 (split-string (cdr ass) ",")))
                           (mail-header-extract)))))))


(require 'w3 nil t)
(require 'url)
(require 'pjb-emacs)

(defun pgp-lookup-keys (critere)
  "DO:      Search for CRITERE with http://wwwkeys.eu.pgp.net:11371/pks/lookup?
         and if a key is returned, add it to the public key ring.
NOTE:    We don't use gpg --recv-keys because this feature does not exist with
         pgp and because it only accept key ids, not emails..."
  (let ((result (url-retrieve-as-string
                 (format
                  "http://wwwkeys.eu.pgp.net:11371/pks/lookup?op=get&search=%s"
                  (url-hexify-string critere)))))
    (if (string-match "No matching keys" result)
        nil
      ;; let's add the key to our public key ring.
      (save-excursion
        (pgp-prepare-err-buffer)
        (let ( (in-buffer  (get-buffer-create
                            (concat " Adding key for " critere))) )
          (message (buffer-name in-buffer))
          (set-buffer in-buffer)
          (insert result)
          (shell-command-on-region (point-min) (point-max)
                                   (funcall pgp-command 'add-keys)
                                   nil t pgp-err-buffer-name)
          ;; nothing interesting in stdout...
          (set-buffer in-buffer)
          (set-buffer-modified-p nil)
          (kill-buffer in-buffer)
          ;; let's report stderr...
          (set-buffer pgp-err-buffer-name)
          (message (buffer-string))
          (erase-buffer))
        (pgp-bury-err-buffer-if-empty)
        t))))



;; (defun mail-test ()
;;   (interactive)
;;   (let ( (pkr-emails (get-emails-in-public-key-ring))
;;          (recipients (mail-get-recipients))
;;          (cryppients nil) ;; there is a pubkey for them.
;;          (clearients nil) ;; no pubkey found for them.
;;          )
;;     (while recipients
;;       (let ((recipient (car recipients)))
;;         (if (member recipient pkr-emails)
;;             (setq cryppients (cons recipient cryppients))
;;           ;; else lookup in wwwkeys.eu.pgp.net
;;           (if (and (string-match "<.*@.*>" recipient)
;;                    (pgp-lookup-keys recipient))
;;               (setq cryppients (cons recipient cryppients))
;;             (setq clearients (cons recipient clearients)))))
;;       (setq recipients (cdr recipients)));;while
;;     (message "\ncryppients=%S\nclearients=%S\n" cryppients  clearients)
;;     (when (null clearients)
;;
;;       )))

;;;; THE END ;;;;

