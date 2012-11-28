;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:               pjb-mail.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports various functions related to mail handling.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2002-10-01 <PJB> Created.
;;;;
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
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

(require 'pjb-cl)
(require 'pjb-list)

(require 'pjb-strings)
(require 'pjb-emacs)
(require 'pjb-pgp)

(require 'mailheader)
(require 'mm nil t)
(require 'sendmail)




(defun slist-get (slist prop &rest options)
  "
DO:     Extract a value from a property list.
PLIST:  is a property list, which is a list of the form
        (PROP1 VALUE1 PROP2 VALUE2...).  
RETURN: the value corresponding to the given PROP, or nil if PROP is not
        one of the properties on the list.
NOTE:   The difference with plist-get is that this function works with 
        strings properties.
OPTIONS can contain :ignore-case in which case the case string and prefix 
        are matched case insensitively.
"
  (let ((ignore-case (member :ignore-case options))
        )
    (when ignore-case 
      (setq prop (upcase prop)))
    (loop for clist = slist then (cddr clist)
       while clist
       for sprop = (if ignore-case (upcase (car clist)) (car clist))
       while (STRING/= sprop prop)
       finally return (cadr clist))
    )) ;;slist-get



(defun pjb-mail-narrow-to-headers ()
  "Narrow the buffer to the head of the message."
  ;; Taken from message-narrow-to-headers
  (interactive)
  (widen)
  (narrow-to-region
   (goto-char (point-min))
   (if (re-search-forward
        (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
       (match-beginning 0)
       (point-max)))
  (goto-char (point-min))) ;;pjb-mail-narrow-to-headers


(defun pjb-mail-remove-header (header &optional is-regexp first reverse)
  "
PRE:    (pjb-mail-narrow-to-header)
DO:     Remove HEADER in the narrowed buffer.
        If IS-REGEXP, HEADER is a regular expression.
        If FIRST, only remove the first instance of the header.
        If REVERSE, remove the header not matching HEADER (regexp).
RETURN: the number of headers removed.
NOTE:   See original code: message-remove-header
"
  (goto-char (point-min))
  (let ((regexp (if is-regexp header (concat "^" (regexp-quote header) ":")))
        (number 0)
        (case-fold-search t)
        last)
    (while (and (not (eobp))
                (not last))
      (if (let ((case-fold-search t)) 
            ;; case is not significant in headers whatever what.
            (if reverse
                (not (looking-at regexp))
                (looking-at regexp)))
          (progn
            (incf number)
            (when first
              (setq last t))
            (delete-region
             (point)
             ;; There might be a continuation header, so we have to search
             ;; until we find a new non-continuation line.
             (progn
               (forward-line 1)
               (if (re-search-forward "^[^ \t]" nil t)
                   (goto-char (match-beginning 0))
                   (point-max)))))
          (forward-line 1)
          (if (re-search-forward "^[^ \t]" nil t)
              (goto-char (match-beginning 0))
              (point-max))))
    number)) ;;pjb-mail-remove-header



(defun pjb-mail-make-boundary ()
  "
RETURN:  A unique string that can be used as a boundary.
"
  (let ((ut (MULTIPLE-VALUE-LIST (DECODE-UNIVERSAL-TIME (GET-UNIVERSAL-TIME)))))
    (format "%s.%08d.%04d-%02d-%02d-%02d-%02d-%02d/%02d" 
      (system-name) (random 100000000)
      (elt ut 5) (elt ut 4) (elt ut 3)
      (elt ut 2) (elt ut 1) (elt ut 0)
      (or (elt ut 8) 0))))


(defun pjb-mail$$insert-content-headers (&rest headers-k-v)
  "
DO:     Inserts at point 'Content-' MIME headers as directed by headers-k-v
        which should be of the form:
            :key1 value1 :key2 value2
        -->
            Content-Key1: value1
            Content-Key2: value2
"
  ;;(message "headers-k-v=%S\n" headers-k-v)
  (loop for current = headers-k-v then (cddr current)
     while current
     for key = (car current)
     for value = (cadr current)
     do (insert (format "Content-%s: %s\n" 
                  (capitalize (subseq (symbol-name key) 1))
                  value))
     ) ;;loop
  )    ;;pjb-mail$$insert-content-headers


(defun pjb-mail$$select (indicator list)
  "
RETURN:  The sublist of the elements from LIST for which the indicator function
         does not return nil.
"
  (let ( (result nil) )
    (dolist (element list)
      (when (funcall indicator element)
        (setq result (cons element result))))
    result)
  ) ;;pjb-mail$$select



(defun pjb-mail-make-mime ()
  "
PRE:    the current buffer contains a RFC822 message.
DO:     If the mail is not already a MIME message, 
        then makes it a MIME message.
POST:   the current buffer contains a MIME multipart/mixed message,
        either the original one, or a new one with the old message contents
        as the first MIME section.
"
  (let  ((headers 
          (progn (widen) (goto-char (point-min)) (mail-header-extract)))
         boundary)

    (unless (mail-header 'mime-version headers)
      ;; not already a MIME message. 
      ;; let's encapusulate the existing body into the first section before
      ;; adding a new section for the attached file.
      (setq boundary (pjb-mail-make-boundary))
      (pjb-mail-narrow-to-headers)
      (goto-char (point-max))
      (insert "MIME-Version: 1\n")
      (pjb-mail$$insert-content-headers :transfer-encoding "8bit"
                                        :type (format "multipart/mixed; boundary=\"%s\""
                                                boundary)
                                        :disposition "inline")
      (widen)
      (mail-text)
      (insert "\n")
      (insert "This is a MIME formated message.\n\n")
      (insert (format "--%s\n" boundary))
      (pjb-mail$$insert-content-headers :encoding "8bit"
                                        :type "text/plain; charset=iso-8859-1"
                                        :language "fr,en"
                                        :disposition "inline"
                                        :description "Cover letter.")
      (goto-char (point-max))
      (insert "\n\n")
      (insert (format "--%s--\n" boundary))
      (insert "\n")
      (widen)
      ) ;;unless not already a MIME message.
    ))  ;;pjb-mail-make-mime


(defun pjb-mail-make-multipart/mixed ()
  "
PRE:    the current buffer contains a MIME message.
DO:     if it's not already a multipart/mixed message, 
        then makes it multipart/mixed.
POST:   the current buffer contains a MIME multipart/mixed message,
        either the original one, or a new one with the old message contents
        as the first MIME section.
"
  (let ((headers 
         (progn (widen) (goto-char (point-min)) (mail-header-extract)))
        boundary)

    (unless (string-prefix-p  (mail-header 'content-type headers) 
                              "multipart/mixed" :ignore-case)
      ;; already a MIME, but not a mixed
      ;; let's move the content- headers to the existing body and
      ;; encapusulate it into the first section before
      ;; adding a new section for the attached file.

      (setq boundary (pjb-mail-make-boundary))
      ;; since it's not a multipart/mixed, there's no boundary yet.
      
      ;; first, let's report the current "Content-" headers into the body.
      (widen)
      (mail-text)
      (insert "\n")
      (insert "This is a MIME formated message.\n\n")
      (insert (format "--%s\n" boundary))
      (apply 'pjb-mail$$insert-content-headers
             (flatten
              (mapcar 
               (lambda (item)
                 (list (intern (concat ":" 
                                       (chop-prefix (symbol-name (car item))
                                                    "content-" :ignore-case)))
                       (cdr item)))
               (pjb-mail$$select 
                 (lambda (item)
                   (string-prefix-p (symbol-name (car item))
                                    "content-" :ignore-case))
                 headers)
               )))
      (insert "\n")
      ;; (sleep 0.001) ;; to display the changes...
      (goto-char (point-max)) ;; end of message text
      (insert "\n\n")
      (insert (format "--%s--\n" boundary))
      (insert "\n")

      ;; next, let's remove the current "Content-" headers
      (pjb-mail-narrow-to-headers)
      (pjb-mail-remove-header "^Content-.*" t nil nil)
      ;; finally, let's add the new "Content-" headers.
      (goto-char (point-max)) ;; end of headers
      ;; no need to insert MIME-Version, it's aready a MIME message.
      (pjb-mail$$insert-content-headers
       :transfer-encoding "8bit"
       :type (format "multipart/mixed; boundary=\"%s\""
               boundary)
       :disposition "inline")
      (widen)
      )))


(defun pjb-mail-get-multipart/mixed-boundary (headers)
  "
RETURN: the boundary attribute of the multipart/mixed content-type
        header in headers.
"
  (replace-regexp-in-string 
   "^\"\\(.*\\)\"$" "\\1"
   (slist-get (delete "" (flatten
                          (mapcar
                           (lambda (s) (split-string s "="))
                           (split-string (chop-prefix
                                          (mail-header 'content-type headers) 
                                          "multipart/mixed" :ignore-case) 
                                         "[\n	 ]*;[\n	 ]*")))) 
              "boundary" :ignore-case) nil nil))


(defun pjb-mail-attach-file (file-name &optional compress)
  "
DO:      Ask for the path of a file to attach to the message being edited 
         in the current buffer.
"
  (interactive "*fFile to attach: 
P")
  ;; (unless (eq major-mode 'mail-mode)
  ;;   (error "I can attach a file only to a mail buffer."))
  (pjb-mail-make-mime)
  (pjb-mail-make-multipart/mixed)
  (let ((headers (progn (widen) (goto-char (point-min)) (mail-header-extract))))
    (unless (mail-header 'content-type headers)
      (error "Can't get Content-Type: header!"))
    ;; already a MIME and already a mixed
    ;; let's just recover the boundary and add a section.
    (let ((boundary (pjb-mail-get-multipart/mixed-boundary headers)))
      (when (or (null boundary) (= 0 (length boundary)))
        (error "Invalid multipart/mixed boundary. Please fix."))
      (widen)
      (goto-char (point-max))
      (if (re-search-backward (format "^--%s\\(--\\)?\n" 
                                (regexp-quote boundary))
                              (point-min) t)
          (progn ;; found
            (goto-char (match-beginning 0))
            (insert (concat "--" boundary "\n"))
            ;; TODO: handle errors in case insert-attachment-here fails.
            (pjb-mail-insert-attachment-here file-name compress))
          (error "No boundary in a multipart/mixed. Please fix.")))))



(defun parse-address-list-string (adlist-string)
  "
DO:      parse the adlist-string and extract from it the email addresses.
RETURN:  a list of strings containing each one address.
"
  (let ((i 0)
        (len (length adlist-string))
        (result  nil)
        (curaddr nil)
        (state      :unknown) 
        ;; ( :unknown :in-address 
        ;;   :in-comment-paren :in-escape-quote :in-comment-angle )
        (prevstate  nil)
        (escape     nil) ;; ?\
        (start 0)
        curchar
        (dbl-quote   (character "\"" ))
        (left-paren  (character "(" ))
        (right-paren (character ")" ))
        (left-angle  (character "<" ))
        (right-angle (character ">" ))
        (anti-slash  (character "\\" ))
        (comma       (character "," ))
        )

    (while (<= i len)
      
      (if (< i len)
          (setq curchar (aref adlist-string i))
          (setq curchar nil))
      
      (cond
       
        ((eq state :unknown)
         (if (and escape curchar)
             (progn
               (push curchar curaddr)
               (setq escape nil))
             (cond
               ((or (null curchar) (= curchar comma))
                (let ((address (list-to-string (nreverse curaddr))))
                  (when (< 0 (length address))
                    (push (chop-spaces address) result))
                  (setq curaddr nil)))
               ((= curchar dbl-quote) 
                (push curchar curaddr)
                (setq prevstate state
                      state     :in-escape-quote))
               ((= curchar left-paren)   (setq state  :in-comment-paren))
               ((= curchar left-angle)   (setq state  :in-address 
                                               curaddr nil))
               ((= curchar anti-slash)
                (push curchar curaddr)
                (setq escape t))
               (t (push curchar curaddr))
               ))) ;;state :unknown

        ((eq state :in-address)
         (if (and escape curchar)
             (progn
               (push curchar curaddr)
               (setq escape nil))
             (cond
               ((or (null curchar) (= curchar right-angle))
                (let ((address (list-to-string (nreverse curaddr))))
                  (when (< 0 (length address))
                    (push (chop-spaces address) result))
                  (setq curaddr nil))
                (setq state :in-comment-angle))
               ((= curchar anti-slash)   
                (push curchar curaddr)
                (setq escape t))
               (t (push curchar curaddr))
               ))) ;;state :in-address

        ((eq state :in-comment-paren)
         (if escape
             (setq escape nil)
             (cond
               ((null curchar))
               ((= curchar right-paren) (setq state :unknown))
               ((= curchar anti-slash)  (setq escape t))
               ))) ;;state :in-comment-paren
       
        ((eq state :in-escape-quote)
         (if escape
             (progn
               (push curchar curaddr)
               (setq escape nil))
             (cond
               ((null curchar))
               ((= curchar dbl-quote)
                (setq state prevstate))
               ((= curchar anti-slash)  
                (push curchar curaddr)
                (setq escape t))
               (t 
                (push curchar curaddr))
               ))) ;;state :in-escape-quote

        ((eq state :in-comment-angle)
         (if escape
             (setq escape nil)
             (cond
               ((null curchar))
               ((= curchar anti-slash)  (setq escape t))
               ((= curchar comma)       (setq state :unknown))        
               ))) ;;state :in-comment-angle
        )
      (setq i (1+ i))
      )     ;;while
    result)) ;;parse-address-list-string



;;;   (mapc (lambda (adls)
;;;           (printf "%S\n    %S\n\n" adls (parse-address-list-string  adls)))
;;;         ;; --------------------------------------------------
;;;         '("Bernard <bern@dot.com>,Agathe <agat@dot.com>"
;;;           "ber\\<nard <ber(n)@dot.com>"
;;;           "pjb@informatimago.com"
;;;           "(comment) pjb@informatimago.com"
;;;           "pjb@informatimago.com (comment)"
;;;           "(comment) pjb@informatimago.com (comment)"
;;;           "pjb@(comment)informatimago.com"
;;;           "<pjb@informatimago.com>"
;;;           "<(comment) pjb@informatimago.com>"
;;;           "<pjb@informatimago.com (comment)>"
;;;           "<(comment) pjb@informatimago.com (comment)>"
;;;           "<pjb@(comment)informatimago.com>"
;;;           "a,b,c"
;;;           ","
;;;           ""
;;;           "a,,b"
;;;           "(abc),(def)"
;;;           "(abc)"
;;;           "a\\,b,c"
;;;           "\\\\,"
;;;           "\\,"
;;;           "\""
;;;           "a\\,,b"
;;;           "\\a,,b"
;;;           "\\(abc)\\,(def)"
;;;           "\\(abc)"
;;;           "\"Toto Duchnock\" <toto@dot.com>"
;;;           "<\"toto duchnock\"@dot.com> haha ,(toto@dot.com) toto"
;;;           ))


(defun pjb-mail-sign    (pass-phrase)
  "
Content-Type: multipart/signed; micalg=pgp-sha1;
	protocol=\"application/pgp-signature\"; boundary=\"5mCyUwZo2JvN/JJP\"

--5mCyUwZo2JvN/JJP
Content-Type: text/plain; charset=us-ascii
Content-Disposition: inline


--5mCyUwZo2JvN/JJP
Content-Type: application/pgp-signature
Content-Disposition: inline

-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1.0.6 (GNU/Linux)
Comment: w00t!

iD8DBQE9xuYI76uNUzjDrRQRAmiHAJ9rPkgxJBK6TdIaiaalc7U5JxLX+wCfU/IG
4KOwAyGYpBr18qA4av8qMjA=
=WfP3
-----END PGP SIGNATURE-----

--5mCyUwZo2JvN/JJP--
"
  ) ;;pjb-mail-sign

(defun pjb-mail-encrypt (pass-phrase recipients &optional conv-utf-8)
  "
DO:      Encrypt the current mail and put it into a multipart/encrypted MIME.
         Sign and encrypt the buffer, with pgp-signer key, 
         unlocked by the PASS-PHRASE, for the given RECIPIENTS.
"
  (interactive 
   (list
    ;; pass-phrase
    (progn
      (unless (eq major-mode 'mail-mode)
        (error "I can encrypt a message only in a mail buffer."))
      (let ((pass-phrase (read-string "Pass phrase: ")))
        (pgp-remove-key-from-history pass-phrase)
        pass-phrase))
    ;; recipients
    (let ((recipients
           (unsplit-string
            (remove-duplicates
             (flatten 
              (mapcar ;; extract only the email addresses
               (lambda (item) (parse-address-list-string (cdr item)))
               (pjb-mail$$select ;; select only To: Cc: and Bcc: headers.
                 (lambda (item) (member (car item) '(to cc bcc))) 
                 (progn ;; get the headers.
                   (widen) 
                   (expand-mail-aliases (point-min) (progn (mail-text) (point)))
                   (goto-char (point-min)) 
                   (mail-header-extract)))))))))
      (if recipients
          recipients
          (read-string "Recipients: ")))
    ;; conv-utf-8
    current-prefix-arg
    ))

  (unless (eq major-mode 'mail-mode)
    (error "I can encrypt a message only in a mail buffer."))
  
  (pjb-mail-make-mime)
  
  (let ((headers 
         (progn (widen) (goto-char (point-min)) (mail-header-extract)))
        (boundary (pjb-mail-make-boundary))
        (text-beg (make-marker))
        (text-end (make-marker))
        )

    ;; copy the Content-* headers to the message body, ready to be encrypted.
    (mail-text)
    (apply 'pjb-mail$$insert-content-headers
           (flatten
            (mapcar 
             (lambda (item)
               (list (intern (concat ":" 
                                     (chop-prefix (symbol-name (car item))
                                                  "content-" :ignore-case)))
                     (cdr item)))
             (pjb-mail$$select 
               (lambda (item)
                 (string-prefix-p (symbol-name (car item))
                                  "content-" :ignore-case))
               headers)
             )))
    (insert "\n")

    ;; next, let's remove the current "Content-" headers
    (pjb-mail-narrow-to-headers)
    (pjb-mail-remove-header "^Content-.*" t nil nil)
    ;; let's add the new "Content-" headers.
    (goto-char (point-max)) ;; end of headers
    ;; no need to insert MIME-Version, it's aready a MIME message.
    (pjb-mail$$insert-content-headers 
     :transfer-encoding "7bit" ;; encrypted is pure ASCII
     :type (format (concat "multipart/encrypted; "
                           "protocol=\"application/pgp-encrypted\"; "
                           "boundary=\"%s\"") boundary)
     :disposition "inline")
    (widen)

    (mail-text)
    (set-marker text-beg (point))
    (set-marker text-end (point-max))
    (insert-before-markers 
     (format
         (concat "Beginning of MIME encrypted message.\n"
                 "\n"
                 "--%s\n"
                 "Content-Type: application/pgp-encrypted\n"
                 "Content-Disposition: inline; filename=\"msg.asc\"\n"
                 "\n"
                 "Version: 1\n"
                 "\n"
                 "--%s\n"
                 "Content-Type: application/octet-stream\n"
                 "Content-Disposition: inline\n"
                 "\n") boundary boundary))
    (goto-char text-end)
    (insert
     (format
         (concat "\n"
                 "--%s--\n"
                 "\n"
                 "End of MIME encrypted message.\n"
                 "\n") boundary))

    (condition-case  exception
        (progn
          (when conv-utf-8
            (pgp-convert-to-utf-8 text-beg text-end) )
          (pgp-encrypt-pk-range pass-phrase recipients text-beg text-end)
          )
      (error 
       (message "PGP encryption raised an exception: %S" exception)))
    (set-marker text-beg nil)
    (set-marker text-end nil)
    
    (if (< 0 (buffer-size (buffer-named "*PGP*")))
        (save-selected-window
          (switch-to-buffer-other-window "*PGP*")
          (fit-window-to-buffer)
          ))
    )) ;;pjb-mail-encrypt



(defun old-pjb-mail-attach-file (file-name &optional compress)
  "
"
  (interactive "*fFile to attach: 
P")
  (unless (eq major-mode 'mail-mode)
    (error "I can attach a file only to a mail buffer."))
  (let (headers boundary
                )

    (setq headers 
          (progn (widen) (goto-char (point-min)) (mail-header-extract)))

    ;;(pjb-mail-narrow-to-headers)
    (unless (mail-header 'mime-version headers)
      ;; not already a MIME message. 
      ;; let's encapusulate the existing body into the first section before
      ;; adding a new section for the attached file.
      (setq boundary (pjb-mail-make-boundary))
      (pjb-mail-narrow-to-headers)
      (goto-char (point-max))
      (insert "MIME-Version: 1\n")
      (pjb-mail$$insert-content-headers :transfer-encoding "8bit"
                                        :type (format "multipart/mixed; boundary=\"%s\""
                                                boundary)
                                        :disposition "inline")
      (widen)
      (mail-text)
      (insert "\n")
      (insert "This is a MIME formated message.\n\n")
      (insert (format "--%s\n" boundary))
      (pjb-mail$$insert-content-headers :encoding "8bit"
                                        :type "text/plain; charset=iso-8859-1"
                                        :language "fr,en"
                                        :disposition "inline"
                                        :description "Cover letter.")
      (goto-char (point-max))
      (insert "\n\n")
      (insert (format "--%s--\n" boundary))
      (insert "\n")
      (setq headers 
            (progn (widen) (goto-char (point-min)) (mail-header-extract)))
      ) ;;unless not already a MIME message.


    (unless (string-prefix-p  (mail-header 'content-type headers) 
                              "multipart/mixed" :ignore-case)
      ;; already a MIME, but not a mixed
      ;; let's move the content- headers to the existing body and
      ;; encapusulate it into the first section before
      ;; adding a new section for the attached file.

      (setq boundary (pjb-mail-make-boundary))
      ;; since it's not a multipart/mixed, there's no boundary yet.
      
      ;; first, let's report the current "Content-" headers into the body.
      (widen)
      (mail-text)
      (insert "\n")
      (insert "This is a MIME formated message.\n\n")
      (insert (format "--%s\n" boundary))
      (apply 'pjb-mail$$insert-content-headers
             (flatten
              (mapcar 
               (lambda (item)
                 (list (intern (concat ":" 
                                       (chop-prefix (symbol-name (car item))
                                                    "content-" :ignore-case)))
                       (cdr item)))
               (pjb-mail$$select 
                 (lambda (item)
                   (string-prefix-p (symbol-name (car item))
                                    "content-" :ignore-case))
                 headers)
               )))
      (sleep 0.001)
      (goto-char (point-max)) ;; end of message text
      (insert "\n\n")
      (insert (format "--%s--\n" boundary))
      (insert "\n")

      ;; next, let's remove the current "Content-" headers
      (pjb-mail-narrow-to-headers)
      (pjb-mail-remove-header "^Content-.*" t nil nil)
      ;; finally, let's add the new "Content-" headers.
      (goto-char (point-max)) ;; end of headers
      ;; no need to insert MIME-Version, it's aready a MIME message.
      (pjb-mail$$insert-content-headers :transfer-encoding "8bit"
                                        :type (format "multipart/mixed; boundary=\"%s\""
                                                boundary)
                                        :disposition "inline")
      
      (setq headers 
            (progn (widen) (goto-char (point-min)) (mail-header-extract)))
      ) ;;unless not already a multipart/mixed

    (unless (mail-header 'content-type headers)
      (error "Can't get Content-Type: header!"))
    ;; already a MIME and already a mixed
    ;; let's just recover the boundary and add a section.
    (setq boundary
          (replace-regexp-in-string "^\"\\(.*\\)\"$" "\\1" (slist-get (flatten (mapcar (lambda (s) (split-string s "=")) (split-string (chop-prefix (mail-header (quote content-type) headers) "multipart/mixed" :ignore-case) "[\n	 ]*;[\n	 ]*"))) "boundary" :ignore-case) nil nil))
    (when (or (null boundary) (= 0 (length boundary)))
      (error "Invalid multipart/mixed boundary. Please fix."))
    (widen)
    (goto-char (point-max))
    (if (re-search-backward (format "^--%s\\(--\\)?\n" 
                              (regexp-quote boundary))
                            (point-min) t)
        (progn ;; found
          (goto-char (match-beginning 0))
          (insert (concat "--" boundary "\n"))
          ;; TODO: handle errors in case insert-attachment-here fails.
          (pjb-mail-insert-attachment-here file-name compress)
          )
        (error "No boundary in a multipart/mixed. Please fix."))
    ) ;;progn
  )   ;;old-pjb-mail-attach-file




(defvar *pjb-mail-inline-types*
  '("image/.*"))

(defvar *pjb-mail-mime-type-as-8bit* 
  '(
    "application/ghostview"
    "application/mac-binhex40"
    "application/pgp"
    "application/pgp-signature"
    "application/postscript"
    "application/rtf"
    "application/x-csh"
    "application/x-httpd-php"
    "application/x-perl"
    "application/x-sh"
    "application/x-shar"
    "application/x-tcl"
    "application/x-tex"
    "application/x-texinfo"
    "application/x-troff"
    "application/x-troff-man"
    "application/x-troff-me"
    "application/x-troff-ms"
    "application/octetstream"
    "message/external-body"
    "message/news"
    "message/partial"
    "message/rfc822"
    "multipart/alternative"
    "multipart/appledouble"
    "multipart/digest"
    "multipart/mixed"
    "multipart/parallel"
    "text/css"
    "text/html"
    "text/plain"
    "text/richtext"
    "text/tab-separated-values"
    "text/x-setext"
    "text/x-sgml"
    "text/x-vCalendar"
    "text/x-vCard"
    "text/xml"
    "text/vnd.rn-realtext"
    )
  "List of mime type that can be sent with transport-encoding: 8bit.
Others will be sent as base64.")

(defun pjb-mail-insert-attachment-here (file-name &optional compress)
  "
DO:     Insert a file attachment at the point, prefixed with Content- headers
        and possibly encoded. May ask the user for further attributes.
"
  (setq file-name (expand-file-name file-name))
  (unless (file-exists-p file-name)
    (error "File '%s' does not exist." file-name))
  (when (file-directory-p file-name)
    (error "Can't attach a directory!"))
  (let ((out-fname (basename file-name))
        (type  nil)
        transfer-encoding 
        description
        (disposition "attachment"))     ; may be "inline"
    (if compress
        (setq type "application/x-gzip"
              transfer-encoding "base64")
        (progn
          ;; (mm-parse-mimetypes)
          ;; (when (string-match "\\(\\.[^\\.]+\\)$" file-name)
          ;;   (setq type (mm-extension-to-mime (match-string 0 file-name))))
          (unless type
            (setq type 
                  (replace-regexp-in-string
                   ", English" ""
                   (car (split-string
                         ;; TODO: handle errors
                         (shell-command-to-string
                          (format "file -L -b -i %s 2>/dev/null"
                            (shell-quote-argument file-name))) "\n")) t t)))
          
          (when (or (null type) (= 0 (length type)) (string= "data" type))
            (setq type
                  (read-string "Can't determine mime-type. Please tell me: "
                               "application/octetstream")))
          (when (= 0 (length type))
            (setq type "application/octetstream"))
          (if (member* (car (split-string type " *; *"))
                      *pjb-mail-mime-type-as-8bit* :test (function string-equal*))
              (setq transfer-encoding "8bit")
              (setq transfer-encoding "base64"))
          (when (member* (car (split-string type " *; *"))
                        *pjb-mail-inline-types*
                        :test (lambda (s r)
                                ;; (message "(%S %S) -> %S" s r (string-match r s))
                                (string-match r s)))
            (setf disposition "inline"))))
    (setq description (read-string "Please enter attachment description: "))
    (when (= 0 (length description))
      (setq description 
            (format "Attachment %s" (shell-quote-argument out-fname))))
    (pjb-mail$$insert-content-headers 
     :transfer-encoding transfer-encoding
     :type type
     :disposition (format "%s; filename=\"%s\""
                    disposition
                    (shell-quote-argument out-fname))
     :description description)
    (insert "\n\n")
    (previous-line 1)
    (if (STRING= "8bit" transfer-encoding)
        (insert-file-contents file-name)
        (if compress
            (shell-command (format "gzip<%s|base64-encode" 
                             (shell-quote-argument file-name))  t nil)
            (shell-command (format "base64-encode<%s" 
                             (shell-quote-argument file-name))  t nil)))))




(when (require 'vm nil t)
  (require 'vm-vars)

  ;;----------------------------------------------------------------------
  ;; Create auto-folders
  ;;----------------------------------------------------------------------
  (defstruct (afr (:type list)) label slot regexp)

  (defvar *auto-folder-regexps*
    '(("Subject"   vm-subject-of  "^Subject:")
      ("To"        vm-to-of       "^To:")
      ("From"      vm-from-of     "^From:")
      ("FromToCc"  vm-from-of     "^\\(From:\\|To:\\|Cc:\\)")))

  (defun vm-current-message () (car vm-message-pointer))

  ;; (defun pjb-vm-create-auto-folder-and-save-message (selector)
  ;;   (interactive
  ;;    (list
  ;;     (completing-read 
  ;;      "Selector: " (mapcar (afr-label) *auto-folder-regexps*) nil t "From")))
  ;;   (let ((
  ;; 
  ;;          (vm-subject-of message)
  ;;          (vm-from-of    message)
  ;;          (vm-to-of      message)
  ;;          (vm-cc-of      message)
  ;;          
  ;; (vm-auto-select-folder vm-message-pointer vm-auto-folder-alist)
  
  );; when require



(provide 'pjb-mail)
;;;; THE END ;;;;
