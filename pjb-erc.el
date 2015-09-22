;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-erc.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ERC stuff.
;;;;    
;;;;    Don't worry anymore about flooding a channel!
;;;;
;;;;    This yank function for erc uses lisppaste when the text to yank
;;;;    is more than *erc-yank-flood-limit* lines long.
;;;;
;;;;    If the text is below this limit, it's yanked using the function
;;;;    set in *erc-yank-function* (eg. can be yank or delete-region-and-yank).
;;;;
;;;;    Otherwise, if the current buffer is one of the buffer where lisppaste
;;;;    works, then use lisppaste on this buffer (and let lisppaste send its
;;;;    notification message; the Title is taken as the first non empty line
;;;;    of the pasted text).
;;;;
;;;;    Otherwise, the text is pasted to the "None" lisppaste channel, and the
;;;;    url of the lisppaste is yanked into the current buffer.
;;;;
;;;;    You can safely use:
;;;;       (global-set-key (kbd "C-y") (function erc-yank))
;;;;    or just add it to some erc buffer hook.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-11-15 <PJB> Extracted from ~/.emacs; Fusioned erc-yank.el
;;;;    2006-10-23 <PJB> Updated for the new anti-spam lisppaste feature.
;;;;    2006-05-15 <PJB> Created.
;;;;BUGS
;;;;    We should check if there's a recent paste with same title, and
;;;;    if there is, add: annotate=#pastenumber to get an annotation.
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2014
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(require 'cl)

(require 'w3           nil t) ; not anymore. we should clean them up.
(require 'w3-forms     nil t)

(require 'xml) ; parse-xml-region 


(require 'pjb-cl)
(require 'pjb-make-depends)
(require 'pjb-html)

(eval-when (compile load exec)
  (require 'erc-auto     nil t)
  ;;(autoload 'erc-select "erc" "Connect to an IRC server." t)
  ;;(autoload 'irc   "Kiwi.4.37" "Start an IRC sub-process."  t)
  ;;(setf irc-nick "pjb__")
  (require 'erc          nil t)
  (require 'erc-nets     nil t)
  (require 'erc-fill     nil t)
  (require 'erc-lang     nil t)
  (require 'erc-stamp    nil t)
  (require 'erc-match    nil t)
  (require 'erc-button   nil t)
  (require 'erc-truncate nil t)
  (require 'erc-nickserv nil t))

(require 'erc)


;;;---------------------------------------------------------------------
;;; erc-yank / lisppaste
;;;---------------------------------------------------------------------
  
(defconst +newline+ 10)

(defvar *erc-yank-flood-limit* 5
  "Maximum number of lines allowed to yank to an erc buffer.")

(defvar *erc-yank-function* (function delete-region-and-yank)
  "The function called by erc-yank to actually yank
   (function delete-region-and-yank) or (function yank)")

(defun pjb-erc-buffer-p (buffer)
  "Returns whether buffer is an erc buffer."
  (member buffer (erc-buffer-list)))

(defun pjb-erc-buffer-channel (buffer)
  "Returns the channel associated to an erc buffer, or nil if not an erc buffer"
  ;; (and (pjb-erc-buffer-p buffer) (buffer-name buffer))
  (save-excursion
    (set-buffer buffer)
    (erc-default-target)))

(defun pjb-text-to-yank (arg)
  "Returns the text to yank. This should be used by yank for consistency..."
  (current-kill (cond ((listp arg) 0)
                      ((eq arg '-) -2)
                      (t           (1- arg)))))

(defun pjb-http-get (url)
  "Fetches a resource at URL, and returns it."
  (let ((result (shell-command-to-string
                 (format "PATH='%s' wget --no-convert-links -q -nv -o /dev/null -t 3  -O -  %s"
                         (mapconcat (function identity) exec-path ":")
                         (shell-quote-argument url)))))
    (if (string= result "/bin/bash: wget: command not found\n")
        (error result)
        result)))

(defun pjb-parse-xml (xml)
  "Parse the XML string."
  (with-temp-buffer
    (insert xml)
    (xml-parse-region (point-min) (point-max))))

(defun pjb-parse-html (html)
  "Parse the HTML string."
  (pjb-parse-xml html))


(defun pjb-find-html-tag (tag html)
  (cond
    ((atom html) nil)
    ((eq tag (car html)) html)
    (t (or (pjb-find-html-tag tag (car html))
           (pjb-find-html-tag tag (cdr html))))))


(defvar *lisp-paste-url*        "http://paste.lisp.org/new")
(defvar *lisp-paste-action-url* "http://paste.lisp.org/submit")
(defvar *lisp-paste-channels* nil)
(defvar *lisp-paste-channel-list-url* "http://paste.lisp.org/channels")


;; (setf *lisp-paste-url*        "http://paste.lisp.org/new"
;;       *lisp-paste-action-url* "http://paste.lisp.org/submit"
;;       *lisp-paste-channels* nil
;;       *lisp-paste-channel-list-url* "http://paste.lisp.org/channels")


(defun pjb-collect-channels (tree)
  (remove-if-not
   (lambda (item)
     (and (stringp item)
          (string-match "^#\+[a-z0-9A-Z][-a-z0-9A-Z]*[a-z0-9A-Z]$" item)))
   (flatten tree)))

(defun pjb-cache-lisp-paste ()
  (let ((html (parse-html (http-get *lisp-paste-channel-list-url*))))
    (setf *lisp-paste-channels* (collect-channels html))))

(defun pjb-lisp-paste-action-url ()
  "Returns the action of paste.lisp.org."
  *lisp-paste-action-url*)

(defun pjb-lisp-paste-channels ()
  "Returns the list of channels paste.lisp.org can paste to."
  (unless *lisp-paste-channels*
    (cache-lisp-paste))
  *lisp-paste-channels*)

     
(defun pjb-http-read (process)
  (accept-process-output process 5 0)
  (save-excursion
    (set-buffer (process-buffer process))
    (buffer-string)))

(defun pjb-http-send (process ctrl-string &rest args)
  ;; (message "http-send: %s"  (apply (function format) ctrl-string args))
  (process-send-string process (apply (function format) ctrl-string args)))

(defun pjb-split-header (http-output)
  (let* ((header-end (or (search "\r\n\r\n" http-output)
                         (search "\n\n" http-output)))
         (headers (mapcar
                   (lambda (line) (string-trim "\r" line))
                   (split-string (subseq http-output 0 header-end) "\n"))))
    (values
     (first headers)
     (mapcar (lambda (header)
               (let ((colon (position (character ":") header)))
                 (cons (string-trim " " (subseq header 0 colon))
                       (string-trim " " (subseq header (1+ colon))))))
             (rest headers))
     (string-trim "\r\n" (subseq http-output  header-end)))))

(defun pjb-http-session (method url headers data)
  (with-temp-buffer
    (let* ((url     (url-generic-parse-url url))
           (process (open-network-stream
                     "http-post"
                     (current-buffer)
                     (url-host url)
                     (nth-value 0 (cl:parse-integer (url-port url))))))
      (unwind-protect
           (progn
             (http-send process "%s %s HTTP/1.0\r\n"
                        method (url-recreate-url url))
             (dolist (item headers)
               (http-send process "%s: %s\r\n" (car item) (cdr item)))
             (http-send process "\r\n%s" data)
             (process-send-eof process)
             (split-header (http-read process)))
        (delete-process process)))))


(defun pjb-lisp-paste (network channel nick text)
  (if (fboundp 'w3-form-encode-xwfu)
      (let* ((channel (if (and (eq 'freenode network)
                               (member* channel (lisp-paste-channels)
                                        :test (function string=)))
                          channel "None"))
             (data    (subseq
                       (apply
                        (function concat)
                        (mapcar
                         (lambda (attribute)
                           (concat
                            "&"
                            (w3-form-encode-xwfu (car attribute))
                            "="
                            (w3-form-encode-xwfu (cdr attribute))))
                         (list
                          (cons "channel" channel)
                          (cons "username" nick)
                          (cons "title"
                                (replace-regexp-in-string
                                 "\\`\n*\\([^\n]*\\)\n\\(.\\|\n\\)*\\'" "\\1" text))
                          (cons "colorize" "")
                          (cons "text"
                                (replace-regexp-in-string "\n" "\r\n" text))))) 1))
             (session (http-session
                       "POST" (or (lisp-paste-action-url)
                                  (error "Lisppaste is down!"))
                       (list '("Content-Encoding" .
                               "application/x-www-form-urlencoded")
                             (cons "Content-Length" (length data)))
                       data)))
        ;; (message "%s" (second (split-string (first session)  " ")))
        (when (string= "200" (second (split-string (first session) " ")))
          (values (cdr (assoc 'href (second (find-html-tag
                                             'a (parse-html (third session))))))
                  (not (string= "None" channel)))))
      (error "No w3 to encode the attributes...")))


(defvar *paste-lisp-org-delay* (* 1 60 60)
  "(seconds) delay between two ping checks")
(defvar *paste-lisp-org-cache* (cons 0 nil)
  "(time-of-last-check . paste.lisp.org-available-p)")

(defun pjb-paste-lisp-org-available-p ()
  (and nil
       (if (cl:< (GET-UNIVERSAL-TIME)
                 (cl:+ (car *paste-lisp-org-cache*) *paste-lisp-org-delay*))
           (cdr *paste-lisp-org-cache*)
           (progn
             (message "pinging paste.lisp.org")
             (let ((result (zerop (car (read-from-string
                                        (shell-command-to-string
                                         "ping -q -c 1 -w 2 paste.lisp.org>/dev/null 2>&1;echo $?"))))))
               (message (format "pinging paste.lisp.org --> %s "
                                (if result 'success 'failed)))
               (setf (car *paste-lisp-org-cache*) (GET-UNIVERSAL-TIME)
                     (cdr *paste-lisp-org-cache*) result))))))
  


(defun pjb-erc-yank (&optional arg)
  "Yank or lisppaste depending on the size of the yanked text.
When inside a erc channel buffer, if the text to yank is more than 3 lines,
then use http://paste.lisp.org to lisppaste it instead of yanking it.
Otherwise, just yank it.
"
  (interactive "*P")
  (if (pjb-erc-buffer-p (current-buffer))
      (let* ((text  (pjb-text-to-yank arg))
             (lines (count +newline+ text)))
        (if (<= lines *erc-yank-flood-limit*)
            (funcall *erc-yank-function* arg)
            (if (pjb-paste-lisp-org-available-p)
                (multiple-value-bind
                      (url sent-p)
                    (lisp-paste (funcall (if (fboundp 'erc-network)
                                             'erc-network
                                             'erc-current-network))
                                (pjb-erc-buffer-channel (current-buffer))
                                (erc-current-nick)
                                text)
                  (unless sent-p
                    (insert-for-yank url)))
                (error "To many lines to yank: %d" lines))))
      (funcall *erc-yank-function* arg)))

 

;;;---------------------------------------------------------------------
;;; erc answers
;;;---------------------------------------------------------------------

(defparameter *pjb-erc-answers*
  '((macro . "The big secret of macros in lisp, is that they are functions
just like the other functions.  

The only difference, is that defmacro hooks those functions into the
compiler, to be called at compilation time, passed a form, and expected
to return a new form to be compiled instead of the original form.

\(The original form is actually destructured according to the macro
lambda list; in emacs &whole isn't implemented, but in Common Lisp it
allows receiving the original form intact).")
    (lisp-1-vs-lisp-2-technical-issues  
     . "Please read: http://www.nhplace.com/kent/Papers/Technical-Issues.html")
    (equal         
     . "Please read: http://www.nhplace.com/kent/PS/EQUAL.html")
    (ambitious-eval
     . "Please read: http://www.nhplace.com/kent/PS/Ambitious.html")
    (what-implementation  
     . "To get help choosing a CL implementation, connect to telnet://voyager.informatimago.com:8101 ; have a look at http://www.cliki.net/Common%20Lisp%20implementation")
    (clhs          
     . "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
    (intersection  
     . "Have a look at (intersection common-lisp emacs-lisp scheme) http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/intersection-r5rs-common-lisp-emacs-lisp/")
    (scheme-or-cl  
     . "CL vs. Scheme http://irreal.org/blog/?p=813")
    (cliki         
     . "Have a look at http://cliki.net/ ; start with http://www.cliki.net/Getting%20Started")
    (newbie
     . "http://cliki.net/Getting%20Started or http://articulate-lisp.com/ ")
    (getting-started 
     . "Start with http://www.cliki.net/Getting%20Started  or  http://articulate-lisp.com/" )
    (emacs-lisp-intro
     . "An Introduction to Programming in Emacs Lisp  http://www.gnu.org/software/emacs/emacs-lisp-intro/  or  M-: (info \"(eintr)Top\") RET (for non-programmers)")
    (emacs-lisp      
     . "Emacs Lisp Manual http://www.gnu.org/software/emacs/manual/elisp.html  or  M-: (info \"(elisp)Top\") RET")
    (emacs-manual    
     . "Emacs Manual http://www.gnu.org/software/emacs/manual/   or  M-: (info \"(emacs)Top\") RET")
    (the-art-of-unix-programming    
     . "The Art of Unix Programming http://www.faqs.org/docs/artu/")
    (hacker-howto    
     . "http://www.catb.org/~esr/faqs/hacker-howto.html")
    (the-craft-of-text-editing    
     . "The Craft of Text Editing   http://www.finseth.com/craft/")
    (essentials-of-programming-languages     
     . "Essentials of Programming Languages, 3rd ed.   Daniel P. Friedman and Mitchell Wand   ISBN: 978-0-262-06279-4   http://MITPress.MIT.Edu/0262062798/  http://WWW.EoPL3.Com/")
    (practical-common-lisp      
     . "Practical Common Lisp http://www.gigamonkeys.com/book/")
    (common-lisp-a-gentle-introduction-to-symbolic-computation   
     . "Common Lisp: A Gentle Introduction to Symbolic Computation  http://www.cs.cmu.edu/~dst/LispBook/  http://www-cgi.cs.cmu.edu/afs/cs.cmu.edu/user/dst/www/LispBook/index.html")
    (common-lisp-programming-for-artificial-intelligence   
     . "Common Lisp Programming for Artificial Intelligence  Tony Hasemer & John Domingue - 1989  International Computer Science Series  Addison & Wesley  ISBN 0-201-17579-7")
    (common-lisp-an-interactive-approach    
     . "Common Lisp: An Interactive Approach  by Stuart C. Shapiro   http://www.cse.buffalo.edu/~shapiro/Commonlisp/")
    (paradigms-of-artificial-intellgience     
     . "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp")
    (artifical-intelligence-a-modern-approach     
     . "Artificial Intelligence: A Modern Approach  http://aima.cs.berkeley.edu")
    (sicp     
     . "Structure and Interpretation of Computer Programs  http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html  http://swiss.csail.mit.edu/classes/6.001/abelson-sussman-lectures/")
    (sicp-mit 
     . "http://web.mit.edu/alexmv/6.S184/")
    (6.S184   
     . "http://web.mit.edu/alexmv/6.S184/")
    ;; http://www.codepoetics.com/wiki/index.php?title=Topics:SICP_in_other_languages
    ;; http://eli.thegreenplace.net/category/programming/lisp/sicp/
    ;; http://www.neilvandyke.org/sicp-plt/
    ;; http://www.youtube.com/watch?v=rdj6deraQ6k
    (r5rs     
     . "http://www.schemers.org/Documents/Standards/R5RS/HTML/")
    (how-to-design-programs     
     . "How to Design Programs -- An Introduction to Computing and Programming  http://www.htdp.org/2003-09-26/Book/  ")
    (concrete-abstraction       
     . "Concrete Abstractions -- An Introduction to Computer Science Using Scheme  http://www.gustavus.edu/+max/concrete-abstractions.html")
    (lisp-in-small-pieces     
     . "Lisp in Small Pieces   http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html  http://pagesperso-systeme.lip6.fr/Christian.Queinnec/Books/LiSP-2ndEdition-2006Dec11.tgz")
    (on-lisp   
     . "On Lisp  Paul Graham   http://www.paulgraham.com/onlisptext.html  http://www.bookshelf.jp/texi/onlisp/onlisp.html  http://www.bookshelf.jp/texi/onlisp/onlisp.tar.gz")
    (compiler-principle-techniques-and-tools     
     . "Compiler Principles Techniques and Tools, Aho et al. http://dragonbook.stanford.edu/")
    (the-art-of-computer-programming    
     . "The Art of Computer Programming  Donald E. Knuth  Addison & Wesley")
    (goedel-escher-bach      
     . "Gödel, Escher, Bach: An Eternal Golden Braid  Douglas Hofstadter")
    (basic-lisp-technique      
     . "Basic Lisp Techniques  Cooper - 2003 Franz, Inc. - 100 pages.  http://www.franz.com/resources/educational_resources/cooper.book.pdf")
    (casting-speels-in-lisp  
     . "Casting Spels in Lisp  Conrad Barski, M.D.  http://www.lisperati.com/casting.html")
    (floating-point
     . "What Every Computer Scientist Should Know About Floating-Point Arithmetic http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html   and   What Every Programmer Should Know About Floating-Point Arithmetic http://floating-point-gui.de/") 
    ;; --
    (gitlab-lisp 
     . "https://gitlab.com/com-informatimago/com-informatimago")
    (gitlab-emacs
     . "https://gitlab.com/com-informatimago/emacs")
    (rc       
     . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/rc")
    (bin      
     . "http://git.informatimago.com/viewgit/index.php?a=summary&p=public/bin")
    (idiots   
     . "There, there, we know there are idiots on the Internet.  Lisp will make it all better.")
    (maintained-illustration
     . "http://tinyurl.com/last-commit-six-month-ago http://tinyurl.com/monthly-commits http://tinyurl.com/last-commit-yesterday http://tinyurl.com/last-commit-before-VCS-existed")
    (ibcl
     . "Image Based Development http://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/index.html")
    ;; --
    (see-defpackage
     . ";;;;    See defpackage documentation string.\n")
    (agpl3         
     . "
License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 1994 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <a href=\\\"http://www.gnu.org/licenses/\\\">http://www.gnu.org/licenses/</a>.

")))


(defun pjb-erc-get-answers ()
  (mapcar (function car) *pjb-erc-answers*))

(defvar *pjb-erc-last-answer* nil)

(defun pjb-erc-answer (key)
  (interactive (list 
                (intern (completing-read 
                         "What? " (mapcar (lambda (x) (cons x nil)) (pjb-erc-get-answers))
                         (lambda (answer) (setq *pjb-erc-last-answer* (car answer)))
                         t))))
  (insert (format "%s" (cdr (assoc key *pjb-erc-answers*)))))


;;;---------------------------------------------------------------------
;;; Miscellaneous
;;;---------------------------------------------------------------------

(defun pjb-server-available-p (server)
  (let ((result (shell-command-to-string
                 (format "ping -c 10 -i 0.5 -q -w 5 %s"
                         (shell-quote-argument (cl:string server))))))
    (when (string-match " \\([0-9]+\\)% packet loss" result)
      (let ((loss (nth-value 0 (cl:parse-integer (match-string 1 result)))))
        (if (< loss 80)
            (progn
              (message "pjb-erc: (server-available-p %S) -> %d (true)"
                       server loss)
              loss)
            (progn
              (message "pjb-erc: (server-available-p %S) -> %d (nil)"
                       server loss)
              nil))))))


(defun pjb-erc-add-fools (&rest fools)
  (dolist (fool fools)
    ;;  (pushnew fool erc-fools :test (function STRING-EQUAL))
    (unless (member fool erc-ignore-list)
      (erc-cmd-IGNORE fool))))


(defun pjb-erc-join-channels (&rest channels)
  (dolist (chan channels)
    (erc-join-channel chan)
    (split-window-vertically)
    ;; we must balance to avoid 2^-n window size.
    (balance-windows)))

(defun pjb-erc-adjust-width (&optional frame)
  "Set the erc-fill-column to the minimum width of the windows
displaying erc buffers."
  (interactive)
  (setf erc-fill-column
        (or (let ((frame (or frame (current-frame))))
              (loop
                 with width = nil
                 with buffers = (erc-all-buffer-names)
                 for window in (window-list frame nil)
                 do (when (or (member (window-buffer window) buffers)
                              (null width)
                              (< (window-width window) width))
                      (setf width (window-width window)))
                 finally (return width))) 72)))

(defvar *erc-cancel-auto-adjust-width* nil)
  
(defun pjb-erc-auto-adjust-width ()
  (interactive)
  (if  *erc-cancel-auto-adjust-width*
       (setf  *erc-cancel-auto-adjust-width* nil)
       (progn (pjb-erc-adjust-width)
              (run-with-idle-timer 10 10 (function erc-auto-adjust-width)))))

(defun pjb-erc-cancel-auto-adjust-width ()
  (interactive)
  (setf *erc-cancel-auto-adjust-width* t))


  
(defun pjb-erc-user-input ()
  "Return the input of the user in the current buffer."
  ;; We use erc-bol to ignore the prompt.
  (string-trim "\n" (buffer-substring
                     (progn
                       (goto-char (erc-beg-of-input-line))
                       (erc-bol))
                     (erc-end-of-input-line))))


(defun pjb-erc-send-bitlbee-password (password)
  (erc-send-command  (format "PRIVMSG &bitlbee :identify %s" password)))


(defun nsplit-list-on-indicator (list indicator)
  "
RETURN: a list of sublists of list (the conses from list are reused),
        the list is splited between items a and b for which (indicator a b).
"
  (let* ((result nil)
         (sublist list)
         (current list)
         (next    (cdr current)))
    (loop while next do
         (if (funcall indicator (car current) (car next))
             (progn ;; split
               (setf (cdr current) nil)
               (push sublist result)
               (setq current next)
               (setq next (cdr current))
               (setq sublist current))
             (progn ;; keep
               (setq current next)
               (setq next (cdr current)))))
    (push sublist result)
    (nreverse result)))


(unless (fboundp (quote netrc-parse))
  (defalias 'netrc-point-at-eol
      (if (fboundp 'point-at-eol)
          'point-at-eol
          'line-end-position))
  (defun netrc-parse (file)
    "Parse FILE and return a list of all entries in the file."
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((tokens '("machine" "default" "login"
                        "password" "account" "macdef" "force"
                        "port"))
              alist elem result pair)
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Go through the file, line by line.
          (while (not (eobp))
            (narrow-to-region (point) (netrc-point-at-eol))
            ;; For each line, get the tokens and values.
            (while (not (eobp))
              (skip-chars-forward "\t ")
              ;; Skip lines that begin with a "#".
              (if (eq (char-after) ?#)
                  (goto-char (point-max))
                  (unless (eobp)
                    (setq elem
                          (if (= (following-char) ?\")
                              (read (current-buffer))
                              (buffer-substring
                               (point) (progn (skip-chars-forward "^\t ")
                                              (point)))))
                    (cond
                      ((equal elem "macdef")
                       ;; We skip past the macro definition.
                       (widen)
                       (while (and (zerop (forward-line 1))
                                   (looking-at "$")))
                       (narrow-to-region (point) (point)))
                      ((member elem tokens)
                       ;; Tokens that don't have a following value are ignored,
                       ;; except "default".
                       (when (and pair (or (cdr pair)
                                           (equal (car pair) "default")))
                         (push pair alist))
                       (setq pair (list elem)))
                      (t
                       ;; Values that haven't got a preceding token are ignored.
                       (when pair
                         (setcdr pair elem)
                         (push pair alist)
                         (setq pair nil)))))))
            (when alist
              (push (nreverse alist) result))
            (setq alist nil
                  pair nil)
            (widen)
            (forward-line 1))
          (nreverse result))))))


(defvar server (if (boundp 'erc-server) erc-server "irc.freenode.org")
  "For get-password")
(defvar nick   (first erc-nick)
  "For get-password")
(defun get-password (server nick)
  (cdr (assoc "password"
              (car
               (delete-if
                (lambda (entry)
                  (or (cl:string/= (cdr (assoc "machine" entry)) server)
                      (cl:string/= (cdr (assoc "login"   entry)) nick)))
                (nsplit-list-on-indicator
                 (reduce 'nconc (netrc-parse "~/.netrc"))
                 (lambda (current next) (string= "machine" (car next)))))))))

(defun pjb-set-erc-nickserv-passwords ()
  (setf erc-nickserv-passwords
        (mapcar (lambda (nickserv)
                  (list (second nickserv)
                        (mapcar
                         (lambda (entry)
                           (cons (cdr (assoc "login" entry))
                                 (cdr (assoc "password" entry))))
                         (delete-if
                          (lambda (entry)
                            (cl:string/= (cdr (assoc "machine" entry))
                                         (second nickserv)))
                          (nsplit-list-on-indicator
                           (reduce 'nconc (netrc-parse "~/.netrc"))
                           (lambda (current next) (string= "machine" (car next))))))))
                '((freenode "irc.freenode.org")))))



(defun pjb-pjb-erc-unhtmlize ()
  (message ".EMACS: %S" (buffer-substring-no-properties (point-min) (point-max)))
  (let ((s (make-marker)) (e (make-marker)) (c (make-marker)))
    (set-marker c (point))
    (set-marker s (point-min))
    (set-marker e (point-max))
    (unwind-protect
         (progn
           (goto-char (point-min))
           (when (search-forward " ")
             (goto-char (match-end 0))
             (narrow-to-region (point) (point-max))
             (when (and (< 6 (- (point-max) (point-min)))
                        (cl:string-equal "<HTML>" (buffer-substring-no-properties
                                                   (point-min) (+ 6 (point-min)))
                                         :end2 6))
               (shell-command-on-region
                (point-min) (point-max) "lynx -stdin -dump" t t)
               (goto-char (point-min))
               (while (re-search-forward "[ \n]\+" (point-max) t)
                 (delete-region (match-beginning 0)  (match-end 0))
                 (insert " "))
               (insert "\n"))))
      (widen)
      (narrow-to-region s e)
      (goto-char c)
      (set-marker c nil)
      (set-marker s nil)
      (set-marker e nil))))


(defun pjb-erc-timestamp-offset ()
  "Get length of timestamp if inserted left."
  (if (and (boundp 'erc-timestamp-format)
           erc-timestamp-format
           (eq erc-insert-timestamp-function 'erc-insert-timestamp-left)
           (not erc-hide-timestamps))
      (length (car (last (split-string
                          (format-time-string erc-timestamp-format) "\n"))))
      0))

(defun pjb-erc-fill-regarding-timestamp ()
  "Do nothing"
  (values))


(defun pjb-erc-insert-timestamp-left (string)
  "Insert timestamps at the beginning of the line."
  (goto-char (point-min))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
                        (string-equal string erc-timestamp-last-inserted)))
         (len (length  (car (last (split-string string "\n")))))
         (s (if ignore-p (make-string len ? ) string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (erc-put-text-property 0 len 'field 'erc-timestamp s)
    (insert s)))

(defun pjb-erc-insert-timestamp-right (string)
  "Insert timestamp on the right side of the screen.
STRING is the timestamp to insert.  The function is a possible value
for `erc-insert-timestamp-function'.

If `erc-timestamp-only-if-changed-flag' is nil, a timestamp is always
printed.  If this variable is non-nil, a timestamp is only printed if
it is different from the last.

If `erc-timestamp-right-column' is set, its value will be used as the
column at which the timestamp is to be printed.  If it is nil, and
`erc-fill-mode' is active, then the timestamp will be printed just
before `erc-fill-column'.  Otherwise, if the current buffer is
shown in a window, that window's width is used.  If the buffer is
not shown, and `fill-column' is set, then the timestamp will be
printed just `fill-column'.  As a last resort, the timestamp will
be printed just before the window-width."
  (unless (and erc-timestamp-only-if-changed-flag
               (string-equal string erc-timestamp-last-inserted))
    (setq erc-timestamp-last-inserted string)
    (goto-char (point-max))
    (forward-char -1) ;; before the last newline
    (let ((current-window (get-buffer-window (current-buffer))))
      (flet ((get-pos (string)
               (cond
                 (erc-timestamp-right-column
                  (+ erc-timestamp-right-column (length string)))
                 ((and (boundp 'erc-fill-mode)
                       erc-fill-mode
                       (boundp 'erc-fill-column))
                  (1+ erc-fill-column))
                 (current-window
                  (- (window-width current-window)
                     1))
                 (fill-column
                  (1+ fill-column))
                 (t
                  (- (window-width)
                     1)))))
        (let* ((prefix-lines (butlast (split-string string "\n")))
               (string (car (last (split-string string "\n"))))
               (pos (get-pos string))
               (from (point)))
          (setq pos (- pos (length string)))
          (if (= pos (indent-to pos))
              (insert string)
              (newline)
              (indent-to pos)
              (insert string))
          (erc-put-text-property from (1+ (point)) 'field 'erc-timestamp)
          (erc-put-text-property from (1+ (point)) 'rear-nonsticky t)
          (when prefix-lines
            (forward-line -1)
            (setf from (point))
            (dolist (line prefix-lines)
              (if (string= "" line)
                  (insert "n")
                  (insert (format "%s%s\n"
                                  (make-string (- (get-pos line) (length line)) 32)
                                  line))))
            (erc-put-text-property from (1+ (point)) 'field 'erc-timestamp)
            (erc-put-text-property from (1+ (point)) 'rear-nonsticky t)
            (setf from (point))))))))

  
;;;---------------------------------------------------------------------
;;; erc/irc emacs commands
;;;---------------------------------------------------------------------

(defun pjb-erc ()
  (interactive)
  ;; --------------------
  (erc-stamp-enable)                    ; == erc-timestamp-mode
  (erc-show-timestamps)
  (erc-match-mode 1)
  ;;     (erc-add-scroll-to-bottom)
  ;;     (setf erc-insert-modify-hook
  ;;           (cons 'erc-add-timestamp      ; must come before the rest.
  ;;                 (delete 'erc-add-timestamp erc-insert-modify-hook)))
  ;; --------------------
  (when t                ; (server-available-p "zelazny.freenode.org")
    (erc :server "irc.freenode.org" :port 6667
         :nick "pjb" :full-name "Pascal J. Bourguignon")
    (pjb-erc-join-channels
     ;;"#cl-gardeners"
     "#lisp" "#emacs"
     ;; "#ucw"
     ;;"#emacsfr"
     ;;"#sicp"
     "#scheme")
    ;; (pjb-erc-add-fools "Xach")
      
    (dolist (fool '("NP" "Xah" "xah" "xahlee"  "xahlee_" "xahlee__"))
      (pjb-erc-add-fools fool)))
  ;; --------------------
  (when nil                      ; (server-available-p "irc.oftc.net")
    (erc "irc.oftc.net" 6667 "pjb"      "Pascal J. Bourguignon" t)
    (pjb-erc-join-channels
     "#ubuntu" "#uml"))
  ;; --------------------
  (when t                      ; (server-available-p "im.bitlbee.org")
    (let ((server  "im.bitlbee.org")
          (nick     "matimago"))
      (erc server 6667 nick "Pascal J. Bourguignon" t)
      (pjb-erc-join-channels "&bitlbee")
      (when  (get-password server nick)
        (pjb-erc-send-bitlbee-password (get-password server nick)))))
  ;; --------------------
  (erc-auto-adjust-width)
  (balance-windows))
  

(defun irc ()
  (interactive)
  (pjb-server-stop)
  (set-frame-name "ERC")
  (set-palette pal-irc)
  (setf *frame-server-job-ticket* "~/frame-erc")
  ;; --------------------
  (erc-stamp-enable)                    ; == erc-timestamp-mode
  (erc-show-timestamps)
  (erc-match-mode 1)
  ;;     (erc-add-scroll-to-bottom)
  ;;     (setf erc-insert-modify-hook
  ;;           (cons 'erc-add-timestamp      ; must come before the rest.
  ;;                 (delete 'erc-add-timestamp erc-insert-modify-hook)))
  ;; --------------------
  (when t                ; (server-available-p "zelazny.freenode.org")
    (erc "irc.freenode.org" 6667 "pjb"      "Pascal J. Bourguignon" t)
    (pjb-erc-join-channels
     ;;"#cl-gardeners"
     "#lisp" "#emacs"
     ;; "#ucw"
     ;;"#emacsfr"
     ;;"#sicp"
     "#scheme")
    ;; (pjb-erc-add-fools "Xach")
      
    (dolist (fool '("NP" "Xah" "xah" "xahlee"  "xahlee_" "xahlee__"))
      (pjb-erc-add-fools fool)))
  ;; --------------------
  (when nil                      ; (server-available-p "irc.oftc.net")
    (erc "irc.oftc.net" 6667 "pjb"      "Pascal J. Bourguignon" t)
    (pjb-erc-join-channels
     "#ubuntu" "#uml"))
  ;; --------------------
  (when nil                    ; (server-available-p "im.bitlbee.org")
    (let ((server  "im.bitlbee.org")
          (nick     "matimago"))
      (erc server 6667 nick "Pascal J. Bourguignon" t)
      (pjb-erc-join-channels "&bitlbee")
      (when  (get-password server nick)
        (pjb-erc-send-bitlbee-password (get-password server nick)))))
  ;; --------------------
  ;; (erc-auto-adjust-width)
  (balance-windows))


;;;---------------------------------------------------------------------
;;; IRC commands
;;;---------------------------------------------------------------------

;; Not needed anymore: bitlbee can convert encoding all right.
;;
;;   (defun erc-coding-system-for-target (target)
;;     "Return the coding system or cons cell appropriate for TARGET.
;; This is determined via `erc-encoding-coding-alist' or
;; `erc-default-coding-system'."
;;     ;;(print `(erc-coding-system-for-target ,target))
;;     (block :result
;;       (when target
;;         (dolist (coding  erc-encoding-coding-alist)
;;           (if (string-match (car coding) target)
;;             (return-from :result (cons (cdr coding) (cdr coding))))))
;;       erc-default-coding-system))


(defun pjb-erc-match-directed-at-fool-p (msg)
  "Check wether MSG is directed at a fool."
  (with-syntax-table erc-button-syntax-table
    (erc-list-match (mapcar (lambda (fool)
                              (concat "\\b" fool "\\b"))
                            erc-fools)
                    msg)))

(defun pjb-erc-cmd-ICQWHOIS (uin)
  "Queries icq-user with UIN `uin', and returns the result."
  (let* ((result (myerc-query-icq-user uin))
         (fname (cdr (assoc 'fname result)))
         (lname (cdr (assoc 'lname result)))
         (nick (cdr (assoc 'nick result))))
    (erc-display-message nil 'notice (current-buffer)
                         (format "%s (%s %s)" nick fname lname))))
  
(defun pjb-erc-cmd-CLEAR ()
  (interactive)
  ;;(save-buffer)
  (save-excursion
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-region 1 (point)))))

;; (fset 'ClearERC [?\C-p ?\C-e ?\C-  ?\M-< ?\C-w ?\M->])
;; (global-set-key (kbd "C-c C-x C-c") 'ClearERC)


(defun pjb-erc-cmd-QUIT (reason)
  "Disconnect from the current server.
If REASON is omitted, display a default quit message, otherwise display
the message given by REASON."
  (unless reason
    (setf reason (read-string "Reason: " "Awaking to the real world.")))
  (cond
    ((string-match "^\\s-*\\(.*\\)$" reason)
     (let* ((s (match-string 1 reason))
            (ob (current-buffer))
            (buffer (erc-server-buffer))
            (reason (funcall erc-quit-reason (if (equal s "") nil s))))
       (if (and buffer (bufferp buffer)) (set-buffer buffer))
       (erc-log (format "cmd: QUIT: %s" reason))
       (setq quitting t)
       (erc-send-command (format "QUIT :%s" reason))
       (set-buffer ob)
       (run-hook-with-args 'erc-quit-hook erc-process)
       (when erc-kill-queries-on-quit
         (erc-kill-query-buffers erc-process)))
     t)
    (t nil)))


;;;---------------------------------------------------------------------
;;; erc hooks
;;;---------------------------------------------------------------------

(defun pjb/erc-insert-post-meat ()
  (interactive)
  (reset-movement-keypad)
  (setf erc-insert-timestamp-function 'pjb/erc-insert-timestamp-left
        erc-fill-function 'pjb/erc-fill-static)
  (remove-hook 'erc-insert-modify-hook 'erc-unmorse))
(add-hook 'erc-insert-post-hook 'pjb/erc-insert-post-meat)


(defun cl-eval-last-expression ()
  (interactive)
  (let ((current-prefix-arg t)
        (expression (slime-last-expression)))
    (push-mark) 
    (slime-eval-async `(swank:eval-and-grab-output-and-error ,expression)
                      (lambda (result)
                        (destructuring-bind (output values) result
                          (insert (if (zerop (length output)) " #|" " #| ")
                                  output)
                          (when (plusp (length values))
                            (insert
                             " --> "
                             (replace-regexp-in-string "\n" " ; " values t t)))
                          (insert " |# "))))))

(defun el-eval-last-expression ()
  (interactive)
  (let ((pt (point))
        (current-prefix-arg t))
    (set-mark pt)
    (eval-last-sexp t)
    (save-excursion
     (goto-char pt)
     (insert " --> "))))

(defun scheme-eval-last-expression ()
  (interactive)
  (let ((pt (point))
        (current-prefix-arg t))
    (set-mark pt)
    (lisp-eval-last-sexp t)
    (save-excursion
     (goto-char pt)
     (insert " --> "))))

(defun pjb/erc-join-meat ()
  (let ((buffer (pjb-erc-buffer-channel (current-buffer))))
    (unless (and buffer (char= (character "#") (aref buffer 0)))
     (erc-log-mode 1)
     (local-set-key (kbd "C-y") 'pjb-erc-yank)
     (local-set-key (kbd "H-a") 'pjb-erc-answer)))
  (loop with current-channel = (buffer-name)
        for (channels . eval-function)
          in '((("#lisp" "##lisp" "#lispcafe" "#lispgame"  "#lisp-lab" "#lisp-fr" "#lisp-es" 
                 "#ccl" "#sbcl" "#quicklisp")
                . cl-eval-last-expression)
               (("#emacs")
                . el-eval-last-expression)
               (("#scheme")
                . scheme-eval-last-expression))
        when (member* current-channel channels :test (function string=))
          do (local-set-key (kbd "C-x C-e") eval-function)))

(add-hook 'erc-join-hook 'pjb/erc-join-meat)
(mapcar (lambda (buffer) (with-current-buffer buffer (pjb/erc-join-meat))) (buffer-list))
(add-hook 'erc-mode-hook 'abbrev-mode)

;;;---------------------------------------------------------------------

;; (pjb-set-erc-nickserv-passwords)
;; (setf erc-timestamp-format "%Y-%m-%d %H:%M\n")
;; (erc-match-mode 1)

(provide 'pjb-erc)
