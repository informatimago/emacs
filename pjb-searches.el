;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-searches.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Web and grep searchers.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-20 <PJB> Extracted from ~/rc/emacs-common.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

;; (require 'google)
;; (setq google-license-key "dF18sc1QFHLPxvBVqwv/WxCbYR18GHbp")
;; ;; Then M-x google-search RET
;; ;; or M-x google-search-region RET
;; (defalias 'url-retrieve-synchronously 'url-retrieve)

(defun %search-region (start end thing search-function)
  (when start
    (cond
      ((null end)
       (let ((bounds (bounds-of-thing-at-point thing)))
         (if bounds
             (%search-region (car bounds) (cdr bounds) thing search-function)
             (call-interactively search-function))))
      ((= start end)
       (call-interactively search-function))
      (t
       (funcall search-function (buffer-substring-no-properties start end))))))

;; (if (or (not mark-active) (eql (point) (mark)))
;;     "string"
;;     (buffer-substring-no-properties (min (point) (mark))
;;                                     (max (point) (mark))))


(defparameter *whitespaces* '(32 9 10 13))

(defmacro with-browser-for-apple-documentation (&rest body)
  `(let ((browse-url-browser-function (if (and (eq window-system 'ns)
                                              (eq system-type 'darwin))
                                         'browse-url-generic
                                         'browse-url-firefox2)))
    ,@body))

(defun osx-search (search-string)
  "Search a string with Apple."
  (interactive "sApple Developer Documentation Search: ")
  (with-browser-for-apple-documentation
   (browse-url
    (format "https://developer.apple.com/library/mac/search/?q=%s"
            (browse-url-url-encode-chars
             (cl:string-trim *whitespaces* search-string)
             "[^A-Za-z0-9]")))))

(defun osx-search-region (start end)
  "Search the text in the region with Apple."
  (interactive "r")
  (%search-region start end 'symbol 'osx-search))

;; (debug-on-entry 'browse-url)
(defun ios-search (search-string)
  "Search a string with Apple."
  (interactive "sApple Developer Documentation Search: ")
  (with-browser-for-apple-documentation
      (browse-url
       (format "https://developer.apple.com/library/ios/search/?q=%s"
               (browse-url-url-encode-chars
                (cl:string-trim *whitespaces* search-string)
                "[^A-Za-z0-9]")))))

(defun ios-search-region (start end)
  "Search the text in the region with Apple."
  (interactive "r")
  (%search-region start end 'symbol 'ios-search))


(defun android-search (search-string)
  "Search a string with Android."
  (interactive "sAndroid Developer Documentation Search: ")
  (browse-url (or (when (and (search "." search-string) (not (search ".." search-string)))
                    (let ((words (split-string search-string "\\.")))
                      (when (and (<= 3 (length words))
                                 (every (lambda (word)
                                          (and (alpha-char-p (aref word 0))
                                               (every (function alphanumericp) word)))

                                        words))
                        (format "http://developer.android.com/reference/%s.html"
                                (mapconcat (function identity) words "/")))))
                  (format "http://developer.android.com/reference/index.html?q=%s"
                          (browse-url-url-encode-chars
                           (cl:string-trim *whitespaces* search-string)
                           "[^A-Za-z0-9]")))))

(defun android-search-region (start end)
  "Search the text in the region with Android."
  (interactive "r")
  (%search-region start end 'symbol 'android-search))

(defun project-search (search-string)
  "Search a regex in the current project (with `find-grep' and `grep-find-command')."
  (interactive "sSearch Project Regexp: ")
  (find-grep (concat grep-find-command " " (shell-quote-argument search-string))))

(defun project-search-region (start end)
  "Search the text in the region in the current project (with `find-grep' and `grep-find-command')."
  (interactive "r")
  (%search-region start end 'symbol 'project-search))


(defun google-search (search-string)
  "Search a string with Google."
  (interactive "sGoogle Search: ")
  (browse-url
   (format "http://www.google.com/search?as_q=%s&num=50&hl=en&ie=ISO8869-1&btnG=Google+Search&as_epq=&as_oq=&as_eq=&lr=&as_ft=i&as_filetype=&as_qdr=all&as_nlo=&as_nhi=&as_occt=any&as_dt=i&as_s
itesearch=&safe=images"
           (browse-url-url-encode-chars
            (cl:string-trim *whitespaces* search-string)
            "[^A-Za-z0-9]"))))

(defun google-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'google-search))


(defparameter *acronym-search-url* "http://www.acronymfinder.com/%s.html")
;;  "http://www.cygwin.com/acronyms/#%s"
(defun acronym-search (acronym-string)
  (interactive "sAcronym Search: ")
  (browse-url (format *acronym-search-url* acronym-string)))

(defun acronym-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'acronym-search))



(defun includes-search (string)
  (interactive "sIncludes Search: ")
  (find-grep (format "find /usr/include/ /usr/local/include/ -type f -exec grep -n -i %s {} /dev/null \\; #" (shell-quote-argument string))))

(defun includes-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'includes-search))

(defalias 'grep-includes 'includes-search)


(defun hyperspec-search (string)
  (interactive "sHyperspec Search: ")
  (find-grep (format "find '%s' -type f -print|while read f ; do lynx -dump -nolist \"$f\" | grep -i '%s' && echo \"$f:1:-\" ; done #" (shell-quote-argument *hyperspec-path*) string)))

(defun hyperspec-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'hyperspec-search))

(defalias 'grep-hyperspec 'hyperspec-search)


(defun here-search (pattern)
  "Does an egrep  in the current directory just asking for a pattern."
  (interactive (list (read-from-minibuffer (format "In %s egrep pattern: " (shell-quote-argument default-directory)))))
  (check-type pattern string)
  (if (string-equal "" pattern)
      (error "The empty string matches everything. Are you happy?")
      (grep (format "egrep -n -e '%s' `find . -type f -print` /dev/null" pattern))))

(defun here-search-region (start end)
  (interactive "r")
  (%search-region start end 'symbol 'here-search))


(global-set-key (kbd "C-h 0")
                (lambda ()
                  (interactive)
                  (message (format "C-h 1 %s  C-h 2 google  C-h 3 acronym  C-h 4 project  C-h 5 includes  C-h 6 hyperspec  C-h 7 this directory"
                                   (let* ((search (format "%s" (local-key-binding (kbd "C-h 1") t)))
                                          (dash   (search "-" search)))
                                     (if dash
                                         (subseq search 0 dash)
                                         search))))))

;;(global-set-key (kbd "C-h 1") 'android-search-region)
;;(global-set-key (kbd "C-h 1") 'osx-search-region)
(global-set-key (kbd "C-h 1") 'ios-search-region)
(global-set-key (kbd "C-h 2") 'google-search-region)
(global-set-key (kbd "C-h 3") 'acronym-search-region)
(global-set-key (kbd "C-h 4") 'project-search-region)
(global-set-key (kbd "C-h 5") 'includes-search-region)
(global-set-key (kbd "C-h 6") 'hyperspec-search-region)
(global-set-key (kbd "C-h 7") 'here-search-region)
(global-set-key (kbd "C-h 0") 'android-browse-documentation-of-class-at-point)

(defun set-osx-search-region-function ()
  (interactive)
  (local-set-key (kbd "C-h 1") 'osx-search-region))
(defun set-ios-search-region-function ()
  (interactive)
  (local-set-key (kbd "C-h 1") 'ios-search-region))
(defun set-android-search-region-function ()
  (interactive)
  (local-set-key (kbd "C-h 1") 'android-search-region)
  (local-set-key (kbd "C-h 0") 'android-browse-documentation-of-class-at-point))


(add-hook 'objc-mode-hook 'set-osx-search-region-function)
(add-hook 'objc-mode-hook 'set-ios-search-region-function)
(add-hook 'java-mode-hook 'set-android-search-region-function)

;;;; THE END ;;;;
