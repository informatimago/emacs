;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-blink.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Alternate blinking parens.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2011
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
;;;;****************************************************************************


(defun blink (start end)
  (set-mark start)
  (goto-char (1+ end)))

(defun unblink (start end)
  "Nothing to do")

;; Another implementation of blink and unblink could set the face,
;; or put some overlay on the region or on the start and end characters.


(defun blink-matching-open-and-close ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (interactive)
  (let ((close-point (1- (point))))
    (and (> (point) (1+ (point-min)))
         blink-matching-paren
         ;; Verify an even number of quoting characters precede the close.
         (= 1 (logand 1 (- (point)
                           (save-excursion
                             (forward-char -1)
                             (skip-syntax-backward "/\\")
                             (point)))))
         (let* ((oldpos (point))
                (blinkpos)
                (mismatch)
                matching-paren)
           (save-excursion
             (save-restriction
               (if blink-matching-paren-distance
                   (narrow-to-region (max (point-min)
                                          (- (point) blink-matching-paren-distance))
                                     oldpos))
               (condition-case ()
                   (let ((parse-sexp-ignore-comments
                          (and parse-sexp-ignore-comments
                               (not blink-matching-paren-dont-ignore-comments))))
                     (setq blinkpos (scan-sexps oldpos -1)))
                 (error nil)))
             (and blinkpos
                  ;; Not syntax '$'.
                  (not (eq (syntax-class (syntax-after blinkpos)) 8))
                  (setq matching-paren
                        (let ((syntax (syntax-after blinkpos)))
                          (and (consp syntax)
                               (eq (syntax-class syntax) 4)
                               (cdr syntax)))
                        mismatch
                        (or (null matching-paren)
                            (/= (char-after (1- oldpos))
                                matching-paren))))
             (if mismatch (setq blinkpos nil))
             (if blinkpos
                 ;; Don't log messages about paren matching.
                 (let (message-log-max)
                   (blink blinkpos close-point)
                   (if (pos-visible-in-window-p)
                       (and blink-matching-paren-on-screen
                            (sit-for blink-matching-delay))
                       (goto-char blinkpos)
                       (message
                        "Matches %s"
                        ;; Show what precedes the open in its line, if anything.
                        (if (save-excursion
                              (skip-chars-backward " \t")
                              (not (bolp)))
                            (buffer-substring (progn (beginning-of-line) (point))
                                              (1+ blinkpos))
                            ;; Show what follows the open in its line, if anything.
                            (if (save-excursion
                                  (forward-char 1)
                                  (skip-chars-forward " \t")
                                  (not (eolp)))
                                (buffer-substring blinkpos
                                                  (progn (end-of-line) (point)))
                                ;; Otherwise show the previous nonblank line,
                                ;; if there is one.
                                (if (save-excursion
                                      (skip-chars-backward "\n \t")
                                      (not (bobp)))
                                    (concat
                                     (buffer-substring (progn
                                                         (skip-chars-backward "\n \t")
                                                         (beginning-of-line)
                                                         (point))
                                                       (progn (end-of-line)
                                                              (skip-chars-backward " \t")
                                                              (point)))
                                     ;; Replace the newline and other whitespace with `...'.
                                     "..."
                                     (buffer-substring blinkpos (1+ blinkpos)))
                                    ;; There is nothing to show except the char itself.
                                    (buffer-substring blinkpos (1+ blinkpos)))))))
                   (unblink blinkpos close-point))
                 (cond (mismatch
                        (message "Mismatched parentheses"))
                       ((not blink-matching-paren-distance)
                        (message "Unmatched parenthesis")))))))))

(setf blink-paren-function (function  blink-matching-open-and-close))
