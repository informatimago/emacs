;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-work.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports function to insert timestamps when 
;;;;    starting and stoping work, and updating total lines.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    199?-??-?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2006
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
;;;;****************************************************************************
(require 'pjb-utilities)
(require 'pjb-cl)
(provide 'pjb-work)

(defun duree (a b) 
  (let ((da (dms-d a)) (db (dms-d b)))
    (d-dms (+ (if (<= da db) 0 24) (- db da)))))

(defun duree-d (a b)
  (let ((da (dms-d a)) (db (dms-d b)))
    (+ (if (<= da db) 0 24) (- db da))))


(defun pjb-work-begin ()
  "Inserts the date and time."
  (interactive)
  (beginning-of-line)
  (insert (format-time-string " %Y-%m-%d ( \"%H:%M:%S\" \"\" ) ; \n\n   - "))
  (end-of-line))


(defun pjb-work-end ()
  "Inserts the time and compute the duration."
  (interactive)
  (let ((now (format-time-string "%H:%M:%S"))
        (duration)
        (here (point))
        (mat))
    (search-backward-regexp "( \"\\(..:..:..\\)\" \\(\"\" *).*\\)\n")
    (setq mat (match-data))
    (setq duration (duree  (match-string-no-properties 1) now))
    (if (< (length duration) 8)
        (setq duration (concat "0" duration)))
    (store-match-data mat)
    (replace-match (format "%S ) ; %s" now duration) t t nil 2)
    (pjb-work-total)))






(defvar *pjb-work-duration-regexp*
  "( \"\\(..:..:..\\)\" \"\\(..:..:..\\)\" )\\(.*\\)\n")


(defun pjb-work-duration ()
  "Compute the duration of the first occurence of '( \"HH:MM:SS\" \"HH:MM:SS\" )'
found backward from the point."
  (interactive)
  (search-backward-regexp  *pjb-work-duration-regexp*)
  (let ((mat (match-data))
        (duration (duree (match-string-no-properties 1) 
                         (match-string-no-properties 2))))
    (store-match-data mat)
    (replace-match (format " ; %s" (if (< (length duration) 8)
                                       (concat "0" duration)
                                       duration))
                   t t nil 3)
    (message "Use M-x pjb-work-total RET to update the totals.")))


(defun pjb-work-total ()
  "Compute the total works."
  (interactive)
  (let ((periods)
        (total 0)
        (totalstring)
        (amount)
        (hourly-rate)
        (p))
    ;; Update all the durations
    (goto-char (point-min))
    (while (re-search-forward *pjb-work-duration-regexp* nil t)
      (goto-char (match-end 0))
      (pjb-work-duration))
    ;; gather the hourly rate.
    (goto-char (point-min))
    (if  (re-search-forward "Hourly Rate *\\([^ ].*\\)\n" nil t)
         (let ((hrstr (match-string-no-properties 1)))
           (setq hourly-rate (string-to-number hrstr)))
         (setq hourly-rate 40.0))
    ;; gather periods.
    (goto-char (point-min))
    (while (re-search-forward "( \"\\(..:..:..\\)\" \"\\(..:..:..\\)\" )" nil t)
      (let ((start (match-string-no-properties 1))
            (stop  (match-string-no-properties 2)))
        (setq periods (append periods (list (list start stop))))))
    ;; compute total time.
    (setq p periods)
    (while p
      (setq total (+ total (duree-d (nth 0 (car p)) (nth 1 (car p))))
            p (cdr p)))
    ;; build the total string.
    (setq amount (* total hourly-rate))
    (setq totalstring 
          (concat
           (format " Total %30s %10s = %5.2f j   (%6.2f h)\n" "" (d-dms total) (/ total 8.0) total)
           (format " Facturation %34s %8.2f EUR HT\n" "" amount)
           (format " Hourly Rate %34s    %5.2f EUR HT/hour\n" "" hourly-rate)))
    ;; insert the total string.
    (goto-char (point-min))
    (if (search-forward-regexp
         " Total.*\n Facturation.*\n Hourly Rate.*\n" nil t)
        (replace-match totalstring)
        (if (search-forward-regexp (format " %s\n" (make-string 72 ?=)) nil t)
            (goto-char (match-end 0))
            (goto-char (point-max)))
        (insert totalstring))))



(defun intervention-file-path (firm project date)
  "
RETURN:  The file name formated from the firm, project and date.
"
  (format "%s/firms/%s/interventions/%s-%s.txt" 
    (getenv "HOME") firm date  project))


(defun get-firm-list ()
  "
RETURN: A list of the names of the directories directly in ~/firms/
        that have an intervention subdirectory that is accessible.
        (We skip names beginning with an underline or ending with a tilde).
"
  (mapcan 
   (lambda (path)
     (when (and (file-directory-p path)
                (not (string-match ".*/_[^/]*$\\|~$" path))
                (file-accessible-directory-p (format "%s/interventions" path)))
       (string-match ".*/\\([^/]*\\)$" path)
       (list (match-string 1 path))))
   (file-expand-wildcards (format "%s/firms/*" (getenv "HOME")) t)))



(defun get-project-list (firm)
  "
RETURN: A list of the names of projects initiated for the FIRM. 
        It is determined from the name of the files in the interventions 
        subdirectory of the firm.
"
  (delete-duplicates
   (mapcan
    (lambda (path)
      (when (string-match 
             ".*/\\([0-9][0-9][0-9][0-9][0-9]*\\)-\\([^-/][^-/]*\\).txt$" path)
        (list (match-string 2 path))))
    (file-expand-wildcards 
     (format "%s/firms/%s/interventions/[0-9]*-*.txt" (getenv "HOME") firm) t))
   :test (function string=*)))


(defun find-last-intervention (firm project)
  (car 
   (sort 
    (delete-duplicates
     (mapcan 
      (lambda (path)
        (when (string-match
               ".*/\\([0-9][0-9][0-9][0-9][0-9]*\\)-\\([^-/][^-/]*\\).txt$" path)
          (list (match-string 1 path))))
      (file-expand-wildcards 
       (format "%s/firms/%s/interventions/[0-9]*-%s.txt" 
         (getenv "HOME") firm project) t)))
    (function string>=*))))


(defvar *pjb-intervention-firm* nil)

(defun pjb-intervention (firm project)
  (interactive
   (list 
    ;; firms
    (completing-read 
     "Firm: " (mapcar (lambda (x) (cons x nil)) (get-firm-list))
     (lambda (firm) (setq *pjb-intervention-firm* (car firm)))  t)
    ;; projects:
    (completing-read
     "Project: " (mapcar (lambda (x) (cons x nil)) 
                         (get-project-list *pjb-intervention-firm*))
     (function identity) nil)))
  (let ((date)
        (new nil))
    (MULTIPLE-VALUE-BIND (s m h day month year) (GET-DECODED-TIME)
      (let* ((last-intervention (find-last-intervention firm project))
             (yearly (format "%04d" year))
             (monthly (format "%04d%02d" year month)))
        (setq new  (and (string/=* yearly last-intervention)
                        (string/=* monthly  last-intervention)))
        (setq date (if new monthly last-intervention))))
    (find-file (intervention-file-path firm project date))
    (let* ((footer (let ((footer (make-string 74 (character "="))))
                     (setf (char footer 0) (character " ")
                           (char footer 73) (character " "))
                     footer))
           (header (let ((header (copy-seq footer)))
                     (replace header (format " %s " (string-upcase project))
                              :start1 5)
                     header)))
      (when (and new (= 0 (buffer-size)))
        (insert "\n")
        (insert header)
        (insert "\n\n\n\n\n")
        (insert footer)
        (insert "\nTotal:\n Facturation:\n Hourly Rate:\n\n\n"))
      (cond
        ((progn (goto-char (point-min))
                (search-forward-regexp " Total.*\n Facturation.*" nil t))
         (beginning-of-line)
         (if (search-backward "===="
                              (save-excursion (forward-line -5) (point)) t) 
             (progn (beginning-of-line) (open-line 2))
             (progn (forward-line -1) (open-line 1) 
                    (insert footer)
                    (beginning-of-line) (open-line 2))))
        ((progn (goto-char (point-min))
                (search-backward footer nil t))
         (beginning-of-line)
         (open-line 2))
        (t
         (goto-char (point-max)))))
    (insert "\n\n")
    (pjb-work-begin)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Watch the user activity.

;; (defvar *start-inactivity-timer* nil)
;; (defvar *inactivity-timer*       nil)
;; (defvar *slice-activity-timer*   nil)

;; (defvar *command-count*          0)     

(defvar *current-command*       nil)
(defvar *hostname*
  (shell-command-to-string
   "echo -n $( (hostname -f 2>/dev/null) || (hostname 2>/dev/null) )"))
(defvar *activity-tag* "EMACS")


(defstruct activity
  event
  (time (GET-UNIVERSAL-TIME))
  (hostname *hostname*)
  (tag *activity-tag*)
  (buffer-name (buffer-name (current-buffer)))
  (file-name   (buffer-file-name (current-buffer)))
  last-command-char
  last-command
  last-prefix-arg
  duration
  client
  project
  annotation)
               

(defun activity-send-message (message)
  (message "%S" message))

(defvar *activity-last-start-event* nil)
(defvar *activity-last-stop-event*  nil)
(defvar *activity-last-count*       0)

(defun activity-enqueue-event (event)
  (cond
    ((equal event *activity-last-start-event*)
     (return-from activity-enqueue-event))
    ((equal event *activity-last-stop-event*)
     (incf  *activity-last-count*)
     (return-from activity-enqueue-event))
    ((eql 'start (activity-event event))
     (setf *activity-last-start-event* event))
    ((eql 'stop (activity-event event))
     (setf *activity-last-stop-event* event))
    ))

(defun activity-pre-command ()
  (ignore-errors
    (setf *current-command*
          (make-activity :event 'start
                         :last-command-char last-command-char
                         :last-command      last-command
                         :last-prefix-arg   last-prefix-arg))
    (activity-enqueue-event *current-command*)))


(defun activity-post-command ()
  (ignore-errors
    (let* ((time (GET-UNIVERSAL-TIME))
           (duration (- time (activity-time *current-command*)))
           (event (make-activity :event 'stop
                                 :last-command-char last-command-char
                                 :last-command      last-command
                                 :last-prefix-arg   last-prefix-arg
                                 :duration          duration)))
      (activity-enqueue-event event))))


(defun activity-annotate (annotation)
  (interactive "sAnnotation: ")
  (activity-enqueue-event (make-activity :event 'annotate :annotation annotation))
  'annotated)

(defun activity-switch (client project)
  (interactive
   (list 
    ;; firms
    (completing-read 
     "Firm: " (mapcar (lambda (x) (cons x nil)) (get-firm-list))
     (lambda (firm) (setq *pjb-intervention-firm* (car firm)))  t)
    ;; projects:
    (completing-read
     "Project: " (mapcar (lambda (x) (cons x nil)) 
                         (get-project-list *pjb-intervention-firm*))
     (function identity) nil)))
  (activity-enqueue-event (make-activity :event 'switch :client client :project project))
  'switched)


(defun activity-on ()
  (interactive)
  (add-hook 'pre-command-hook   'activity-pre-command)
  (add-hook 'post-command-hook  'activity-post-command))


(defun activity-off ()
  (interactive)
  (remove-hook 'pre-command-hook   'activity-pre-command)
  (remove-hook 'post-command-hook  'activity-post-command))

(when nil
  (activity-on)
  (activity-annotate "Howdy")
  (activity-switch "afaa" "telemaintenance")
  (activity-off)
  )


;;   (activity-install-inactivity-timer)
;;   (activity-remove-timers)

;; (defun activity-slice ()
;;   (ignore-errors
;;     (let ((command-count 0))
;;       (rotatef command-count *command-count*)
;;       (if (zerop command-count)
;;           (progn                        ; inactive
;;             (activity-enqueue-event (list 'inactive (GET-UNIVERSAL-TIME))))
;;           (progn                        ; active
;;             (activity-enqueue-event (list 'active (GET-UNIVERSAL-TIME)
;;                                           command-count)))))))


;; (defun activity-idle ()
;;   (let ((event (list 'idle (GET-UNIVERSAL-TIME)
;;                      (buffer-name (currentbuffer)))))
;;     (activity-enqueue-event event)))


;; (defun activity-idle-start ()
;;   (ignore-errors
;;     (when *inactivity-timer*
;;       (cancel-timer *inactivity-timer*)))
;;   (setf *inactivity-timer* (run-with-timer 2 2 'activity-idle)))



;; (defun activity-install-timers ()
;; ;;   (when *start-inactivity-timer*
;; ;;     (cancel-timer *start-inactivity-timer*))
;; ;;   (setf *start-inactivity-timer* (run-with-idle-timer 2 t 'activity-idle-start))
;;   (when *slice-activity-timer*
;;     (cancel-timer *slice-activity-timer*))
;;   (setf *slice-activity-timer* (run-with-timer 60 60 'activity-slice)))
;; 
;; (defun activity-remove-timers ()
;; ;;   (when *start-inactivity-timer*
;; ;;     (cancel-timer *start-inactivity-timer*)
;; ;;     (setf *start-inactivity-timer* nil))
;; ;;   (when *inactivity-timer*
;; ;;     (cancel-timer *inactivity-timer*)
;; ;;     (setf *inactivity-timer* nil))
;;   (when *slice-activity-timer*
;;     (cancel-timer *slice-activity-timer*)
;;     (setf *slice-activity-timer* nil)))


;; timer-idle-list
;; timer-list

;; activity-switch         (client project &optional annotation)
;; activity-add-annotation (annotation)
;; 
;; each minute
;; count the number of commands
;; see if the minute is idle
;; 
;; collect lav too...





;; (directory (format "%s/firms/%s/interventions/*-%s.txt" (getenv "HOME") firm))
;; (unless (and (< 2 (length firm))
;;              (directory-p (format "%s/firms/%s/" (getenv "HOME") firm)))
;;     (error "There is no firm named %S." firm))

;;;; pjb-work.el                      --                     --          ;;;;
