;;;
;;; This is superseded by the mfod command (using emacsclient to make frames).
;;;

(defparameter *frame-server-job-ticket* "~/frame-emacs"
  "Path to the job-ticket file.")


(defun frame-server (&optional token-path)
  (setf token-path (or token-path *frame-server-job-ticket*))
  (when (file-exists-p token-path)
    (find-file token-path)
    (make-frame-on-display
     (delete ?\n (prog1 (buffer-string)
                   (kill-buffer (current-buffer))
                   (delete-file token-path)))
     (list (cons 'name (format "n%s" (frame-parameter nil 'name)))))))

(defun frame-server-start ()
  (interactive)
  (run-at-time nil 5 (function frame-server) nil))

(frame-server-start)


;;;
;;; Not used anymore.
;;;


(defvar *galatea-frame* nil)


(defun open-frame-on-galatea ()
  (interactive)
  (unless *galatea-frame*
    (setq *galatea-frame*
          (make-frame-on-display "galatea.informatimago.com:0.0")))
  (set-frame-size  *galatea-frame* 96 40)
  (let ((current-frame (selected-frame)))
    (select-frame *galatea-frame*)
    (set-background-color "#102040")
    (set-foreground-color "#80f0f0")
    ;;(set-face-foreground 'font-lock-comment-face "Green")
    ;;(set-face-foreground 'font-lock-function-name-face "Yellow")
    (select-frame current-frame))
  (setq common-lisp-hyperspec-frame *galatea-frame*))


(defun reopen-frame-on-galatea ()
  (interactive)
  (when *galatea-frame*
    (delete-frame *galatea-frame*)
    (setq *galatea-frame* nil))
  (open-frame-on-galatea))
