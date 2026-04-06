;;; emacs-window-focus-mouse.el --- -*- lexical-binding: t -*-

(defun window-follow-mouse ()
  (unless (active-minibuffer-window)
    (let* ((mp     (mouse-position))
           (frame  (car mp))
           (col    (cadr mp))
           (row    (cddr mp))
           (mse-window (and (numberp col) (numberp row)
                            (window-at col row frame)))
           (cur-window (selected-window)))
      (when (and mse-window (not (eql mse-window cur-window)))
        (select-window mse-window)))))

(defun emacs-window-focus-mouse--center-mouse (&rest _args)
  "Center the mouse pointer on whatever window is currently selected.
Installed as `:after' advice on `select-window' by
`schedule-window-follow-mouse'."
  (let* ((edges  (window-edges (selected-window) t)) ; t = body edges
         (left   (nth 0 edges))
         (top    (nth 1 edges))
         (right  (nth 2 edges))
         (bottom (nth 3 edges))
         (frame  (selected-frame))
         (column (truncate (+ left right) 2))
         (row    (truncate (+ top bottom) 2)))
    (set-mouse-position frame column row)))


(defvar *window-follow-mouse-timer* nil)

(defun schedule-window-follow-mouse ()
  (interactive)
  (advice-add 'select-window :after #'emacs-window-focus-mouse--center-mouse)
  (setf *window-follow-mouse-timer* (run-at-time 1 0.1 'window-follow-mouse)))

(defun unschedule-window-follow-mouse ()
  (interactive)
  (when *window-follow-mouse-timer*
    (advice-remove 'select-window #'emacs-window-focus-mouse--center-mouse)
    (cancel-timer *window-follow-mouse-timer*)
    (setf *window-follow-mouse-timer* nil)))
