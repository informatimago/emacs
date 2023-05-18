
(defun window-follow-mouse ()
  (unless (active-minibuffer-window)
    (destructuring-bind (frame column . row) (mouse-position)
      (let ((mse-window (window-at column row frame))
            (cur-window (selected-window)))
        (unless (eql mse-window cur-window)
          (select-window mse-window))))))

(defadvice select-window  (after select-window/window-follow-mouse disable)
  (destructuring-bind (left top right bottom) (window-edges (selected-window) :body)
    (let ((frame  (selected-frame))
          (column (truncate (+ left right) 2))
          (row    (truncate (+ top bottom) 2)))
      (set-mouse-position frame column row))))


(defvar *window-follow-mouse-timer* nil)

(defun schedule-window-follow-mouse ()
  (interactive)
  (ad-add-advice 'select-window  'after 'select-window/window-follow-mouse 'last)
  (setf *window-follow-mouse-timer* (run-at-time 1 0.1 'window-follow-mouse)))

(defun unschedule-window-follow-mouse ()
  (interactive)
  (when *window-follow-mouse-timer*
    (ad-remove-advice 'select-window 'after 'select-window/window-follow-mouse)
    (cancel-timer *window-follow-mouse-timer*)
    (setf *window-follow-mouse-timer* nil)))

