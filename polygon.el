(defvar *polygon* '())

(defun start-collect-polygon ()
  (interactive)
  (setf *polygon* '()))

(defun polygon-collect-point (event)
  (interactive "e")
  (push (third (event-start event)) *polygon*))

(defun end-collect-polygon ()
  (interactive)
  (setf *polygon* (nreverse  *polygon*))
  (insert (format "%S" *polygon*)))

(global-set-key (kbd "C-c p s")   'start-collect-polygon)
(global-set-key (kbd "<mouse-3>") 'polygon-collect-point)
(global-set-key (kbd "C-c p e")   'end-collect-polygon)

((694 . 332) (504 . 486) (306 . 349) (291 . 163) (551 . 198))
