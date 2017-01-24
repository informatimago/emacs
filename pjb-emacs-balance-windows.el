;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-emacs-balance-window.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Set same size for all windows.
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

(defun horizontal-offset ()
  "Number of columns taken by the fringe and vertical scroll bar"
  ;; TODO: Implement in function of the effective fringe and vertical scroll bar.
  5)

(defun pjb-balance-windows-vertically ()
  "Make all visible windows the same width (approximately)."
  (interactive)
  (let ((count -1) levels newsizes level-size
        (last-window (previous-window (frame-first-window (selected-frame))))
        ;; Don't count the columns that are past the lowest main window.
        total)
    ;; Rightmost edge of last window determines what size we have to work with.
    (setq total
          (+ (window-width last-window) (horizontal-offset)
             (nth 0 (window-edges last-window))))
    ;; Find all the different hpos's at which windows start,
    ;; then count them.  But ignore levels that differ by only 1.
    (let (lefts (prev-left -2))
      (walk-windows (function (lambda (w)
                      (setq lefts (cons (nth 0 (window-edges w))
                                        lefts))))
                    'nomini)
      (setq lefts (sort lefts '<))
      (while lefts
        (if (> (car lefts) (1+ prev-left))
            (setq prev-left (car lefts)
                  count (1+ count)))
        (setq levels (cons (cons (car lefts) count) levels))
        (setq lefts (cdr lefts)))
      (setq count (1+ count)))
    ;; Subdivide the frame into desired number of vertical levels.
    (setq level-size (/ total count))
    (.EMACS "levels=%S" levels)
    (.EMACS "level-size=%S" level-size)
    (save-selected-window
      ;; Set up NEWSIZES to map windows to their desired sizes.
      ;; If a window ends at the rightmost level, don't include
      ;; it in NEWSIZES.  Those windows get the right sizes
      ;; by adjusting the ones above them.
      (walk-windows (function
                     (lambda (w)
                      (let ((newleft (cdr (assq (nth 0 (window-edges w))
                                                levels)))
                            (newright (cdr (assq (+ (window-width w)
                                                    (horizontal-offset)
                                                    (nth 0 (window-edges w)))
                                                 levels))))
                        (message ".EMACS: newleft=%S newright=%S"
                                 newleft newright)
                        (if newright
                            (setq newsizes
                                  (cons (cons w (* level-size
                                                   (- newright newleft)))
                                        newsizes))))))
                    'nomini)
      (.EMACS "newsizes=%S" newsizes)
      ;; Make walk-windows start with the leftmost window.
      (select-window (previous-window (frame-first-window (selected-frame))))
      (let (done (count 0))
        ;; Give each window its precomputed size, or at least try.
        ;; Keep trying until they all get the intended sizes,
        ;; but not more than 3 times (to prevent infinite loop).
        (while (and (not done) (< count 3))
          (setq done t)
          (setq count (1+ count))
          (walk-windows (function (lambda (w)
                          (select-window w)
                          (let ((newsize (cdr (assq w newsizes))))
                            (when newsize
                              (apply (function enlarge-window)
                                     (- newsize
                                        (horizontal-offset)
                                        (window-width))
                                     t
                                     (if (= 2 (cdr (function-argument-counts
                                                    (function enlarge-window))))
                                         '()
                                         '(preserve)))
                              (unless (= (window-width)
                                         (- newsize (horizontal-offset)))
                                (setq done nil))))))
                        'nomini))))))



(defun pjb-balance-windows (&optional horizontally)
  "Make all visible windows on the current frame the same size (approximately).
If optional prefix arg is not given, \"same size\" is same height.
When prefix arg is given,  \"same size\" is same width."
  (interactive "P")
  (let* (count size w cmjr resize
               (edge (if horizontally 0 1)) ;; Minor field to sort by 0=LEFT, 1=TOP
               (mjr (- 1 edge))             ;; Major field to sort
               (far (+ 2 edge)) ;; far edge (right/bottom) - for current size
               (windows nil)    ;; list of windows
               (ix 0)
               nwin                   ;; number of windows
               (curw (selected-window)) ;; selected window (to return to)
               )
    ;; Build and sort list of all windows on frame
    (save-window-excursion
      (walk-windows (function (lambda (w)
                      (let ((ltrb (window-edges w)))
                        (setq windows (cons (list
                                             (nth mjr  ltrb)
                                             (nth edge ltrb)
                                             (nth far  ltrb)
                                             w) windows)))))
                    'nomini)
      (setq windows (sort windows (lambda (e1 e2)
                                    (if (< (nth 0 e1) (nth 0 e2))
                                        t
                                        (if (= (nth 0 e1) (nth 0 e2))
                                            (if (< (nth 1 e1) (nth 1 e2))
                                                t)))))))
    (setq nwin (length windows))
    ;; add 1 extra entry (for while check)
    (appendf windows '((-1 -1 -1 nil)))

    (while (< ix nwin)                  ; walk on all (sorted) windows
      (setq count ix)         ; count the windows in 1 column (or row)
      (setq cmjr (car (nth ix windows))) ; column / raw identification
      (while (= cmjr (car (nth ix windows)))   ; same column / row
        (setq ix (1+ ix)))                     ; next window
      (setq count (- ix count))
      (if (/= count 1) ; do only if more than one window in this column/row
          (let ((gix (- ix count)))
            (setq size (- (nth far (window-edges (nth 3 (nth (1- ix) windows))))
                          (nth edge (window-edges
                                     (nth 3 (nth (- ix count) windows))))))
            (setq size (/ (+ size count -1) count)) ; average window size

            ;; (.EMACS "Size=%d" size)

            (while (< gix ix)
              (setq w (nth 3 (nth gix windows)))
              (setq resize (- size (- (nth far (window-edges w))
                                      (nth edge (window-edges w)))))

              ;; (.EMACS "Window=%s  resize=%d" w resize)
                                        ; don't resize by 1 character/line
              (if (or (> resize 1)
                      (< resize -1))
                  (progn

                    ;; (sit-for 2)

                    (select-window w)   ; window to work on
                    (apply (function enlarge-window)
                           resize horizontally
                           (if (= 2 (cdr (function-argument-counts
                                          (function enlarge-window))))
                               '()
                               '(preserve)))
                    ;; (sit-for 2)
                    ))
              (setq gix (1+ gix))))))

    ;; (.EMACS "")
    (select-window curw)))

;;;; THE END ;;;;
