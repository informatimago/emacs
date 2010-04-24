(require 'pjb-cl)

;; Some time ago I wrote some suggestions about how to rewrite
;; balance-windows to use the windows split tree. I have tried to do
;; that. The file bw.el at
;; 
;;    http://ourcomments.org/Emacs/DL/elisp/test/
;; 
;; contains my rewrite of balance-windows. Could those who are interested
;; please test this version?



;; Since window parents are not normal emacs lisp windows, I'd assume
;; we'd export the window internals as a new data type. Let's call it
;; "split".
;; 
;; (frame-split-root [frame])           --> split or nil
;; (window-configuration-split winconf) --> split or nil
;; (splitp object)                      --> boolean
;; (split-direction split)              --> :vertical | :horizontal
;; ;; (window-split-vertically) produces a :vertical split-direction.
;; (split-edges sow)                    --> (left top right bottom)
;; ;; Perhaps (defalias split-edges window-edges)
;; (split-cuts split)                   --> (BReak  BReak  ...)
;; ;; The bottom or right of the correponding child.
;; (split-children split)               --> (child  child  ...)
;; ;; A split child may be a window or a split.
;; (split-parent sow) --> split
;; (enlarge-split sow increment preserve-before)
;; 
;; 
;; ;; The parent is null for root splits or windows,
;; ;; othewise it's always a split.
;; (or (null   (split-parent sow))
;;     (splitp (split-parent sow)))
;; 
;; ;; the edges of the split are the same as the edge of an alone window:
;; (equal (split-edges split) (progn (delete-other-windows) (window-edges)))
;; 
;; ;; the length is the number of collapsed windows or splits:
;; (= (length (split-cuts split)) (length (split-children split)))
;; 
;; ;; child splits are perpendicular:
;; (every (lambda (child-split)
;;          (not (eq (split-direction split) (split-direction child-split))))
;;        (delete-if-not (function splitp) (split-children split)))
;; 
;; ;; split cuts are sorted (hence the order of the corresponding split-children
;; ;; list):
;; (dotimes (i (1- (length (split-cuts split))))
;;   (< (nth i (split-cuts split)) (nth (1+ i) (split-cuts split))))


;; (dotimes (i (length (split-cuts split)))
;;   (and
;;    ;; all children are either a split or a window:
;;    (or (windowp  (nth i (split-children split)))
;;        (splitp   (nth i (split-children split))))
;;    ;; the parent of each children of split is the split itself:
;;    (eq split (split-parent (nth i (split-children split))))
;;    ;; the cuts are the bottom or right of the corresponding child:
;;    (= (nth i (split-cuts split))
;;       (funcall (if (eq (split-direction split) :vertical)
;;                    (function bottom)
;;                    (function right))
;;                (split-edges (nth i (split-children split)))))
;;    ;; the sides are the same for all children:
;;    (every (lambda (side)
;;             (= (funcall side (split-edges split))
;;                (funcall side (split-edges (nth i (split-children split))))))
;;           (if (eq (split-direction split) :vertical)
;;               (list (function left) (function right))
;;               (list (function top)  (function bottom))))
;;    ))
;; 
;; 
;; 
;; Several parallel cuts are collapsed into one split.
;; 
;; (progn (delete-other-windows)
;;        (split-window-vertically)
;;        (split-window-vertically))
;; 
;; and:
;; 
;; (progn (delete-other-windows)
;;        (split-window-vertically)
;;        (other-window 1)
;;        (split-window-vertically))
;; 
;; would both give (= 3 (length (split-cuts (frame-split-root)))).
;; And:
;; 
;; (equal (progn (delete-other-windows)
;;               (split-window-vertically)
;;               (split-window-vertically)
;;               (frame-split-root))
;;        (progn (delete-other-windows)
;;               (split-window-vertically)
;;               (other-window 1)
;;               (split-window-vertically)
;;               (frame-split-root)))
;; 
;; 
;; 
;; The the balancing algorithm could be:

(defun plusp  (x) (< 0 x))
(defun left   (x) (first  x))
(defun top    (x) (second x))
(defun right  (x) (third  x))
(defun bottom (x) (fourth x))

(defun split-height (split)
  (- (bottom (split-edges split)) (top  (split-edges split))))

(defun split-width  (split)
  (- (right  (split-edges split)) (left (split-edges split))))
   
(defun balance-split (split)
  (labels ((count-children (sow direction)
             ;; This could be cached into then split-or-window structure
             (if (splitp sow)
                 (reduce (if (eq (split-direction sow) direction)
                             (function +)
                             (function max))
                         (mapcar (lambda (child) (count-children child direction))
                                 (split-children sow)))
                 1)))
    (let* ((relative-sizes
            (mapcar (lambda (child) (count-children child (split-direction split)))
                    (split-children split)))
           (total (reduce (function +) relative-sizes))
           (split-size (if (eq (split-direction split) :vertical)
                           (split-height split)
                           (split-width  split))))
      (loop
         for child-size in relative-sizes
         for child in (butlast (split-children split))
         for new-size = (truncate (* split-size child-size) total)
         do (enlarge-split child
                           (- new-size
                              (if (eq (split-direction split) :vertical)
                                  (split-height child)
                                  (split-width  child)))
                           (split-direction split)
                           t))
      (dolist (child (split-children split))
        (when (splitp child)
          (balance-split child))))))

(defun balance-windows ()
  (interactive)
  (when (splitp (frame-split-root))
    (balance-split (frame-split-root))))













;; The following is a simulator for the above specified split data structure.

(defstruct sow direction parent children right bottom)

(defun split-or-window-previous (sow)
  (let ((parent  (split-or-window-parent sow)))
    (when parent
      (let* ((children (split-or-window-children parent))
             (i (position sow children)))
        (and i (plusp i) (nth (1- i) children))))))
    
(defun split-or-window-left (sow)
  (let ((parent (split-or-window-parent sow)))
    (if parent
        (if (eq (split-or-window-direction parent) :horizontal)
            (let ((previous (split-or-window-previous sow)))
              (if previous
                  (split-or-window-right previous)
                  (split-or-window-left  parent)))
            (split-or-window-left  parent))
        0)))

(defun split-or-window-top (sow)
  (let ((parent (split-or-window-parent sow)))
    (if parent
        (if (eq (split-or-window-direction parent) :vertical)
            (let ((previous (split-or-window-previous sow)))
              (if previous
                  (split-or-window-bottom previous)
                  (split-or-window-top   parent)))
            (split-or-window-top   parent))
        0)))

(defun splitp (sow)
  (and (split-or-window-p sow)
       (split-or-window-children sow)))

(defun split-edges (sow)
  (list (split-or-window-left sow)
        (split-or-window-top sow)
        (split-or-window-right sow)
        (split-or-window-bottom sow)))

(defvar *current-window*   (make-split-or-window  :right 42 :bottom 42))
(defvar *frame-split-root* *current-window*)
(defun frame-split-root    (&optional frame)  *frame-split-root*)
(defvar *window-list*      (list *current-window*))
(defun window-list*        () *window-list*)
(defun current-window      () *current-window*)

(defun initialize (width height)
  (setf *current-window*    (make-split-or-window  :right width :bottom height)
        *frame-split-root*  *current-window*
        *window-list* (list *current-window*)))

(defun split-direction (split)
  (assert (splitp split))
  (split-or-window-direction split))

(defun split-cuts (split)
  (assert (splitp split))
  (mapcar (if (eq (split-direction split) :vertical)
              (function bottom)
              (function right)) (split-children split)))

(defun split-children (split)
  (assert (splitp split))
  (split-or-window-children split))

(defun split-parent (sow)
  (split-or-window-parent sow))

(defun enlarge-split (sow increment direction preserve-before)
  (unless (zerop increment)
    (if (eq direction :vertical)
        (incf (split-or-window-bottom sow) increment)
        (incf (split-or-window-right  sow) increment))
    (when (splitp sow)
      (if (eq (split-direction sow) direction)
          ;; change the size in same direction
          ;; TODO: check increment vs. last size and preserve-before
          (let ((last-child  (first (last (split-or-window-children
                                           sow)))))
            (enlarge-split last-child increment direction preserve-before))
          ;; change the size orthogonally
          (dolist (child (split-or-window-children sow))
            (enlarge-split child increment direction preserve-before))))
    (let ((parent (split-or-window-parent sow)))
      (when (and parent (eq (split-direction parent) direction))
        (let ((next (second (memq sow (split-or-window-children parent)))))
          (when next
            (if (eq direction :vertical)
                (incf (split-or-window-top  next) increment)
                (incf (split-or-window-left next) increment))))))))

(defun insert-after (new old list) (push new (cdr (memq old list))))

(defun split (direction &optional size)
  (flet ((split-size (split) (if (eq direction :vertical)
                                 (split-height split)
                                 (split-width  split)))
         (make-split (direction window parent)
           (let* ((edges (split-edges window))
                  (result (make-split-or-window
                           :parent parent
                           :children (list window)
                           :direction direction
                           :right  (right  edges)
                           :bottom (bottom edges))))
             (setf (split-or-window-parent window) result))))
    (setf size (or size (/ (split-size (current-window)) 2)))
    (unless (< 1 size)
      (error  "split: Window too small."))
    (when (<= (split-size (current-window)) size)
      (error "split: Too big size asked."))
    (let ((split (split-parent (current-window))))
      (cond
        ((null split)
         ;; make a new root split:
         (setf split (make-split direction (current-window) nil))
         (setf *frame-split-root* split))
        ((not (eq direction (split-direction split)))
         ;; make a new perpendicular split
         (let ((perpendicular (make-split direction (current-window) split)))
           (setf (split-or-window-children split)
                 (nsubstitute perpendicular (current-window)
                              (split-or-window-children split)))
           (assert (memq perpendicular (split-or-window-children split)))
           (setf split perpendicular))))
      (assert (eq split (split-parent (current-window))))
      (assert (memq (current-window) (split-children split)))
      (assert (eq direction (split-direction split)))
      (let* ((dx (if (eq direction :vertical) 0 size))
             (dy (if (eq direction :vertical) size 0))
             (edges (split-edges (current-window)))
             ;; make a new window:
             (other (make-split-or-window
                     :parent split
                     :right  (right  edges)
                     :bottom (bottom edges))))
        (if (eq direction :vertical)
            (setf (split-or-window-bottom (current-window)) (+ (top  edges) dy))
            (setf (split-or-window-right (current-window))  (+ (left edges) dx)))
        (insert-after other (current-window) (split-or-window-children split))
        (insert-after other (current-window) *window-list*)
        (assert (memq (current-window) *window-list*))
        (assert (memq other            *window-list*))
        (assert (memq (current-window) (split-or-window-children split)))
        (assert (memq other            (split-or-window-children split)))))))

(defun other-window* (n &optional all-frames)
  (let ((i (mod (+ (position (current-window) (window-list*)) n)
                (length (window-list*)))))
    (setf *current-window* (nth i (window-list*)))))

(defun windowp* (window) (memq window (window-list*)))

(defun delete-other-windows* (&optional window)
  (when (windowp* window)
    (setf *current-window* window))
  (setf window (current-window))
  (when (split-parent window)
    (let ((root  (frame-split-root)))
      (setf (split-or-window-left   window) (split-or-window-left   root)
            (split-or-window-top    window) (split-or-window-top    root)
            (split-or-window-right  window) (split-or-window-right  root)
            (split-or-window-bottom window) (split-or-window-bottom root)))
    (setf (split-or-window-parent window) nil)
    (setf *frame-split-root* window
          *window-list* (list window))))
     
(defun split-describe (split &optional level)
  (setf level (or level ""))
  (if (splitp split)
      (progn
        (insert (format "%s#<split %s %s %s %s>\n"
                  level
                  (format ":direction %S" (split-direction split))
                  (format ":parent %S" (not (null (split-parent split))))
                  (format ":edges %S" (split-edges split))
                  (format ":children %S" (length (split-children split)))))
        (dolist (child (split-children split))
          (split-describe child (concat "  " level))))
      (insert (format "%s#<window %s %s>\n"
                level
                (format ":parent %S" (not (null (split-parent split))))
                (format ":edges %S" (split-edges split))))))

(defun split-draw (split)
  (with-current-buffer (get-buffer-create "*splits*")
    (erase-buffer)
    (unless (eq major-mode 'picture-mode)
      (picture-mode))
    (picture-open-line (1+ (bottom (split-edges split))))
    (labels ((goto (x y)
               (message "goto %s %s" x y)
               (goto-line y)
               (beginning-of-line)
               (picture-forward-column x))
             (draw-rectangle (edges)
               (picture-draw-rectangle
                (progn (goto (left  edges) (top    edges)) (point))
                (progn (goto (right edges) (bottom edges)) (point))))
             (draw-split (split)
               (draw-rectangle (split-edges split))
               (if (splitp split)
                   (dolist (child (split-children split))
                     (draw-split child)))))
      (draw-split split)))
  (sit-for 1)
  (force-mode-line-update t))


(progn
  (initialize 60 56)
  (delete-other-windows*)
  (split :vertical)
  (split :horizontal)
  (split :vertical)
  (split :vertical)
  (balance-windows) (split-draw (frame-split-root))
  (other-window* 4)
  (split :vertical)
  (split :horizontal)
  (split :horizontal)
  (other-window* 1)
  (split :vertical)
  (balance-windows) (split-draw (frame-split-root))
  (other-window* 7)
  (split :vertical)
  (split :vertical)
  (split :vertical)
  (balance-windows) (split-draw (frame-split-root))
  (split-describe (frame-split-root)))


;; #<split :direction :vertical :parent nil :edges (0 0 60 56) :children 3>
;;   #<split :direction :horizontal :parent t :edges (0 0 60 32) :children 2>
;;     #<split :direction :vertical :parent t :edges (0 0 30 32) :children 3>
;;       #<window :parent t :edges (0 0 30 10)>
;;       #<window :parent t :edges (0 10 30 20)>
;;       #<window :parent t :edges (0 20 30 32)>
;;     #<split :direction :vertical :parent t :edges (30 0 60 32) :children 4>
;;       #<window :parent t :edges (30 0 60 8)>
;;       #<window :parent t :edges (30 8 60 16)>
;;       #<window :parent t :edges (30 16 60 24)>
;;       #<window :parent t :edges (30 24 60 32)>
;;   #<split :direction :horizontal :parent t :edges (0 32 60 48) :children 3>
;;     #<window :parent t :edges (0 32 20 48)>
;;     #<split :direction :vertical :parent t :edges (20 32 40 48) :children 2>
;;       #<window :parent t :edges (20 32 40 40)>
;;       #<window :parent t :edges (20 40 40 48)>
;;     #<window :parent t :edges (40 32 60 48)>
;;   #<window :parent t :edges (0 48 60 56)>
;; 
;; 
;; +-----------------------------+-----------------------------+
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             +-----------------------------+
;; |                             |                             |
;; +-----------------------------|                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             +-----------------------------+
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; +-----------------------------|                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             +-----------------------------+
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; |                             |                             |
;; +-------------------+-------------------+-------------------+
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   +-------------------|                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; |                   |                   |                   |
;; +-----------------------------------------------------------+
;; |                                                           |
;; |                                                           |
;; |                                                           |
;; |                                                           |
;; |                                                           |
;; |                                                           |
;; |                                                           |
;; +-----------------------------------------------------------+

(frame-list)
(#<frame emacs: pjb@thalassa 0x919a6b0> #<frame PGM 0x8607cc0>)
(mapcar (function window-edges) (window-list))



(mapcar (function window-edges)
        (window-list (first (frame-list)) nil
                     (frame-first-window (first (frame-list)))))


(defstruct (pt (:type list)) x y)

(defun pt-of-edges (edges)
  (list
   (make-pt :x (left  edges) :y (top    edges))
   (make-pt :x (left  edges) :y (bottom edges))
   (make-pt :x (right edges) :y (top    edges))
   (make-pt :x (right edges) :y (bottom edges))))



(defun plusp  (x) (< 0 x))
(defun left   (x) (first  x))
(defun top    (x) (second x))
(defun right  (x) (third  x))
(defun bottom (x) (fourth x))


(defalias 'current-frame  'selected-frame)
(defalias 'current-window 'selected-window)

(defstruct sow* parent left top right bottom)
(defstruct (window* (:include sow*)) window)
(defstruct (split*  (:include sow*)) direction children)

(defun window* (window)
  (let ((edges (window-edges window)))
    (make-window* :window window
                  :left   (left   edges)
                  :right  (right  edges)
                  :top    (top    edges)
                  :bottom (bottom edges))))

(defun window*-list (&optional frame)
  (setf frame (or frame  (current-frame)))
  (mapcar (function window*)
          (window-list frame nil (frame-first-window frame))))

(defun split*-edges (sow)
  (etypecase sow
    (window  (window-edges sow))
    (window* (list (window*-left  sow) (window*-top    sow)
                   (window*-right sow) (window*-bottom sow)))
    (split*  (list (split*-left    sow) (split*-top      sow)
                   (split*-right   sow) (split*-bottom   sow)))))

(defun split*-height (split)
  (- (bottom (split*-edges split)) (top  (split*-edges split))))

(defun split*-width  (split)
  (- (right  (split*-edges split)) (left (split*-edges split))))

(defun boxes-match-sides (a b)
  (cond
    ((and (= (right  a) (left   b))
          (= (top    a) (top    b))
          (= (bottom a) (bottom b))) :on-the-right)
    ((and (= (right  b) (left   a))
          (= (top    b) (top    a))
          (= (bottom b) (bottom a))) :on-the-left)
    ((and (= (bottom a) (top    b))
          (= (left   a) (left   b))
          (= (right  a) (right  b))) :over)
    ((and (= (bottom b) (top    a))
          (= (left   b) (left   a))
          (= (right  b) (right  a))) :under)
    (t nil)))


(defun symetric-side (x)
  (ecase x
    (:on-the-right :on-the-left)
    (:on-the-left  :on-the-right)
    (:over         :under)
    (:under        :over)))

(defun orthogonal-sides-p (a b)
  (ecase a
    ((:on-the-right :on-the-left)  (ecase b
                                     ((:on-the-right :on-the-left) nil)
                                     ((:over         :under)       t)))
    ((:over         :under)        (ecase b
                                     ((:on-the-right :on-the-left) t)
                                     ((:over         :under)       nil)))))



(defun more-than-one-p (set) (cdr set))
(defun choose&extract (set) (values (first set) (rest set) set))
(defun find&extract   (item set &rest cl-keys)
  (cl-parsing-keywords ((:test (function eql)) (:key (function identity))) nil
    (let ((index (position item set :test cl-test :key cl-key)))
      (cond
        ((null index)  (values nil set nil))
        ((zerop index) (values (first set) (rest set) t))
        (t             (values (nth index set)
                               (delete-if (constantly t) set
                                          :start index :end (1+ index))
                               t))))))
                                         
(defun make-chainlet    (list)   (cons list (last list)))
(defun chainlet-list    (c) (car c))
(defun chainlet-tail    (c) (cdr c))
(defun chainlet-first   (c) (caar c))
(defun chainlet-last    (c) (cadr c))
(defun chainlet-match-p (c d) (eql (cadr c) (caar d)))
(defun nchainlet-join   (c d) (setf (cddr c) (cdar d)  (cdr c) (cdr d)) c)
(defun chain-lists (lists)
  (loop
     with changed = t
     with chainlets = (mapcar (function make-chainlet) lists)
     while changed
     do (setf changed nil)
     (loop
        with new-chainlets = '()
        while (more-than-one-p chainlets)
        do (multiple-value-bind (current rest) (choose&extract chainlets)
             (multiple-value-bind (next rest gotit)
                 (find&extract current rest :test (function chainlet-match-p))
               (if gotit
                   (progn
                     (push (nchainlet-join current next) new-chainlets)
                     (setf changed   t))
                   (push current new-chainlets))
               (setf chainlets rest)))
        finally (setf chainlets (append chainlets new-chainlets)))
     finally (return (mapcar (function chainlet-list) chainlets))))


(defun group-sows (sows)
  (flet ((collect-sides (windows edges)
           (loop
              for wl on windows
              for el on edges
              nconc (loop
                       with wina = (car wl)
                       with edga = (car el)
                       for winb in (cdr wl)
                       for edgb in (cdr el)
                       for side = (boxes-match-sides edga edgb)
                       when side collect (list wina side winb)
                       when side collect (list winb (symetric-side side)
                                               wina)))))
    (let ((sides
           (loop
              with sow = sows
              for edges = (mapcar (function split*-edges) sow)
              for sides = (collect-sides sow edges)
              for ww = (make-hash-table)
              while (block :body
                      (dolist (side sides)
                        (let ((w (gethash (first side) ww)))
                          (if (and w (orthogonal-sides-p w (second side)))
                              (let ((win  (first side)))
                                (with-selected-window (window*-window win)
                                  (enlarge-window
                                   (if (< (split*-height win) 2) 1 -1) nil)
                                  (enlarge-window
                                   (if (< (split*-width  win) 2) 1 -1) t))
                                (return-from :body t))
                              (setf (gethash (first side) ww) (second side))
                              nil))))
              finally (return sides))))
      (delete-if (lambda (side) (memq (second side) '(:under :on-the-left))) sides))))

(defun frame-split-root (&optional frame)
  (setf frame (or frame (current-frame)))
  (flet ((ensplit (children direction)
           (let* ((edges (mapcar (function split*-edges) children))
                  (split (make-split*
                          :children  children
                          :direction direction
                          :left    (reduce (function min)
                                           (mapcar (function left) edges))
                          :right   (reduce (function max)
                                           (mapcar (function right) edges))
                          :top     (reduce (function min)
                                           (mapcar (function top) edges))
                          :bottom  (reduce (function max)
                                           (mapcar (function bottom) edges)))))
             (dolist (child children)
               (setf (sow*-parent child) split))
             split)))
    (loop
       with sows = (window*-list frame)
       while (more-than-one-p sows)
       do (multiple-value-bind (horizontal vertical)
              (loop
                 with groups = (group-sows sows)
                 with horizontal = '()
                 with vertical   = '()
                 for group in groups
                 ;; Note: we keep emacs meaning for horizontal and vertical!
                 do (if (eq (verb group) :over)
                        (push (list (first group) (third group)) horizontal)
                        (push (list (first group) (third group)) vertical))
                 finally (return (values horizontal vertical)))
            (setf horizontal (chain-lists horizontal)
                  vertical   (chain-lists vertical)
                  sows (nconc
                        (mapcar (lambda (children) (ensplit children :horizontal))
                                horizontal)
                        (mapcar (lambda (children) (ensplit children :vertical))
                                vertical)))
            (print sows))
       finally (return (car sows)))))
                
(defun split-describe (split &optional level)
  (setf level (or level ""))
  (etypecase split
    (null (insert (format "%snil\n" level)))
    (split*
     (insert (format "%s#<split %s %s %s %s>\n"
               level
               (format ":direction %S" (split*-direction split))
               (format ":parent %S" (not (null (split*-parent split))))
               (format ":edges %S" (split*-edges split))
               (format ":children %S" (length (split*-children split)))))
     (dolist (child (split*-children split))
       (split-describe child (concat "  " level))))
    (window*
     (insert (format "%s#<window %s %s>\n"
               level
               (format ":parent %S" (not (null (window*-parent split))))
               (format ":edges %S" (split*-edges split)))))))



(split-describe (frame-split-root))

(#1=[cl-struct-split* nil 0 0 40 20 :horizontal ([cl-struct-window* #1# 0 0 40 13 #<window 141 on split.el>] #2=[cl-struct-window* #3=[cl-struct-split* nil 0 13 40 27 :horizontal (#2# [cl-struct-window* #3# 0 20 40 27 #<window 142 on split.el>])] 0 13 40 20 #<window 140 on split.el>])] #3# #4=[cl-struct-split* nil 40 0 86 13 :horizontal ([cl-struct-window* #4# 40 0 86 6 #<window 138 on split.el>] #5=[cl-struct-window* #6=[cl-struct-split* nil 40 6 86 27 :horizontal (#5# [cl-struct-window* #6# 40 13 86 27 #<window 144 on split.el>])] 40 6 86 13 #<window 145 on split.el>])] #6# #7=[cl-struct-split* nil 0 27 86 55 :horizontal ([cl-struct-window* #7# 0 27 86 41 #<window 136 on split.el>] [cl-struct-window* #7# 0 41 86 55 #<window 146 on split.el>])])

nil
nil
nil


(group-sows (window*-list))
(setf sows  (window*-list))

(chain-lists '((1 2) (2 3) (a b) (3 4) (d e) (c d) (aa bb) (cc dd)))

(setf c (make-chainlet (list 1 2))
      d (make-chainlet (list 2 3)))
(chainlet-match-p c d)
(nchainlet-join c d)
