;;;; -*- mode:emacs-lisp; coding:utf-8; lexical-binding:t -*-

(require 'cl)


;; (let ((types   (make-hash-table :test (function equal)))
;;       (objects (make-hash-table))
;;       (queue  '()))
;;   (do-symbols (symbol)
;;     (push symbol queue))
;;   (loop
;;      while queue
;;      for object = (pop queue)
;;      do (unless (gethash object objects)
;;           (setf (gethash object objects) t)
;;           (incf (gethash (type-of object) types 0))
;;           (typecase object
;;             (symbol
;;              (when (boundp  object) (push (symbol-value    object) queue))
;;              (when (fboundp object) (push (symbol-function object) queue))
;;              (setf queue (nconc (copy-list (symbol-plist object)) queue)))
;;             (cons
;;              (push (car object) queue)
;;              (push (cdr object) queue))
;;             (vector
;;              (loop for i below (length object)
;;                 do (push (aref object i) queue))))))
;;   (values
;;    types
;;    (hash-table-count objects)))

;; (#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
;;                (
;;                        cons 674435
;;                        string 95654
;;                        integer 94087
;;                        symbol 73342
;;                        compiled-function 16826
;;                        vector 12107
;;                        subr 1125
;;                        float 461
;;                        marker 174
;;                        char-table 158
;;                        overlay 18
;;                        buffer 72
;;                        hash-table 25
;;                        process 5
;;                        frame 2
;;                        window-configuration 2
;;                        window 2
;;                        obarray 1
;;                 ))
;;    968495)

;; --> ((window window-configuration hash-table frame marker buffer overlay process char-table float vector subr compiled-function string cons integer symbol)
;;      964895)


(defvar inspect:*inspectors* (make-hash-table))

(defmacro inspect:define-inspector (type (object) &body body)
  `(setf (gethash ',type *inspectors*)
         (lambda (,object) (block ,type ,@body))))



(defun inspect:function-parameter-list (function)
  "Return the parameter list of the emacs FUNCTION."
  (let* ((def   (if (symbolp function)
                    (symbol-function function)
                    function))
         (help  (help-function-arglist def))
         (doc   (documentation function))
         (split (help-split-fundoc doc function)))
    (or help
        (when  (first split) (cdar (read-from-string (first split))))
        split
        :unknown)))


(defun inspect:function-argument-counts (function)
  "Return a cons continaing the minimum and maximum number of arguments
the FUNCTION can take."
  (let* ((args (split-lambda-list-on-keywords
                (maptree (lambda (item)
                           (if (memq item '(&optional &rest))
                               (intern (string-upcase item))
                               item))
                         (function-parameter-list (function enlarge-window)))
                :ordinary))
         (min (length (cdr (assoc '&MANDATORY args)))))
    (if (assoc '&REST args)
        (cons min '&rest)
        (cons min (+ min (length (cdr (assoc '&OPTIONAL args))))))))


(defun inspect:hash-table-slots (hash-table)
  (let ((slots '()))
   (maphash (lambda (k v) (push (cons k v) slots)) hash-table)
   (nreverse slots)))


(defun inspect:vector-slots (vector)
  (let ((slots '()))
    (loop for i below (length vector) do (push (cons i (aref vector i)) slots))
    (nreverse slots)))


(defun inspect:obarray-symbols (obarray)
  (let ((symbols '()))
    (do-symbols (symbol obarray)
      (push symbol symbols))
    (mapcar (let ((i -1))
              (lambda (symbol) (cons (incf i) symbol)))
            (sort symbols (function string<)))))


(defun inspect:buffer-slots (buffer)
  ;; properties are in string
  ;; markers? processes? what else?
  (list (list :string (with-current-buffer buffer (buffer-string)))))


(defparameter inspect:*type-attribute-alist*

  '((cons
     car cdr)

    (string
     ;; text-properties-at
     length
     string-bytes)

    (integer
     identity)

    (float
     identity)

    (symbol
     symbol-name
     (boundp symbol-value)
     (fboundp symbol-function)
     symbol-plist
     function-overload-p)

    (compiled-function
     function-argument-counts
     function-parameter-list)

    (vector
     length
     (:contents inspect:vector-slots))

    (subr
     subr-name
     subr-arity)

    (marker
     marker-buffer
     marker-position
     marker-insertion-type)

    (char-table
     ;; char-table-extra-slot
     ;; char-table-range
     char-table-parent
     char-table-subtype)

    (overlay
     overlay-buffer
     overlay-start
     overlay-end
     overlay-properties)


    (buffer
     buffer-name
     buffer-file-name
     buffer-size
     buffer-base-buffer
     buffer-chars-modified-tick
     buffer-live-p
     buffer-local-variables
     buffer-modified-p
     buffer-modified-tick
     (:contents inspect:buffer-slots))


    (hash-table
     hash-table-test
     hash-table-weakness
     hash-table-count
     hash-table-size
     hash-table-rehash-size
     hash-table-rehash-threshold
     (:contents inspect:hash-table-slots))

    (process
     process-id
     process-name
     process-type
     process-plist
     process-buffer
     process-coding-system
     process-command
     process-contact
     process-datagram-address
     process-filter
     process-filter-multibyte-p
     ;; process-get
     process-inherit-coding-system-flag
     process-live-p
     process-mark
     process-query-on-exit-flag
     process-running-child-p
     process-sentinel
     process-status
     process-exit-status
     process-tty-name)

    (frame

      frame-name
      frame-title

      frame-left       frame-top       frame-width       frame-height
      frame-pixel-left frame-pixel-top frame-pixel-width frame-pixel-height

      frame-left-fringe
      frame-right-fringe

      frame-auto-lower
      frame-auto-raise

      frame-foreground-color
      frame-background-color
      frame-background-mode

      frame-border-color
      frame-border-width

      frame-buffer-list
      frame-buffer-predicate
      frame-cursor-color
      frame-cursor-type

      frame-display
      frame-display-type

      frame-font
      frame-icon-name
      frame-icon-type

      frame-internal-border-width

      frame-line-spacing
      frame-menu-bar-lines
      frame-minibuffer
      frame-modeline
      frame-mouse-color
      frame-parent-id
      frame-window-id
      frame-outer-window-id

      frame-screen-gamma

      frame-horizontal-scroll-bars
      frame-vertical-scroll-bars
      frame-scroll-bar-background
      frame-scroll-bar-foreground
      frame-scroll-bar-width
      frame-current-scroll-bars

      frame-tool-bar-lines
      frame-unsplittable
      frame-visibility
      frame-wait-for-wm

      frame-char-height frame-char-width frame-face-alist
      frame-first-window frame-focus frame-live-p frame-parameters
       frame-pointer-visible-p
      frame-root-window frame-selected-window frame-terminal
      frame-terminal-default-bg-mode frame-visible-p

      window-system
      window-list
      window-tree
      buffer-list)

    (window-configuration
     window-configuration-frame)

    (window
     window-atom-root window-body-size window-body-height
     window-body-width window-buffer window-buffer-height
     window-next-buffers window-prev-buffers window-child
     window-child-count window-top-child window-left-child
     window-last-child window-combination-limit window-combinations
     window-combined-p window-current-scroll-bars window-dedicated-p
     window-deletable-p window-display-table window-dot window-edges
     window-end window-fixed-size-p window-frame window-fringes
     window-full-height-p window-full-width-p window-height
     window-hscroll window-inside-absolute-pixel-edges
     window-inside-edges window-inside-pixel-edges window-left
     window-right window-left-column window-top-line window-line-height
     window-live-p window-margins window-max-delta window-min-delta
     window-min-size window-minibuffer-p window-next-sibling
     window-prev-sibling window-normal-size window-parameters
     window-parent window-pixel-edges window-point
     window-safely-shrinkable-p window-scroll-bars window-size-fixed-p
     window-split-min-size window-splittable-p window-start
     window-state-get window-text-height window-total-height
     window-total-size window-total-width window-use-time
     window-valid-p window-vscroll window-width
     frame-root-window-p)

    (obarray
     (:contents inspect:obarray-symbols)))

  "
An a-list mapping types to a list of attributes.

Each attribute is either:

- a symbol naming a predicate or reader function.

- a list of symbols naming predicate or reader functions;  the rest of
  the list is applied only when the preceding elements return true.

- a list containing the keyword :contents and a symbol naming a
  function that takes an object of the given type, returning an a-list
  of (key . value) for each slot in the object.

")


(defun inspect:max-slot-name-width (slots)
  (loop
     for slot in slots
     maximize (cond
                ((symbolp slot) (length (symbol-name slot)))
                ((stringp slot) (length slot))
                ((atom slot) 0)
                ((eq :contents (first slot)) 0)
                (t (reduce (function max) slot
                           :key (lambda (fun) (length (symbol-name fun))))))))


(defun inspect:insert-description (object)
  (let* ((type  (type-of object))
         (slots (cdr (assoc type inspect:*type-attribute-alist*)))
         (width (inspect:max-slot-name-width slots))
         (fmtstr (format "%%-%ds : %%S\n" width))
         (print-length 10)
         (print-level   4))
    (insert (format "\nObject of type %s\n\n" type))
    (loop
       for slot in slots
       do (cond
            ((symbolp slot)
             (insert (format fmtstr slot (funcall slot object))))
            ((atom slot)
             0)
            ((eq :contents (first slot))
             (insert "Contents:\n")
             (let* ((slots (funcall (second slot) object))
                    (width (inspect:max-slot-name-width
                            (mapcar (lambda (slot) (format "%S" (car slot)))
                                    slots)))
                    (fmtstr (format "[%%%dS] : %%S\n" width)))
               (loop
                  for (key . value) in slots
                  do (insert (format fmtstr key value)))))
            (t
             (loop
                for predicate in slot
                for value = (funcall slot object)
                do (insert (format fmtstr slot value))
                while value))))))



;; (inspect:insert-description '(a . 2))
;; (inspect:insert-description '[(a . 2) 3 4.5 "Hello"])
;; (inspect:insert-description (selected-frame))

