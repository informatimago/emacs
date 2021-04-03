
(defun read-inputrc-file (file)
  (let ((expressions '()))
    (flet ((collect-expression (expression)
             (push expression expressions))
           (collect-include (included-expressions)
             (setf expressions (nreverse (revappend expressions
                                                    included-expressions)))))
       (dolist (line (split-string (string-from-file-literally file) "\n"))
         (let* ((sharp-pos (position (character "#") line))
                (line      (if sharp-pos
                               (subseq line 0 sharp-pos)
                               line))
                (colon-pos (position (character ":") line)))
           (if colon-pos
               (let ((left  (substring line 0 colon-pos))
                     (right (substring line (1+ colon-pos))))
                 (collect-expression `(alias
                                       ,(car (read-from-string left))
                                       ,(car (read-from-string right)))))
               (let ((words (split-string line)))
                 (when words
                   (cond
                     ((string= "$include" (first words))
                      (collect-include (ignore-errors
                                        (read-inputrc-file (second words)))))
                     (t
                      (collect-expression (mapcar (lambda (string)
                                                    (car (read-from-string string)))
                                                  words)))))))))
      (nreverse expressions))))


;; (insert (pp (read-inputrc-file "~/.inputrc")))


;; ((set editing-mode emacs)
;;  (set meta-flag on)
;;  (set convert-meta off)
;;  (set input-meta on)
;;  (set output-meta on)
;;  (set bell-style visible)
;;  (alias "[1~" beginning-of-line)
;;  (alias "[3~" delete-char)
;;  (alias "[4~" end-of-line)
;;  (alias "" backward-kill-word)
;;  (alias "[3~" kill-word)
;;  (alias Meta-p "")
;;  (alias Meta-n "")
;;  (alias "p" "")
;;  (alias "n" ""))
;; 
;; (local-set-key "p" "")
;; (kbd 'Meta-p)"[Meta-p]"
;; "Meta-p"

