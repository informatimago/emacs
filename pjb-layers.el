;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-layers.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports a major mode for editing layers of texts.
;;;;    The layers are pages (separated by ^L) which can be merged.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2002-01-01 <PJB> Creation.
;;;;
;;;;BUGS
;;;;    pjb-layers-merge-strings is too slow. Why?
;;;;
;;;;    pjb-layers-mode should display the characters of the layers below
;;;;    the current layer in a lighter color, and readonly.
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
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
;;;;******************************************************************************
(provide 'pjb-layers)

(defvar pjb-layers-mode-map nil
  "Keymap used in pjb-layers mode.")

(if pjb-layers-mode-map
    ()
  (setq pjb-layers-mode-map (make-keymap))
  ;; Bind keys.
  (define-key pjb-layers-mode-map [(prior)]   'pjb-layers-up)
  (define-key pjb-layers-mode-map [(next)]    'pjb-layers-down)
  (define-key pjb-layers-mode-map "\C-o"      'pjb-layers-insert-above)
);;if

(defun pjb-layers-mode ()
  "Major mode for editing layers."
  (interactive)
  (setq major-mode 'pjb-layers-mode)
  (setq mode-name "Pjb-Layers")
  (force-mode-line-update)
  (use-local-map pjb-layers-mode-map)
  (run-hooks 'array-mode-hook)
  (widen)
  (goto-char (point-min))
  (narrow-to-page)
  );;pjb-layers-mode



(defmacro ilayer-index      (ilayer)
  "PRIVATE"
  `(aref ,ilayer 0)
  );;ilayer-index

(defmacro ilayer-length     (ilayer)
  "PRIVATE"
  `(aref ,ilayer 1)
  );;ilayer-length

(defmacro ilayer-chars      (ilayer)
  "PRIVATE"
  `(aref ,ilayer 2)
  );;ilayer-chars

(defmacro ilayer-set-index  (ilayer index)
  "PRIVATE"
  `(aset ,ilayer 0 ,index)
  );;ilayer-set-index

(defmacro ilayer-set-length (ilayer index)
  "PRIVATE"
  `(aset ,ilayer 1 ,index)
  );;ilayer-set-length

(defmacro ilayer-current-char (ilayer)
  "PRIVATE"
  `(let ( (il ,ilayer) )
     (aref (aref il 2) (aref il 0)))
  );;ilayer-current-char

(defmacro ilayer-at-eoln (ilayer)
  "PRIVATE"
  `(= (ilayer-current-char ,ilayer) 10)
  );;ilayer-at-eoln

(defmacro ilayer-at-eof (ilayer)
  "PRIVATE"
  `(let ( (il ,ilayer) )
    (>= (ilayer-index il) (ilayer-length il)))
  );;ilayer-at-eof

(defun ilayer-new  (layer)
  "PRIVATE"
  (let ( (result (make-vector 3 layer)) )
    (ilayer-set-index result 0)
    (ilayer-set-length result (length layer))
    result
    ));;make-ilayer

(defun ilayer-advance (ilayer)
  "PRIVATE"
  (ilayer-set-index ilayer (1+ (ilayer-index ilayer)))
  nil);;ilayer-advance


(defun ilayer-advance-to-eoln (ilayers)
  "PRIVATE"
  (let ( (result nil) (ilayer) )
    (while ilayers
      (setq ilayer  (car ilayers)
            ilayers (cdr ilayers))
      (ilayer-set-index ilayer (1+ (ilayer-index ilayer)))
      (or (ilayer-at-eof  ilayer)
          (ilayer-at-eoln ilayer)
          (setq result (cons ilayer result))))
    (nreverse result)
    ));;ilayer-advance-to-eoln

(defun ilayer-advance-to-eof (ilayers)
  "PRIVATE"
  (let ( (result nil) (ilayer) )
    (while ilayers
      (setq ilayer  (car ilayers)
            ilayers (cdr ilayers))
      (ilayer-set-index ilayer (1+ (ilayer-index ilayer)))
      (or (ilayer-at-eof  ilayer)
          (setq result (cons ilayer result))))
    (nreverse result)
    ));;ilayer-advance-to-eof

(defun ilayer-append-char (ilayer new-char)
  "PRIVATE"
  (aset (ilayer-chars ilayer) (ilayer-index ilayer) new-char)
  (ilayer-set-index ilayer (1+ (ilayer-index ilayer)))
  (when (ilayer-at-eof ilayer)
    (let* ( (old-string (ilayer-chars ilayer))
            (old-length (ilayer-length ilayer))
            (new-length (* 2 old-length))
            (new-string (make-string new-length 0))
            (i 0) )
      (while (< i old-length)
        (aset new-string i (aref old-string i))
        (setq i (1+ i)))
      (aset ilayer 2 new-string)
      (ilayer-set-length ilayer new-length)
      ));;when
  ilayer
  );;ilayer-append-char

(defun ilayer-to-string (ilayer)
  (substring (ilayer-chars ilayer) 0 (ilayer-index ilayer))
  );;ilayer-to-string


(defun pjb-layers-merge-strings (layers)
  "DO:      merge the list of layers.
RETURN:  the merged layer.
NOTE:    each layer is a string of lines.
         the layers are ordered the front-most first.
"
  (let* ( (pjb-layers-not-at-eof
           (apply 'append (mapcar (lambda (layer)
                                      (if (< 0 (length layer))
                                          (list (ilayer-new layer))
                                        nil)) layers)))
          (merge (ilayer-new
                  (make-string (apply 'max
                                      (mapcar (lambda (ilayer)
                                                (ilayer-length ilayer))
                                              pjb-layers-not-at-eof))
                               0)))

          pjb-layers-not-at-eoln
          pjb-layers-with-visible-char
          current-char
          )
    (while pjb-layers-not-at-eof
      (setq pjb-layers-not-at-eoln
            (apply 'append (mapcar (lambda (ilayer)
                                       (if (ilayer-at-eoln ilayer)
                                           nil
                                         (list ilayer)))
                                     pjb-layers-not-at-eof)))
      (while pjb-layers-not-at-eoln
        (setq pjb-layers-with-visible-char pjb-layers-not-at-eoln)
        (setq current-char
              (ilayer-current-char (car pjb-layers-with-visible-char)))
        (setq pjb-layers-with-visible-char (cdr pjb-layers-with-visible-char))
        (while (and (= current-char 32) pjb-layers-with-visible-char)
          (setq current-char
                (ilayer-current-char (car pjb-layers-with-visible-char)))
          (setq pjb-layers-with-visible-char (cdr pjb-layers-with-visible-char))
          );;while
        (ilayer-append-char merge current-char)
        (setq pjb-layers-not-at-eoln
              (ilayer-advance-to-eoln pjb-layers-not-at-eoln))
        );;while not all eoln
      (ilayer-append-char merge  10)
      (setq pjb-layers-not-at-eof( ilayer-advance-to-eof pjb-layers-not-at-eof))
      );;while not all eof
    (ilayer-to-string merge)
    ));;pjb-layers-merge-strings




(defun pjb-layers-up ()
  "DO:      Moves up one layer and narrow to it."
  (interactive)
  (widen)
  (if (search-backward "\f" nil 'at-limit)
      (backward-char)
    (goto-char (point-min)))
  (narrow-to-page)
  );;pjb-layers-up

(defun pjb-layers-down ()
  "DO:      Moves down one layer and narrow to it."
  (interactive)
  (widen)
  (if (search-forward "\f" nil 'at-limit)
      nil
    (goto-char (point-max)))
  (narrow-to-page)
  );;pjb-layers-down

(defun pjb-layers-insert-above ()
  "DO:      Insert a new layer above the current layer, and narrow to it."
  (interactive)
  (widen)
  (unless (search-backward "\f" nil 'at-limit)
    (goto-char (point-min)))
  (insert "\f\n")
  (backward-char)
  (narrow-to-page)
  );;layer-insert-above

(defun pjb-layers-insert-below ()
  "DO:      Insert a new layer below the current layer, and narrow to it."
  (interactive)
  (widen)
  (unless (search-forward "\f" nil 'at-limit)
    (goto-char (point-max)))
  (insert "\n\f")
  (backward-char 2)
  (narrow-to-page)
  );;layer-insert-below


(defun pjb-layers-merge ()
  "DO:      Merges the layers found in the current buffer into a new buffer."
  (interactive)
  (let ( (pmin (point-min))
         (pmax (point-max))
         merged
         )
    (widen)
    (setq merged (pjb-layers-merge-strings (split-string (buffer-string) "\f")))
    (narrow-to-region pmin pmax)
    (switch-to-buffer (get-buffer-create
                       (format "*Merged %s*" (buffer-name (current-buffer)))))
    (erase-buffer)
    (insert merged)
    ));;pjb-layers-merge



;;;; pjb-layers.el                    -- 2002-01-01 19:57:18 -- pascal   ;;;;
