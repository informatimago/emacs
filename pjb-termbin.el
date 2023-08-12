;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-termbin.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Save buffer or region to termbin.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2023-08-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2023 - 2023
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


(defun save-region-to-termbin (start end)
  "Save the current region by sending it using nc termbin.com port 9999,
using a temporary output buffer, and return the string returned.
The result is also put in the kill ring for easy pasting."
  (interactive "p")
  (let ((output-buffer (generate-new-buffer " *temp*")))
    (call-process-region start end "nc" nil output-buffer nil "termbin.com" "9999")
    (let ((result (string-trim  (remove-if (lambda (ch) (find ch [13 0]))
                                           (with-current-buffer output-buffer
                                                (buffer-substring-no-properties (point-min) (point-max)))))))
      (kill-new result)
      (message "Saved %s (%d %d) to %s" (buffer-name) start end result)
      (kill-buffer output-buffer)
      result)))

(defun save-buffer-to-termbin ()
  "Save the current buffer by sending it using nc termbin.com port 9999,
using a temporary output buffer, and return the string returned.
The result is also put in the kill ring for easy pasting."
  (interactive)
  (save-region-to-termbin (point-min) (point-max)))


(provide 'pjb-termbin)
