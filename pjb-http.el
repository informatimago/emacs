(defun pjb-url-encode-string (string)
  "Return a string where all the characters but alphanumerics are converted to %xx"
  (with-output-to-string
      (loop for ch across string
         do (princ (format (if (alphanumericp ch) "%c"  "%%%02X") ch)))))
;; (pjb-url-encode-string "2013/08/26-17:51:37")
;; "2013%2F08%2F26%2D17%3A51%3A37"
