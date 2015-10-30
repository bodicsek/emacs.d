(require 'uuid)

;; ========== replace guids in selection =============
(defun replace-guids (beg end)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "[0-9A-Fa-f]\\{8\\}-[0-9A-Fa-f]\\{4\\}-[0-9A-Fa-f]\\{4\\}-[0-9A-Fa-f]\\{4\\}-[0-9A-Fa-f]\\{12\\}" nil t)
      (backward-char 36)
      (delete-char 36)
      (insert (uuid-string)))))

;; ========== decodes/encodes a string of hex values to/from ascii string (VIPA DB specific) ========
(defun decode-hex-string (hex-string)
  (let ((hex-str (replace-regexp-in-string "00" "" hex-string)))
    (apply #'concat
	   (loop for i from 0 to (- (/ (length hex-str) 2) 1)
		 for hex-byte = (substring hex-str (* 2 i) (* 2 (+ i 1)))
		 collect (format "%c" (string-to-number hex-byte 16))))))

(defun encode-hex-string (str)
  (apply #'concat
	 (loop for i from 0 to (- (length str) 1)
	       collect (format "%x00" (aref str i)))))

(provide 'speed7studio-utils)
