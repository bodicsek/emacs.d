;;;###autoload
(defun copy-paste-selection (beg end)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (let ((thing-bounds (bounds-of-thing-at-point 'line)))
		   (list (car thing-bounds) (cdr thing-bounds)))))
  (save-excursion
    (goto-char end)
    (insert (buffer-substring beg end))))

;;;###autoload
(defun my-kill-line ()
  (interactive)
  (progn
    (move-beginning-of-line nil)
    (kill-line)))

(provide 'copy-paste-utils)
