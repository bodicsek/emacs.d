;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'w32-browser))

;;;###autoload
(defun w32-browser-open-dired-files ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let* ((file-list (if (string-equal major-mode "dired-mode")
                       (dired-get-marked-files)
                      (list (buffer-file-name))))
         (do-it     (if (<= (length file-list) 5)
                        t
                      (y-or-n-p "Open more than 5 files?"))))
    (when do-it
      (mapc (lambda (file-path)
              (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" file-path t t)))
            file-list))))

(provide 'w32-browser-extensions)
