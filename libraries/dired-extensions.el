(defun dired-zip-files (zip-file)
  (interactive "sEnter zip file name: ")
  (shell-command
   (concat "zip -r "
	   zip-file
	   " "
	   (mapconcat 
	    (lambda (fname) (format "%s" (file-relative-name fname)))
	    (dired-get-marked-files) " ")))
  (revert-buffer))

(provide 'dired-extensions)
