;; not my code
;; TODO: update credits

(defvar pe--refresh-done nil
  "True if the package refresh has been done.")

(defun pe-install-required-packages (package-list)
  "Installs the packages in package-list."
  (mapc
   (lambda (package) 
     (unless (require package nil t)
       (when (not pe--refresh-done)
         (package-refresh-contents)
         (setq pe--refresh-done t))
       (package-install package)))
   package-list))

(provide 'package-extensions)
