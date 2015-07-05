;;;; various package extension functions

(defun pe-force-refresh-if-requested ()
  "If the --install command line argument is present it refreshes the package content."
  (when (member "--install" command-line-args)
    (package-refresh-contents)))

(defun pe-install-required-packages (package-list)
  "Installs the packages in package-list."
  (mapc
   (lambda (package) 
     (unless (require package nil t)
       (package-install package)
       (require package)))
   package-list))

(provide 'package-extensions)
