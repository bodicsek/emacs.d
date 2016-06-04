;; -*- lexical-binding: t -*-

;;;; various package extension functions

(defun package-force-refresh-if-requested ()
  "If the --install command line argument is present it refreshes the package content."
  (let ((install-arg "--install"))
    (when (member install-arg command-line-args)
      (package-refresh-contents)
      (setq command-line-args (delete install-arg command-line-args)))))

(defun package-install-packages (package-list)
  "Installs the packages in package-list."
  (mapc
   (lambda (package) 
     (unless (require package nil t)
       (package-install package)
       (require package)))
   package-list))

(provide 'package-extensions)

