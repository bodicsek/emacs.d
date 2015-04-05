;; not my code
;; TODO: update credits

(defun install-required-packages (package-list)
  ;;(package-refresh-contents)
  (mapc
   (lambda (package) 
     (unless (require package nil t)
       (package-install package)))
   package-list))

(provide 'package-extensions)
