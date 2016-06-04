(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (require 'dash)
  (require 'names))

(define-namespace exec-path-

(defun -dirs (rootpath dirnames)
  "Returns all directories with name in 'dirnames'. 'rootpath' is included if eligible."
  (cl-flet ((eligible-dir-p (d) (-contains? dirnames (f-filename d))))
    (let ((dirs (cons rootpath (f-directories rootpath nil t))))
      (-filter #'eligible-dir-p dirs))))

(defun -map-bin-lib-dirs (rootpath f)
  "Applies 'f' to all 'bin' and 'lib' directories under 'rootpath'. 'rootpath' is included if eligible."
  (-each (-dirs rootpath '("bin" "lib")) f))

(defun -exec-path-setup (rootpath)
  "Adds every 'bin' and 'lib' subdirectories of 'rootpath' to 'exec-path'. 'rootpath' is included if eligible."
  (-map-bin-lib-dirs rootpath
                     (lambda (dir) (push dir exec-path))))

(defun -path-setup (rootpath)
  "Adds every 'bin' and 'lib' subdirectories of 'rootpath' to '$PATH'. 'rootpath' is included if eligible."
  (-map-bin-lib-dirs rootpath
                     (lambda (dir) (setenv "PATH" (concat dir ";" (getenv "PATH"))))))

(defun setup (rootpath)
  (-exec-path-setup rootpath)
  (-path-setup rootpath))

)

(provide 'exec-path)

