(load-file "w32-browser-extensions.el")

(ert-deftest open-dired-files/calls-w32-shell-execute ()
  (with-mock
   (stub dired-get-marked-files => '("a.txt" "b.txt"))
   (let ((major-mode 'dired-mode))
     (expect (mock (w32-shell-execute "open" *) :times 2)
             (w32-browser-open-dired-files)))))
