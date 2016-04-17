;; -*- lexical-binding: t -*-

(load-file "package-extensions.el")

(ert-deftest force-refresh-if-requested/request-calls-package-refresh-contents ()
  (with-mock
   (let ((command-line-args '("--install")))
     (expect (mock (package-refresh-contents) => nil)
             (pe-force-refresh-if-requested))
     (expect command-line-args
             '()))))

(ert-deftest install-packages/calls-package-install-with-list-elements ()
  (with-mock
   (stub (require * nil t) => nil)
   (stub (require *) => nil)
   (expect (mock (package-install 'test-package) => nil)
           (pe-install-packages '(test-package)))))

