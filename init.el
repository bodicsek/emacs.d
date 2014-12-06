;; ======================== add path for the extra libraries ==============
;; Emacs Load Path
(setq load-path (cons "~/.emacs.d/libraries" load-path))

;; ======================== set PATH for Windows unix tools  ==============
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setenv "PATH" (concat (getenv "HOME") (concat "/.emacs.d/gnuwin32/bin;" (getenv "PATH"))))
  (add-to-list 'exec-path (concat (getenv "HOME") "/.emacs.d/gnuwin32/bin" )))

;; ======================== set font ======================
(if (or (eq system-type 'windows-nt) (eq system-type 'msdos))
    ;; fonts for windows
    (progn
      (set-face-font 'default "-outline-Consolas-normal-normal-normal-normal-14-*-*-*-c-*-iso8859-1")
      (set-face-font 'bold "-outline-Consolas-bold-normal-normal-normal-14-*-*-*-c-*-iso8859-1")
      (set-face-font 'italic "-outline-Consolas-normal-italic-normal-normal-14-*-*-*-c-*-iso8859-1")
      (set-face-font 'bold-italic "-outline-Consolas-bold-italic-normal-normal-14-*-*-*-c-*-iso8859-1"))
  ;; fonts for linux
  (set-frame-font "DejaVu Sans Mono-10" nil t))

;; ==================== initial setup ======================
(setq-default truncate-lines t         ;;always truncate lines
              indent-tabs-mode nil     ;;never use tabs for indentation
              )

(setq scroll-step 1                    ;;line by line scrolling
      scroll-conservatively 10000
      auto-window-vscroll nil
      backup-inhibited t               ;; disable backup files
      auto-save-default nil            ;; disable auto-save
      inhibit-startup-screen t         ;; disable startup screen
      revert-without-query '(".*")     ;; revert buffer without prompt
      kill-whole-line t                ;; line is killed new line inclusive
      browse-url-generic-program (executable-find "conkeror") ;; default browser
      browse-url-browser-function 'browse-url-generic
      ediff-window-setup-function 'ediff-setup-windows-plain  ;; no new frame for ediff
      gnus-init-file "~/.emacs.d/gnus.init.el"                ;; gnus init file
      )

(mapc (lambda (mode) (when (fboundp mode) (apply mode '(0))))
      '(tool-bar-mode
        scroll-bar-mode))

(mapc (lambda (mode) (when (fboundp mode) (apply mode '(1))))
      '(global-hl-line-mode
        line-number-mode
        column-number-mode
        global-linum-mode
        menu-bar-mode
        delete-selection-mode
        show-paren-mode))

(fset 'yes-or-no-p 'y-or-n-p)          ;; only y and n

(server-start)

;; ======================== package archives ======================
(require 'package)
(package-initialize)
(mapc (lambda (p) (push p package-archives))
      '(("melpa" . "http://melpa.org/packages/")))

(defun install-required-packages (package-list)
  (package-refresh-contents)
  (mapc
   (lambda (package) 
     (unless (require package nil t)
       (package-install package)))
   package-list))

(install-required-packages '(use-package))

;; ======================== use-package ========================
(require 'use-package)

(use-package cl-lib)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package moe-theme
  :idle (load-theme 'moe-light t)
  :ensure t)
	     
(use-package ido
  :init (progn
          (setq ido-enable-flex-matching t)
          (setq ido-everywhere t)
          (setq ido-auto-merge-work-directories-length -1)
          (ido-mode 1)))

(use-package bm
  :bind (("C-<f2>" . bm-toggle)
         ("<f2>"   . bm-next)
         ("S-<f2>" . bm-previous))
  :ensure t)

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

(use-package dired+
  :init (progn
          (put 'dired-find-alternate-file 'disabled nil)
          (eval-after-load "dired"
            '(define-key dired-mode-map "z" 'dired-zip-files))
          (eval-after-load "dired-aux"
            '(add-to-list 'dired-compress-file-suffixes 
                          '("\\.zip\\'" ".zip" "unzip"))))
  :config (setq dired-do-highlighting t dired-dwim-target t)
  :ensure t)

(use-package magit
  :commands (magit-init
             magit-status)
  :init (defalias 'gs 'magit-status)
  :ensure t)
  
(use-package nxml-mode
  :commands nxml-mode
  :mode (("\\.csproj\\'" . nxml-mode)
         ("\\.fsproj\\'" . nxml-mode)
         ("\\.xaml\\'"   . nxml-mode)))

(use-package octave-mode
  :mode ("\\.m$" . octave-mode))

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode)
  :config (progn
            (setq csharp-want-flymake-fixup nil) ;; no flymake
            (use-package omnisharp
              :config (progn
                        ;; (setq omnisharp-debug t)
                        (setq omnisharp-server-executable-path "~/projects/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
                        (define-key csharp-mode-map (kbd "C-o <f12>") 'omnisharp-go-to-definition)
                        (define-key csharp-mode-map (kbd "C-o C-<f12>") 'omnisharp-find-implementations)
                        (define-key csharp-mode-map (kbd "C-o M-S-<f12>") 'omnisharp-find-usages)
                        (define-key csharp-mode-map (kbd "C-o <f6>") 'omnisharp-build-in-emacs)
                        (define-key csharp-mode-map (kbd "C-o C-S-t") 'omnisharp-navigate-to-solution-file)
                        (define-key csharp-mode-map (kbd "C-o C-t") 'omnisharp-navigate-to-solution-member)
                        (define-key csharp-mode-map (kbd "C-o M-ß") 'omnisharp-navigate-to-current-file-member)
                        (define-key csharp-mode-map (kbd "C-o C-SPC") 'omnisharp-auto-complete)
                        (define-key csharp-mode-map (kbd "C-o C-k C-d") 'omnisharp-code-format)
                        (define-key csharp-mode-map (kbd "C-o C-S-r") 'omnisharp-run-code-action-refactoring)
                        (define-key csharp-mode-map (kbd "C-o C-r C-r") 'omnisharp-rename-interactively)
                        (define-key csharp-mode-map (kbd "C-o C-u C-u") '(lambda() (interactive) (omnisharp-unit-test "single")))
                        (define-key csharp-mode-map (kbd "C-o C-u C-f") '(lambda() (interactive) (omnisharp-unit-test "fixture")))
                        (define-key csharp-mode-map (kbd "C-o C-u C-a") '(lambda() (interactive) (omnisharp-unit-test "all")))
                        )
              :ensure t)
            (when (file-exists-p omnisharp-server-executable-path)
              (omnisharp-mode)))
  :ensure t)

(use-package haskell-mode
  :mode (("\\.hs$"  . haskell-mode)
         ("\\.lhs$" . haskell-mode))
  :init (progn
          ;; make the cabal binaries available
          (when (file-directory-p "~/.cabal/bin")
            (progn 
              (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
                (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
                (add-to-list 'exec-path my-cabal-path))
              ;; to be able to use M-. to jump to definitions (requires cabal install hasktags)
              (custom-set-variables '(haskell-tags-on-save t))))
          ;; log ghci
          (custom-set-variables '(haskell-process-log t))
          ;; use 'cabal repl' to use the cabal sandbox if present
          (custom-set-variables '(haskell-process-type 'cabal-repl))
          (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
          (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
  :ensure t)
