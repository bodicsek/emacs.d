(setq gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold gc-cons-threshold-orig)))

;; ======================== set font ======================
(if (eq system-type 'windows-nt)
    ;; fonts for windows
    (progn
      (add-to-list 'initial-frame-alist '(font . "Lucida Console-12"))
      (add-to-list 'default-frame-alist '(font . "Lucida Console-12")))
  ;; fonts for linux
  (set-frame-font "DejaVu Sans Mono-10" nil t))

;; ==================== initial setup ======================
(setq-default truncate-lines t         ;;always truncate lines
              indent-tabs-mode nil     ;;never use tabs for indentation
              )

(setq
      scroll-step 1                    ;;line by line scrolling
      scroll-conservatively 10000
      auto-window-vscroll nil
      backup-inhibited t               ;; disable backup files
      auto-save-default nil            ;; disable auto-save
      inhibit-startup-screen t         ;; disable startup screen
      initial-major-mode 'text-mode    ;; text-mode in *scratch* buffer
      revert-without-query '(".*")     ;; revert buffer without prompt
      kill-whole-line t                ;; line is killed new line inclusive
      browse-url-generic-program (executable-find "chrome") ;; default browser
      ;browse-url-browser-function 'browse-url-generic
      gnus-init-file "~/.emacs.d/gnus.init.el"                ;; gnus init file
      visible-bell 1                   ;; disable bell
      org-notes-dir "~/ownCloud/OrgNotes"
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
        show-paren-mode
        winner-mode))

(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p #'y-or-n-p)          ;; only y and n

(global-set-key (kbd "M--") #'pop-tag-mark)

(defalias 'hr  #'highlight-regexp)
(defalias 'uhr #'unhighlight-regexp)
(defalias 'hf  #'hexl-find-file)
(defalias 'hm  #'hexl-mode)
(defalias 'wm  #'whitespace-mode)
(defalias 'rb  #'revert-buffer)
(defalias 'nb  #'rename-buffer)
(defalias 'br  #'browse-url)

(server-start)

;; themes coming with emacs
;;
;; adwaita-theme.el
;; deeper-blue-theme.el
;; dichromacy-theme.el
;; leuven-theme.el
;; light-blue-theme.el
;; manoj-dark-theme.el
;; misterioso-theme.el
;; tango-dark-theme.el
;; tango-theme.el
;; tsdh-dark-theme.el
;; tsdh-light-theme.el
;; wheatgrass-theme.el
;; whiteboard-theme.el
;; wombat-theme.el
(load-theme 'leuven t)

;; ======================== add path for the extra libraries ==============

(setq load-path (cons "~/.emacs.d/libraries" load-path))

;; =========== setting up min package requirements ================

(require 'package)
(package-initialize)
(mapc (lambda (p) (push p package-archives))
      '(("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-pinned-packages
      '((haskell-mode . "melpa-stable")
        (hindent      . "melpa-stable")
        (shm          . "melpa-stable")
        (ghc          . "melpa-stable")
        (company-ghc  . "melpa-stable")))
(load-file "~/.emacs.d/libraries/package-extensions.el")
(pe-force-refresh-if-requested)
(pe-install-packages '(use-package f dash names))
(setq use-package-verbose t)

;; ======================== set exec-path  ======================

(use-package path-setup
  :demand    t
  :load-path "~/.emacs.d/libraries"
  :config    (ps-setup "~/.emacs.d/bin"))

;; ================================================================

(use-package which-key
  :config (which-key-mode)
  :ensure t)

(use-package windmove
  :bind (("C-x <up>"    . windmove-up)
         ("C-x <down>"  . windmove-down)
         ("C-x <left>"  . windmove-left)
         ("C-x <right>" . windmove-right)))

(use-package windsize
  :bind (("C-S-M-<left>"  . windsize-left)
         ("C-S-M-<right>" . windsize-right)
         ("C-S-M-<up>"    . windsize-up)
         ("C-S-M-<down>"  . windsize-down))
  :ensure t)

(use-package bm
  :bind (("C-<f2>" . bm-toggle)
         ("<f2>"   . bm-next)
         ("S-<f2>" . bm-previous))
  :ensure t)

(use-package vlf
  :init (require 'vlf-setup)
  :ensure t)

(use-package kpm-list
  :load-path "~/.emacs.d/libraries"
  :bind      ("C-x C-b" . kpm-list))

;; ======================== dired ===============================

(use-package dired
  :commands (dired)
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)
            (setq dired-recursive-copies              'always
                  dired-recursive-deletes             'always
                  dired-dwim-target                   t
                  ls-lisp-dirs-first                  t
                  ls-lisp-ignore-case                 t
                  ls-lisp-verbosity                   '(uid)
                  ls-lisp-format-time-list            '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")
                  global-auto-revert-non-file-buffers t
                  auto-revert-verbose                 nil)))

(use-package dired-subtree
  :after  (dired)
  :config (bind-key "<tab>"     #'dired-subtree-toggle dired-mode-map)
          (bind-key "<backtab>" #'dired-subtree-cycle  dired-mode-map)
          (setq dired-subtree-use-backgrounds nil)
  :ensure t)

(use-package w32-browser
  :if     (eq system-type 'windows-nt)
  :after  (dired)
  :ensure t)

(use-package w32-browser-extensions
  :functions (w32-browser-open-dired-files)
  :if        (eq system-type 'windows-nt)
  :load-path "~/.emacs.d/libraries"
  :after     (w32-browser)
  :config    (bind-key "C-<return>" #'w32-browser-open-dired-files dired-mode-map))

(use-package async
  :after  (dired)
  :config (dired-async-mode 1)
  :ensure t)

(use-package wdired
  :after  (dired)
  :config (bind-key "w" #'wdired-change-to-wdired-mode dired-mode-map))

;; ======================== project handling ===============================

(use-package projectile
  :defer  t
  :init   (add-hook 'after-init-hook #'projectile-global-mode)
  :config (progn

            ;; TODO
            ;; regenerate project *.el TAGS
            ;; (define-key projectile-command-map (kbd "R")
            ;;   #'(lambda ()
            ;;       (interactive)
            ;;       (when (projectile-project-p)
            ;;         (if (projectile-file-exists-p ".project.tags")
            ;;             (setq projectile-idle-timer-seconds 10
            ;;                   projectile-enable-idle-timer  t
            ;;                   projectile-tags-command "etags --regex=@.project.tags *.el test/*.el")
            ;;           (setq projectile-tags-command "find . -name \"*.el\" -print | xargs etags")))
            ;;       (projectile-regenerate-tags)))
            
            ;; keyboard shortcut for project ibuffer
            (define-key projectile-command-map (kbd "C-b") #'projectile-ibuffer)
            
            (setq projectile-use-git-grep t)
            ;; always use external tools for indexing
            (setq projectile-indexing-method 'alien)
            ;; always use cache
            (setq projectile-enable-caching t))
  :ensure t)

;; ======================== ediff ===============================

(use-package ediff
  :functions (ediff-setup-windows-plain)
  :commands  (ediff)
  :config    (setq ediff-window-setup-function #'ediff-setup-windows-plain))  

;; ======================== git ===============================

(use-package magit
  :functions (magit-builtin-completing-read)
  :commands  (magit-init magit-status)
  :init      (progn
               (defalias 'gs #'magit-status)
               (when (eq system-type 'windows-nt)
                 (setenv "GIT_ASKPASS" "git-gui--askpass")))
  :config    (setq magit-completing-read-function #'magit-builtin-completing-read)
  :ensure    t)

;; ======================== intellisense ===============================

(use-package company
  :bind   (("C-SPC"   . company-complete)
           ("C-M-SPC" . company-etags))
  :init   (add-hook 'after-init-hook #'global-company-mode)
  :ensure t)

;; ======================== refactoring ===============================

(use-package multiple-cursors
  :bind   (("C->"     . mc/mark-next-like-this)
           ("C-<"     . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this))
  :ensure t)

;; ======================== xml ===============================

(use-package nxml-mode
  :mode (("\\.csproj\\'" . nxml-mode)
         ("\\.fsproj\\'" . nxml-mode)
         ("\\.xaml\\'"   . nxml-mode)))

(use-package nxml-extensions
  :load-path "~/.emacs.d/libraries"
  :after (nxml-mode))

;; ======================== elisp ===============================

(use-package lisp-mode
  :init (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    (use-package cl-lib)
                    (use-package s
                      :ensure t)
                    (use-package f
                      :ensure t)
                    (use-package names
                      :config (require 'names-dev)
                      :ensure t)
                    (use-package dash
                      :config (dash-enable-font-lock)
                      :ensure t)
                    (use-package cl-lib-highlight
                      :config (progn (cl-lib-highlight-initialize)
                                     (cl-lib-highlight-warn-cl-initialize))
                      :ensure t)
                    ;; Recompile if .elc exists
                    (add-hook (make-local-variable 'after-save-hook)
                              (lambda ()
                                  (byte-recompile-file buffer-file-name)))
                    ;; Enter reindents the current line adds a new line and indents the next line
                    (define-key emacs-lisp-mode-map
                            "\r" #'reindent-then-newline-and-indent))))

(use-package paredit
  :commands (enable-paredit-mode)
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
          (add-hook 'ielm-mode-hook #'enable-paredit-mode))
  :config (progn
            (enable-paredit-mode)
            (define-key paredit-mode-map (kbd "C-<right>")   #'paredit-forward)
            (define-key paredit-mode-map (kbd "C-<left>")    #'paredit-backward)
            (define-key paredit-mode-map (kbd "C-<up>")      #'paredit-backward-up)
            (define-key paredit-mode-map (kbd "C-<down>")    #'paredit-forward-down)
            (define-key paredit-mode-map (kbd "C-M-<right>") #'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-M-<left>")  #'paredit-forward-barf-sexp)
            (define-key paredit-mode-map (kbd "C-M-<up>")    #'paredit-backward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-M-<down>")  #'paredit-backward-barf-sexp))
  :ensure t)

(use-package paredit-menu
  :after (paredit)
  :ensure t)
            
(use-package overseer
  :commands (overseer-mode)
  :ensure t)

(use-package eldoc
  :commands (eldoc-mode)
  :init (progn
          (add-hook 'ielm-mode-hook #'eldoc-mode)))

