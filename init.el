(setq gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold gc-cons-threshold-orig)))

;; ======================== set font ======================
(if (eq system-type 'windows-nt)
    ;; fonts for windows
    (progn
      (add-to-list 'initial-frame-alist '(font . "Lucida Console-10"))
      (add-to-list 'default-frame-alist '(font . "Lucida Console-10")))
  ;; fonts for linux
  (set-frame-font "DejaVu Sans Mono-10" nil t))

;; ==================== start server ======================
(require 'server)
(or (eq (server-running-p) t)
    (server-start))

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
      initial-scratch-message nil      ;; disable scratch buffer explanation
      initial-major-mode 'text-mode    ;; text-mode in *scratch* buffer
      revert-without-query '(".*")     ;; revert buffer without prompt
      kill-whole-line t                ;; line is killed new line inclusive
      browse-url-generic-program (executable-find "chrome") ;; default browser
      ;; browse-url-browser-function 'browse-url-generic
      ;; gnus-init-file "~/.emacs.d/gnus.init.el"                ;; gnus init file
      visible-bell 1                   ;; disable bell
      org-notes-dir "~/ownCloudAzure/Notes/OrgNotes")

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

(defalias 'hr  #'highlight-regexp)
(defalias 'uhr #'unhighlight-regexp)
(defalias 'hf  #'hexl-find-file)
(defalias 'hm  #'hexl-mode)
(defalias 'wm  #'whitespace-mode)
(defalias 'rb  #'revert-buffer)
(defalias 'rnb  #'rename-buffer)
(defalias 'br  #'browse-url)

;; (server-start)

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
;; (load-theme 'leuven t)

;; =========== setting up min package requirements ================

(setq load-path (cons "~/.emacs.d/libraries" load-path))

(require 'cl-lib)
(require 'package)
(require 'package-extensions)

(setq package-enable-at-startup nil)
(package-initialize)
(mapc (lambda (p) (push p package-archives))
      '(("melpa"        . "http://melpa.org/packages/")))
(package-force-refresh-if-requested)
(package-install-packages '(use-package f dash names))

(setq use-package-verbose t)

;; ========================= basics =============================

(use-package diminish
  :ensure t)

(use-package atom-dark-theme
  :disabled
  :config (load-theme 'atom-dark t)
  :ensure t)

(use-package darkokai-theme
  :disabled
  :config (load-theme 'darkokai t)
  :ensure t)

(use-package which-key
  :demand t
  :bind ("C-c k" . which-key-show-top-level)
  :config (which-key-mode)
  :ensure t)

(use-package copy-paste-utils
  :load-path "~/.emacs.d/libraries"
  :bind (("C-d"   . copy-paste-selection)
         ("C-S-k" . my-kill-line)))

(use-package undo-tree
  :bind ("C-x u" . undo-tree-visualize)
  :ensure t)

(use-package bm
  :bind (("C-<f2>" . bm-toggle)
         ("<f2>"   . bm-next)
         ("S-<f2>" . bm-previous))
  :ensure t)

(use-package vlf
  :init (require 'vlf-setup)
  :ensure t)

(use-package editorconfig
  :config (editorconfig-mode 1)
  :ensure t)

;; ======================== ivy ===============================

(use-package swiper
  :bind ("C-s" . swiper)
  :ensure t)

(use-package counsel
  :config (counsel-mode 1)
  :diminish (counsel-mode . "Ivy")
  :ensure t)

(use-package counsel-projectile
  :after (projectile)
  :config (counsel-projectile-on)
  :ensure t)

;; ==================== buffer management =======================

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (progn
            (setq ibuffer-expert t
                  ibuffer-show-empty-filter-groups nil)))

;; ==================== window management =======================

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

(use-package zoom-window
  :bind ("C-c z" . zoom-window-zoom)
  :ensure t)

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
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :ensure t)

(use-package dired-narrow
  :after (dired)
  :bind (:map dired-mode-map
              ("n" . dired-narrow))
  :ensure t)

(use-package wdired
  :after  (dired)
  :config (bind-key "w" #'wdired-change-to-wdired-mode dired-mode-map))

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

;; ======================== eshell ===============================

(use-package eshell-prompt-extras
  :config (progn
            (setq eshell-prompt-function #'epe-theme-lambda)
            (setq epe-show-python-info nil)
            (setq epe-git-status "git status --porcelain -b")
            (defun epe-git-unpushed-number ()
              (string-to-number
               (shell-command-to-string "git log @{u}.. --oneline | wc -l"))))
  :ensure t)

;; ======================== project handling ===============================

(use-package projectile
  :defer  t
  :init   (add-hook 'after-init-hook #'projectile-mode)
  :config (progn
            (define-key projectile-command-map (kbd "C-b") #'projectile-ibuffer)
            (setq projectile-use-git-grep t)
            (setq projectile-indexing-method 'alien)
            (setq projectile-enable-caching t))
  :ensure t)

;; ======================== highlight indentation ===============================

(use-package highlight-indent-guides
  :commands (highlight-indent-guides-mode)
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'column)
  :ensure t)

;; ======================== diff ===============================

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
  :config    (progn
               ;; completion function
               (setq magit-completing-read-function #'magit-builtin-completing-read)
               ;; do not use the vc package for git
               (setq vc-handled-backends (delq 'Git vc-handled-backends))
               ;; do not show diffs during commit
               (remove-hook 'server-switch-hook 'magit-commit-diff)
               ;; do not auto revert opened repo buffers
               (magit-auto-revert-mode nil)
               ;; do not ask for confirmation for actions in the list
               (setq magit-no-confirm '(discard reverse stage-all-changes unstage-all-stages))
               ;; workaround on windows https://github.com/magit/with-editor/issues/21
               (when (eq system-type 'windows-nt)
                   (setq with-editor-emacsclient-executable nil)))
  :ensure    t)

;; ======================== intellisense ===============================

(use-package company
  :bind   (("C-SPC"   . company-complete)
           ("C-M-SPC" . company-etags))
  :init   (add-hook 'after-init-hook #'global-company-mode)
  :config (progn
            (setq company-tooltip-align-annotations t))
  :diminish (company-mode . "Cny") 
  :ensure t)

;; ======================== refactoring ===============================

(use-package iedit
  :bind (("C-." . iedit-mode)
         ("C-," . iedit-quit))
  :ensure t)

;; ======================== linting ===============================

(use-package flycheck
  :defer t
  :config (progn
            (setq flycheck-check-syntax-automatically '(save mode-enabled new-line)))
  :ensure t)

;; ======================== documentation at point ===============================

(use-package eldoc
  :commands (eldoc-mode)
  :init (progn
          (add-hook 'ielm-mode-hook #'eldoc-mode)))

;; ======================== xml ===============================

(use-package nxml-mode
  :mode (("\\.csproj\\'" . nxml-mode)
         ("\\.fsproj\\'" . nxml-mode)
         ("\\.xaml\\'"   . nxml-mode)))

(use-package nxml-extensions
  :load-path "~/.emacs.d/libraries"
  :after (nxml-mode))



;; ======================== lisp ===============================

(use-package paredit
  :commands (enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("C-<right>"   . paredit-forward)
              ("C-<left>"    . paredit-backward)
              ("C-<up>"      . paredit-backward-up)
              ("C-<down>"    . paredit-forward-down)
              ("C-M-<right>" . paredit-forward-slurp-sexp)
              ("C-M-<left>"  . paredit-forward-barf-sexp)
              ("C-M-<up>"    . paredit-backward-slurp-sexp)
              ("C-M-<down>"  . paredit-backward-barf-sexp))
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
          (add-hook 'ielm-mode-hook #'enable-paredit-mode))
  :ensure t)

(use-package paredit-menu
  :after (paredit)
  :ensure t)

;; ======================== elisp ===============================

(use-package emacs-lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("RET" . reindent-then-newline-and-indent))
  :init (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    ;; Recompile if .elc exists
                    (add-hook 'after-save-hook
                              (lambda () (byte-recompile-file buffer-file-name))
                              nil
                              t))))

(use-package cl-lib-highlight
  :after (emacs-lisp-mode)
  :functions (cl-lib-highlight-warn-cl-initialize)
  :config (progn
            (cl-lib-highlight-initialize)
            (cl-lib-highlight-warn-cl-initialize))
  :ensure t)
 
(use-package overseer
  :commands (overseer-mode)
  :ensure t)

;; ======================== html ===============================

(use-package web-mode
  :mode "\\.html$"
  :init (add-hook 'web-mode-hook (lambda ()
                                   (set (make-local-variable 'company-backends)
                                        '(company-web-html company-css company-files))))
  :config (progn
            (setq web-mode-enable-auto-closing t)
            (setq web-mode-enable-auto-quoting t)
            ;; eg. comment at the beginning of the file: -*- engine: jsx -*-
            (setq web-mode-enable-engine-detection t)
            ;; set up the company-web-html backend for autocompletion
            (use-package company-web
              :ensure t))
  :ensure t)

;; ======================== js ===============================

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :ensure t)

(use-package js2-refactor
  :after js2-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c j")
  :ensure t)

;; ======================== ts ===============================

(use-package typescript-mode
  :mode "\\.ts$"
  :init (add-hook 'typescript-mode-hook
                  (lambda ()
                    (tide-setup)
                    (flycheck-mode)
                    (eldoc-mode)))
  :ensure t)

(use-package tide
  :commands (tide-setup)
  :config (progn
            ;; formats the buffer before saving
            (add-hook 'before-save-hook #'tide-format-before-save)
            ;; format options
            ;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options
            (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                        :placeOpenBraceOnNewLineForFunctions nil)))
  :ensure t)

;; ======================== restclient ===============================

(use-package restclient
  :mode "\\.rest$"
  :commands (restclient-mode)
  :ensure t)

; ======================== custom set file ===============================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

