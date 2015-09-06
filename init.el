;; ======================== add path for the extra libraries ==============
;; Emacs Load Path
(setq load-path (cons "~/.emacs.d/libraries" load-path))

;; ======================== check for Windows unix tools and adjust settings ==============
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (if (file-directory-p "C:/tools/cmder/vendor/msysgit/bin")
      (setenv "GIT_ASKPASS" "git-gui--askpass")
    (message-box "There is no msysgit at 'C:\\tools\\cmder\\vendor\\msysgit\\bin'. Please install cmder with chocolatey!")))

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
      initial-major-mode 'text-mode    ;; text-mode in *scratch* buffer
      revert-without-query '(".*")     ;; revert buffer without prompt
      kill-whole-line t                ;; line is killed new line inclusive
      browse-url-generic-program (executable-find "firefox") ;; default browser
      browse-url-browser-function 'browse-url-generic
      ediff-window-setup-function 'ediff-setup-windows-plain  ;; no new frame for ediff
      gnus-init-file "~/.emacs.d/gnus.init.el"                ;; gnus init file
      visible-bell 1                   ;; disable bell
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

(fset 'yes-or-no-p 'y-or-n-p)          ;; only y and n

(global-set-key (kbd "M--") 'pop-tag-mark)

(defalias 'hr 'highlight-regexp)
(defalias 'uhr 'unhighlight-regexp)
(defalias 'hf 'hexl-find-file)
(defalias 'hm 'hexl-mode)
(defalias 'wm 'whitespace-mode)
(defalias 'rb 'revert-buffer)
(defalias 'nb 'rename-buffer)
(defalias 'br 'browse-url)

(server-start)

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
(require 'package-extensions)
(pe-force-refresh-if-requested)
(pe-install-required-packages '(use-package))

;; ================================================================
(use-package cl-lib)

(use-package try
  :commands try
  :config (package-refresh-contents)
  :ensure t)

(use-package eyebrowse
  :bind      ("C-c e" . eyebrowse-mode)
  :commands  eyebrowse-mode
  :config    (setq eyebrowse-new-workspace t)
  :ensure    t)

(use-package windmove
  :bind (("C-x <up>" . windmove-up)
         ("C-x <down>" . windmove-down)
         ("C-x <left>" . windmove-left)
         ("C-x <right>" . windmove-right)))

(use-package leuven-theme
  :disabled
  :config (progn
            (load-theme 'leuven t)
            (set-face-attribute 'hl-line nil
                                :background "powder blue"))
  :ensure t)

(use-package monokai-theme
  :config (load-theme 'monokai t)
  :ensure t)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido
  :init (progn
          (setq ido-everywhere t)
          (setq ido-enable-flex-matching t)
          (setq ido-auto-merge-work-directories-length -1)
          (setq ido-use-faces nil))
  :config (progn
            (ido-mode 1)
            (use-package flx-ido
              :config (flx-ido-mode 1)
              :ensure t)
            (use-package smex
              :bind (("M-x" . smex)
                     ("M-X" . smex-major-mode-commands)
                     ("C-c C-c M-x" . execute-extended-command))
              :ensure t)
            (use-package ido-ubiquitous
              :config (ido-ubiquitous-mode 1)
              :ensure t)))

(use-package bm
  :bind (("C-<f2>" . bm-toggle)
         ("<f2>"   . bm-next)
         ("S-<f2>" . bm-previous))
  :ensure t)

(use-package dired+
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)
            (setq dired-do-highlighting t dired-dwim-target t)
            (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
            (use-package dired-extensions)
            (define-key dired-mode-map "z" 'dired-zip-files)
            (use-package dired-sort
              :ensure t)
            (use-package w32-browser
              :if (eq system-type 'windows-nt)
              :ensure t))
  :ensure t)

(use-package vlf
  :config (require 'vlf-setup)
  :ensure t)

(use-package company
  :commands company-mode
  :bind   (("C-SPC" . company-complete)
           ("C-M-SPC" . company-etags))
  :init   (progn
            (add-hook 'after-init-hook 'global-company-mode)
            (custom-set-variables
             '(company-backends
               (quote
                (company-ghc
                 company-bbdb
                 company-nxml
                 company-css
                 company-eclim
                 company-semantic
                 company-clang
                 company-xcode
                 company-cmake
                 (company-etags company-capf)
                 (company-dabbrev-code company-gtags company-keywords)
                 company-oddmuse
                 company-files
                 company-dabbrev)))))
  :ensure t)

(use-package projectile
  :init (projectile-global-mode)
  :config (progn
            ;; regenerate project *.el TAGS
            (define-key projectile-command-map (kbd "R")
              #'(lambda ()
                  (interactive)
                  (when (projectile-project-p)
                    (if (projectile-file-exists-p ".project.tags")
                        (setq projectile-idle-timer-seconds 10
                              projectile-enable-idle-timer  t
                              projectile-tags-command "etags --regex=@.project.tags *.el test/*.el")
                      (setq projectile-tags-command "find . -name \"*.el\" -print | xargs etags")))
                  (projectile-regenerate-tags)))
            ;; keyboard shortcut for project ibuffer
            (define-key projectile-command-map (kbd "C-b") 'projectile-ibuffer)
            ;; completion system is helm
            (use-package helm
              :config (progn
                        (setq projectile-completion-system 'helm)
                        (use-package helm-projectile
                          :config (helm-projectile-on)
                          :ensure t))
              :ensure t)
            ;; default action after project selection
            (setq projectile-switch-project-action 'helm-projectile)
            ;; always use external tools for indexing
            (setq projectile-indexing-method 'alien)
            ;; always use cache
            (setq projectile-enable-caching t))
  :ensure t)

(use-package skeletor
  :commands (skeletor-create-project
             skeletor-create-project-at)
  :bind ("C-c p n" . skeletor-create-project-at)
  :config (progn
            (setq skeletor-init-with-git nil)

            (add-to-list 'skeletor-global-substitutions '("__AUTHOR__" . "bodicsek"))

            (skeletor-define-template "haskell-lib-project"
              :title "Haskell library project"
              :substitutions '(("__SYNOPSIS__" . (lambda () (read-string "Synopsis: "))))
              :after-creation (lambda (dir)
                                (skeletor-shell-command "cabal sandbox init")))
            (skeletor-define-template "haskell-lib-test-project"
              :title "Haskell library and test-suite project"
              :substitutions '(("__SYNOPSIS__" . (lambda () (read-string "Synopsis: "))))
              :after-creation (lambda (dir)
                                (skeletor-shell-command "cabal sandbox init")))
            (skeletor-define-template "haskell-exe-lib-test-project"
              :title "Haskell executable, library and test-suite project"
              :substitutions '(("__SYNOPSIS__" . (lambda () (read-string "Synopsis: "))))
              :after-creation (lambda (dir)
                                (skeletor-shell-command "cabal sandbox init"))))
  :ensure t)

(use-package overseer
  :ensure t)

(use-package paredit
  :commands enable-paredit-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
          (add-hook 'lisp-mode-hook 'enable-paredit-mode)
          (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
          (add-hook 'ielm-mode-hook 'enable-paredit-mode))
  :config (progn
            (use-package paredit-menu
              :ensure t)
            (define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward)
            (define-key paredit-mode-map (kbd "C-<left>") 'paredit-backward)
            (define-key paredit-mode-map (kbd "C-<up>") 'paredit-backward-up)
            (define-key paredit-mode-map (kbd "C-<down>") 'paredit-forward-down)
            (define-key paredit-mode-map (kbd "C-M-<right>") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-M-<left>") 'paredit-forward-barf-sexp)
            (define-key paredit-mode-map (kbd "C-M-<up>") 'paredit-backward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-M-<down>") 'paredit-backward-barf-sexp))
  :ensure t)

(use-package lisp-mode
  :init (add-hook
         'emacs-lisp-mode-hook
         #'(lambda ()
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
               :ensure t))))

(use-package nxml-mode
  :mode (("\\.csproj\\'" . nxml-mode)
         ("\\.fsproj\\'" . nxml-mode)
         ("\\.xaml\\'"   . nxml-mode))
  :config (use-package nxml-extensions))

(use-package octave-mode
  :mode ("\\.m$" . octave-mode))

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode)
  :config (progn
            (setq csharp-want-flymake-fixup nil) ;; no flymake
            (use-package omnisharp
              :config (progn
                        ;; (setq omnisharp-debug t)
                        (setq omnisharp-server-executable-path "~/.emacs.d/bin/omnisharp-server/OmniSharp.exe")
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
  :commands haskell-mode
  :init (progn
          (use-package hindent
            :ensure t)
          (use-package shm
            :init (progn
                    (add-hook 'haskell-mode-hook 'structured-haskell-mode))
            :config (progn
                      ;;(set-face-background 'shm-current-face "#eee8d5")
                      ;;(set-face-background 'shm-quarantine-face "lemonchiffon")
                      )
            :ensure t)
          ;; ghc-mod (requires cabal install ghc-mod)
          (use-package ghc
            :commands (ghc-init ghc-debug)
            :init (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
            :ensure t)
          ;; company-ghc (requires cabal install ghc-mod)
          (use-package company-ghc
            :config (progn
                      (add-to-list 'company-backends 'company-ghc)
                      (setq company-ghc-show-info t))
            :ensure t))
  :config (progn
            ;; make the cabal binaries available
            (when (file-directory-p "~/.cabal/bin")
              (progn
                (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
                  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
                  (add-to-list 'exec-path my-cabal-path))
                ;; to be able to use M-. to jump to definitions (requires cabal install hasktags)
                ;; and the files must be in a cabal project
                (setq haskell-tags-on-save t)
                ;; M-x haskell-mode-stylish-buffer (requires cabal install stylish-haskell)
                ))
            ;; log ghci
            (setq haskell-process-log t)
            ;; use 'cabal repl' to use the cabal sandbox if present
            (setq haskell-process-type 'cabal-repl))
  :ensure t)

(use-package slime
  :commands slime
  :config (progn
            (if (eq system-type 'windows-nt)
                (setq inferior-lisp-program "~/.emacs.d/bin/ccl/wx86cl64.exe")
              (setq inferior-lisp-program "~/.emacs.d/bin/ccl/lx86cl"))
            (slime-setup '(slime-fancy)))
  :ensure t)

(use-package magit
  :commands (magit-init
             magit-status)
  :init (progn
          (defalias 'gs 'magit-status)
          (setq magit-completing-read-function 'magit-ido-completing-read)
          (setq magit-last-seen-setup-instructions "1.4.0"))
  :ensure t)

(use-package deft
  :commands deft
  :config (progn
            (setq deft-extension "org")
            (setq deft-directory (concat "p:/" (user-login-name) "/Dropbox/OrgNotes/Notes"))
            (setq deft-text-mode 'org-mode))
  :ensure t)

(use-package org
  :commands org-mode
  :config (let* ((dropbox-dir (concat "p:/" (user-login-name) "/Dropbox/OrgNotes"))
                 (dropbox-org-dir (concat dropbox-dir "/Notes"))
                 (dropbox-mobileorg-dir (concat dropbox-dir "/MobileOrg"))
                 (dropbox-mobileorg-pullfile (concat dropbox-org-dir "/links.org")))
            (setq org-directory dropbox-org-dir)
            (setq org-agenda-files (list dropbox-org-dir))
            (setq org-mobile-directory dropbox-mobileorg-dir)
            (setq org-mobile-inbox-for-pull dropbox-mobileorg-pullfile)
            (setq org-todo-keywords '((sequence "TODO" "ACTIVE" "|" "DONE")))
            (setq org-completion-use-ido t))
  :ensure t)

(use-package hackernews
  :commands hackernews
  :init (defalias 'hn 'hackernews)
  :ensure t)

(use-package twittering-mode
  :commands twittering-mode
  :init (defalias 'twit 'twittering-mode)
  :ensure t)

(use-package elfeed
  :commands elfeed
  :init (defalias 'rss 'elfeed)
  :config (setq elfeed-feeds
                '(
                  "http://hataratkelo.blog.hu/rss2"
                  "http://www.gog.com/en/frontpage/rss"
                  "http://emberileg.tumblr.com/rss"
                  "http://cvikli.tumblr.com/rss"
                  "http://iddqd.blog.hu/rss2"
                  "http://szakmailag.tumblr.com/rss"
                  "http://www.napinemetteszt.com/feed"
                  "http://mivanvelem.hu/?feed=rss2"
                  ("http://444.hu/feed/" news)
                  ("http://cink.hu/rss" news)
                  ("http://www.wired.com/news/feeds/rss2/0,2610,,00.xml" news)
                  ("http://www.theverge.com/rss/index.xml" news)
                  ("http://www.hwsw.hu/xml/latest_news_rss.xml" dev news)
                  ("http://hup.hu/backend_ext.php" dev news)
                  ("http://rss.slashdot.org/Slashdot/slashdotDevelopers" dev news)
                  ("https://planet.haskell.org/atom.xml" dev haskell)
                  ("http://lambda.jstolarek.com/feed/" dev haskell)
                  ("http://planet.emacsen.org/atom.xml" dev emacs)
                  ("http://www.masteringemacs.org/feed/" dev emacs)
                  ("http://planet.lisp.org/rss20.xml" dev lisp)
                  ("http://plastik.hu/feed/" dev)
                  ("http://www.joelonsoftware.com/rss.xml" dev)
                  ("http://www.martinfowler.com/bliki/bliki.atom" dev)
                  ("http://feeds2.feedburner.com/ElegantCode" dev)
                  ("http://www.go-mono.com/monologue/index.rss" dev .net)
                  ("http://blog.gdinwiddie.com/feed/atom/" dev)
                  ("http://android-developers.blogspot.com/feeds/posts/default?alt=rss" dev android)
                  ("http://herbsutter.wordpress.com/feed/" dev c++)
                  ("http://www.i-programmer.info/index.php?option=com_ninjarsssyndicator&feed_id=3&format=raw" dev)
                  ("http://tifyty.wordpress.com/feed/" dev)
                  ("http://blogs.msdn.com/oldnewthing/rss.xml" dev windows)
                  ("http://www.raspberrypi.org/feed" gadget)
                  ("http://tirania.org/blog/miguel.rss2" dev .net)
                  ("http://feeds.feedburner.com/JonSkeetCodingBlog" dev .net)
                  ("http://xach.livejournal.com/data/atom" dev lisp)
                  ("http://feeds.feedburner.com/codinghorror/" dev)
                  ("http://mikehadlow.blogspot.com/feeds/posts/default?alt=rss" dev)
                  ("http://channel9.msdn.com/Feeds/RSS/" dev microsoft)
                  ("http://queue.acm.org/rss/feeds/queuecontent.xml" dev)
                  ("http://blog.8thlight.com/feed/atom.xml" dev)
                  ("http://lambda-the-ultimate.org/rss.xml" dev)
                  ("http://ericlippert.com/feed/" dev)
                  ))
  :ensure t)

(use-package mu4e
  :if (not (eq system-type 'windows-nt))
  :load-path "~/.emacs.d/libraries/mu4e"
  :commands mu4e
  :config (progn
            ;; default
            ;; (setq mu4e-maildir "~/Maildir")

            (setq mu4e-drafts-folder "/[Gmail].Drafts")
            (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
            (setq mu4e-trash-folder  "/[Gmail].Trash")

            ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
            (setq mu4e-sent-messages-behavior 'delete)

            ;; setup some handy shortcuts
            ;; you can quickly switch to your Inbox -- press ``ji''
            ;; then, when you want archive some messages, move them to
            ;; the 'All Mail' folder by pressing ``ma''.
            (setq mu4e-maildir-shortcuts
                  '( ("/INBOX"               . ?i)
                     ("/[Gmail].Sent Mail"   . ?s)
                     ("/[Gmail].Trash"       . ?t)
                     ("/[Gmail].All Mail"    . ?a)))

            ;; appending own bookmarks
            ;; to invoke a search bookmark press b and letter defined here
            (add-to-list 'mu4e-bookmarks
                         '("flag:flagged" "Flagged messages" ?f)
                         t)

            ;; allow for updating mail using 'U' in the main view:
            (setq mu4e-get-mail-command "offlineimap")

            ;; enable inline images
            (setq mu4e-view-show-images t)
            ;; use imagemagick, if available
            (when (fboundp 'imagemagick-register-types)
              (imagemagick-register-types))

            ;; rendering html mails
            ;; (setq mu4e-html2text-command "w3m -dump -T text/html") ;; requires package w3m
            ;; (setq mu4e-html2text-command "html2text -utf8 -width 72") ;; requires package html2text
            ;; (setq mu4e-html2text-command "html2markdown | grep -v '&nbsp_place_holder;'") ;; requires package python-html2text
            (use-package mu4e-contrib)
            (setq mu4e-html2text-command 'mu4e-shr2text) ;; requires emacs compiled with libxml2 support

            ;; something about ourselves
            (setq
             user-mail-address "david.nabraczky@gmail.com"
             user-full-name  "David Nabraczky")

            ;; make sure the gnutls command line utils are installed
            ;; package 'gnutls-bin' in Debian/Ubuntu
            (use-package smtpmail
              :config (setq message-send-mail-function 'smtpmail-send-it
                            smtpmail-stream-type 'starttls
                            smtpmail-default-smtp-server "smtp.gmail.com"
                            smtpmail-smtp-server "smtp.gmail.com"
                            smtpmail-smtp-service 587))

            ;; show mu4e maildirs summary in mu4e-main-view
            (use-package mu4e-maildirs-extension
              :config (mu4e-maildirs-extension)
              :ensure t)

            (setq mu4e-hide-index-messages t)
            
            ;; don't keep message buffers around
            (setq message-kill-buffer-on-exit t)))

(use-package g-music
  :commands (g-music)
  :init (progn (use-package s
                 :ensure t)
               (use-package dash
                 :ensure t)
               (use-package names
                 :ensure t)
               (use-package request
                 :ensure t)
               (use-package libmpdee
                 :ensure t)
               (if (or (eq system-type 'windows-nt) (eq system-type 'msdos))
                   (add-hook 'g-music-mode-hook
                             '(lambda ()
                                (setq g-music-proxy-process
                                      "c:/tools/python2/python.exe c:/tools/python2/Scripts/GMusicProxy --conf c:/Users/DNabraczky/.gmusicproxy.cfg"))))))

(use-package copenrelational
  :bind ("<f4>" . c-open-relational-file))

(use-package copy-paste-utils
  :bind (("C-d" . copy-paste-selection)
         ("C-k" . my-kill-line)))

(use-package speed7studio-utils
  :commands (replace-guids
             decode-hex-string
             encode-hex-string)
  :config (use-package uuid
            :ensure t))

