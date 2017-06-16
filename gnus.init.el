;;; nnir: search mail with various search engines
(require 'nnir)

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
               ;; search mail via imap by typing 'G G' in the Group buffer
               (nnir-search-engine imap)))

(setq starttls-gnutls-program "gnutls-cli")

(setq message-send-mail-function 'smtpmail-send-it
      user-mail-address "david.nabraczky@gmail.com"
      user-full-name "Dávid Nábráczky"
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
				   "david.nabraczky@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;;; custom article summar with user friendly date
(setq gnus-summary-line-format "%U%R%z%B%(%[%-23,23f%]%) %-80,80s %&user-date;\n")

;;; use eww's shr renderer to display html
(setq mm-text-html-renderer 'shr)
(setq shr-color-visible-distance-min 5
      shr-color-visible-luminance-min 100)

;;; allow inline images
(setq gnus-inhibit-images nil)

;;; switching off caching
(setq gnus-agent nil)

;;; labels are handled automatically from 25.1 on
;;; https://fossies.org/diffs/emacs/24.5_vs_25.1/lisp/gnus/nnimap.el-diff.html
;; (setq gnus-extra-headers '(To Newsgroups X-GM-LABELS))

;;; search mail via imap using the gmail search terms
(add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW"))
(setq nnir-imap-default-search-key "gmail")

;; Automate mail fetching.
;;(require 'gnus-demon)
;; Check for new mail once in every this many minutes.
;;(gnus-demon-add-handler 'gnus-demon-scan-news 5 nil)

(require 'auth-source)
(setq auth-sources (cons "~/.emacs.d/gnus.authinfo" auth-sources))

;;TODO: (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
