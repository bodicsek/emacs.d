(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

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

(setq gnus-summary-line-format "%U%R%z%B%(%[%-23,23f%]%) %-80,80s %&user-date;\n")
(setq mm-text-html-renderer 'shr)
(setq gnus-inhibit-images nil)
(setq gnus-extra-headers '(To Newsgroups X-GM-LABELS))

;; Automate the fetching of mail.
;;(require 'gnus-demon)
;; Check for new mail once in every this many minutes.
;;(gnus-demon-add-handler 'gnus-demon-scan-news 5 nil)

(require 'auth-source)
(setq auth-sources (cons "~/.emacs.d/gnus.authinfo" auth-sources))

;;TODO: (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
