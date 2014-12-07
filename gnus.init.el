(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

;; Debian package gnutls-bin contains gnutls-cli
(setq starttls-gnutls-program "gnutls-cli")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
				   "david.nabraczky@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
      ;;gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; requires Debian package w3m
(setq mm-text-html-renderer 'w3m)
(setq gnus-inhibit-images nil)

;; Automate the fetching of mail.
;;(require 'gnus-demon)
;; Check for new mail once in every this many minutes.
;;(gnus-demon-add-handler 'gnus-demon-scan-news 5 nil)

(require 'auth-source)
(setq auth-sources (cons "~/.emacs.d/gnus.authinfo" auth-sources))

;;TODO: (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
