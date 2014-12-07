;; not my code
;; TODO: update credit

(require 'nxml-mode)

(defun nxml-pretty-print ()
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (search-forward-regexp "\>[ \\t]*\<" nil t)
	  (backward-char)
	  (insert "\n"))
	(indent-region (point-min) (point-max)))
    (message "Ah, much better!"))

(provide 'nxml-extensions)
