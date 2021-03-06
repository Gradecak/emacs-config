(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

;; enable notifications for mu4e
(use-package mu4e-alert
  :ensure)
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(setq mu4e-alert-set-window-urgency nil)

(require 'smtpmail)

;; spell check
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
(setq  mu4e-user-mail-address-list '("marijan@veri.ie" "makigradecak@gmail.com" "gradecam@tcd.ie"))

;; cutom function for determining maildir
(defun mu4e-message-maildir-matches (msg rx)
  (when rx
    (if (listp rx)
        ;; if rx is a list, try each one for a match
        (or (mu4e-message-maildir-matches msg (car rx))
            (mu4e-message-maildir-matches msg (cdr rx)))
      ;; not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))

;; use mu4e for e-mail in eamcs
(setq
 mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
 mu4e-view-prefer-html t
  mu4e-update-interval 120              ;; update every 60 seconds
  mail-user-agent 'mu4e-user-agent
  mu4e-html2text-command 'mu4e-shr2text
  mu4e-maildir "~/Maildir"
  mu4e-attachment-dir "~/Downloads"
  mu4e-sent-messages-behavior 'delete
  message-send-mail-function 'smtpmail-send-it
  message-kill-buffer-on-exit t
  mu4e-get-mail-command "offlineimap"
  mu4e-sent-messages-behavior 'delete
  mu4e-hide-index-messages t
  mu4e-compose-context-policy nil) ;; dont ask for context when
                                   ;; composing. use current one


(setq user-mail-address "makigradecak@gmail.com"
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "yourname@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Outlook and gmail contexts
(setq mu4e-contexts
      `(
        ,(make-mu4e-context
          :name "private"
          :enter-func (lambda () (mu4e-message "Entering private context"))
          :leave-func (lambda () (mu4e-message "Leaving private context"))
          ;; we match based on the maildir of the message
          :match-func (lambda (msg)
			                  (when msg (mu4e-message-maildir-matches msg "^/MG" )))
          :vars '( ( user-mail-address . "makigradecak@gmail.com"  )
				           (smtpmail-smtp-user . "makigradecak@gmail.com")
				           ( smtpmail-smtp-server . "smtp.gmail.com" )
                   ( user-full-name . "Marijan Gradcak" )
                   ( mu4e-sent-folder . "/MG/[Gmail].Sent Mail")
				           ( mu4e-trash-folder . "/MG/[Gmail].Trash" )
				           ( mu4e-refile-folder . "/MG/[Gmail].Archive" )
				           ( mu4e-drafts-folder . "/MG/[Gmail].Drafts" )
                   ( mu4e-compose-signature .
                                            (concat
                                             "Marijan Gradecak \n"))))
         ,(make-mu4e-context
           :name "Veri"
           :enter-func (lambda () (mu4e-message "Entering the Veri context"))
		       :leave-func (lambda () (mu4e-message "Leaving Veri context"))
           ;; we match based on the maildir of the message
           :match-func (lambda (msg)
			                   (when msg (mu4e-message-maildir-matches msg "^/Veri" )))
           :vars '( ( user-mail-address . "marijan@veri.ie" )
				            ( smtpmail-smtp-user . "marijan@veri.ie" )
				            ( smtpmail-smtp-server . "smtp.gmail.com" )
                    ( user-full-name . "Marijan Gradecak" )
                    ( mu4e-sent-folder . "/Veri/[Gmail].Sent Mail")
				            ( mu4e-trash-folder . "/Veri/[Gmail].Trash" )
				            ( mu4e-refile-folder . "/Veri/[Gmail].Archive" )
				            ( mu4e-drafts-folder . "/Veri/[Gmail].Drafts" )
                    ( mu4e-compose-signature  .
                                              (concat
                                               "Marijan Gradecak \n"))))
         ,(make-mu4e-context
          :name "TCD"
          :enter-func (lambda () (mu4e-message "Entering Trinity context"))
          :leave-func (lambda () (mu4e-message "Leaving Trinity context"))
          ;; we match based on the maildir of the message
          :match-func (lambda (msg)
			                  (when msg (mu4e-message-maildir-matches msg "^/TCD" )))
          :vars '( ( user-mail-address . "gradecam@tcd.ie")
				           (smtpmail-smtp-user . "gradecam@tcd.ie")
				           ( smtpmail-smtp-server . "smtp.gmail.com" )
                   ( user-full-name . "Marijan Gradcak" )
                   ( mu4e-sent-folder . "/TCD/[Gmail].Sent Mail")
				           ( mu4e-trash-folder . "/TCD/[Gmail].Bin" )
				           ( mu4e-refile-folder . "/TCD/[Gmail].Archive" )
				           ( mu4e-drafts-folder . "/TCD/[Gmail].Drafts" )
                   ( mu4e-compose-signature .
                                            (concat
                                             "Marijan Gradecak \n"))))))


;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(global-set-key (kbd "C-c m")  'mu4e)

(provide 'user-init-email)
