;; setup HEAVILY inspired by
;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

(use-package org-gcal
  :ensure t
  :config
  (setq
   org-gcal-client-id "316255193923-6cgb799nuvjjta8qkdvkglbs33oimjoo.apps.googleusercontent.com"
   org-gcal-client-secret "2w1FldZ6hgynQ2iOAmQfw9qo"
   org-gcal-file-alist '(("marijan@veri.ie" . "~/gtd/schedule.org")
                         ("en.dutch#holiday@group.v.calendar.google.com" . "~/gtd/schedule.org"))))



;;client-id: 316255193923-6cgb799nuvjjta8qkdvkglbs33oimjoo.apps.googleusercontent.com
;;client-secret: 2w1FldZ6hgynQ2iOAmQfw9qo

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"
                         "~/gtd/schedule.org"))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")
                              ("N" "Note [notes]" entry
                               (file+headline "~/gtd/notes.org" "Notes")
                               "* NOTE %i%?")))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "NOTE(n)" "|" "DONE(d)" "CANCELLED(c)")))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))
		  
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(setq org-agenda-custom-commands 
      '(("h" "Home" tags-todo "@home"
         ((org-agenda-overriding-header "Home")))
        ("v" "Veri" tags-todo "@veri"
         ((org-agenda-overriding-header "Veri")))
        ("f" "Freelance" tags-todo "@freelance"
         ((org-agenda-overriding-header "Freelance")))))

;; save org clocks to disk and allow them to be fetched on next
;; startup
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; keybindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)


(provide 'user-init-org)
