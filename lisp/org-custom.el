(require 'org-capture)
(require 'org-clock)
(require 'org-agenda)


(defcustom org-home-dir "~/Documents/org/"
  "Home directory for org files"
  :group 'org
  :type 'string)

(defun org-file (file-name)
  (interactive)
  (expand-file-name file-name org-home-dir))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

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

(use-package org
  :config
  (setq org-agenda-files (file-expand-wildcards (org-file "*.org"))
	org-capture-templates `(("t" "Todo" entry
				 (file+headline ,(org-file "inbox.org") "Tasks")
				 "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"))
	org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "IN_PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)"))
	org-enforce-todo-dependencies t
	org-clock-persist 'history
	org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1))
	org-outline-path-complete-in-steps nil
	org-refile-use-outline-path t
	org-agenda-custom-commands
	'(("p" "Personal" tags-todo "@personal"
           ((org-agenda-overriding-header "Personal")))
          ("b" "Bloomon" tags-todo "@bloomon"
           ((org-agenda-overriding-header "Bloomon"))))
	org-agenda-window-setup 'current-window
	org-deadline-warning-days 7
	org-agenda-span 'fortnight
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-agenda-sorting-strategy '((agenda deadline-up priority-down)
				      (todo priority-down category-keep)
				      (tags priority-down category-keep)
				      (search category-keep))
	org-tag-alist '(("@bloomon" . ?b)
			("@personal" . ?p)))
  ;; save org clocks to disk and allow them to be fetched on next startup
  (org-clock-persistence-insinuate))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package org-roam
  :after org
  :bind (("C-c r" . 'org-roam-capture))
  :config
  (setq org-roam-directory (expand-file-name "roam" org-home-dir)
	org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

(use-package ox-rst)

(provide 'org-custom)
