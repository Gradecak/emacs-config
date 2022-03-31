(setq create-lockfiles nil			; disable lockfiles
      make-backup-files nil			; disable backup files
      cursor-in-non-selected-windows nil	; Hide the cursor in inactive windows
      indent-tabs-mode nil			; disable tab indent
      tab-width 2				; tab is 2 spaces
      scroll-conservatively 500			; Avoid recentering when scrolling far
      x-stretch-cursor t			; when on a tab stretch the cursor to fit the tab
      scroll-margin 2				; Add a margin when scrolling vertically
      use-dialog-box nil
      custom-file (locate-user-emacs-file "custom-vars.el")
      default-directory (file-name-as-directory (substitute-in-file-name "$HOME")))

(load custom-file 'noerror 'nomessage)
(setq-default truncate-lines t)
(setq-default cursor-type 'bar)
(recentf-mode)                  ; enable recent files
(global-eldoc-mode -1)          ; disable eldoc because it sucks
(global-auto-revert-mode 1)     ; auto reload files when changed on disk
(show-paren-mode t)             ; highlight parenthesis
(electric-indent-mode)          ; indent on RET
(fset 'yes-or-no-p 'y-or-n-p)   ; change yes/no to y/n
(menu-bar-mode -1)              ; no menu bar
(scroll-bar-mode -1)            ; no scroll bar
(tool-bar-mode -1)              ; no tool bar
(delete-selection-mode 1)       ; when pasting over region, delete it
(global-hl-line-mode)		; highlight current line
(blink-cursor-mode -1)		; disable cursor blinking

;; swap around option and command keys when using GUI mac  client
(when (display-graphic-p)
  (setq mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))

;; run garbage collection when focus changes
(add-function :after after-focus-change-function
	      (defun me/garbage-collect-maybe ()
		(unless (frame-focus-state)
		  (garbage-collect))))


;; set the default directory for the auto-generated backup files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;; enable line numbers
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq-default
 display-line-numbers-grow-only t
 display-line-numbers-width 1)

;; delete trailing whitespaces on save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))


;; load $PATH from the shell environment
(use-package exec-path-from-shell
  :ensure
  :init
  (exec-path-from-shell-initialize))

(use-package undo-tree
  :ensure
  :config
  (setq undo-tree-enable-undo-in-region nil
	undo-tree-auto-save-history t
	undo-tree-visualizer-diff t
	undo-tree-history-directory-alist
	`((".*" . ,(concat user-emacs-directory "undo-history"))))
  :init
  (global-undo-tree-mode))


(use-package ag
  :ensure)

;; project management
(use-package projectile
  :ensure t
  :after helm
  :config
  (setq projectile-completion-system 'helm
	projectile-auto-discover nil
	projectile-switch-project-action 'projectile-find-file
        projectile-enable-caching t) ; Enable caching, otherwise find-file is slow
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))
  (projectile-mode 1))

(use-package shell-pop
  :ensure
  :bind (("C-`". shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
	shell-pop-term-shell "/bin/zsh"
	shell-pop-autocd-to-working-dir nil)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package treemacs
  :ensure t
  :after projectile
  :defer t
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
	treemacs-deferred-git-apply-delay      0.5
	treemacs-directory-name-transformer    #'identity
	treemacs-display-in-side-window        t
	treemacs-file-event-delay              5000
	treemacs-file-extension-regex          treemacs-last-period-regex-value
	treemacs-file-follow-delay             0.2
	treemacs-file-name-transformer         #'identity
	treemacs-follow-after-init             t
	treemacs-git-command-pipe              ""
	treemacs-goto-tag-strategy             'refetch-index
	treemacs-indentation                   2
	treemacs-indentation-string            " "
	treemacs-is-never-other-window         nil
	treemacs-max-git-entries               5000
	treemacs-missing-project-action        'ask
	treemacs-no-png-images                 nil
	treemacs-no-delete-other-windows       t
	treemacs-project-follow-cleanup        nil
	treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	treemacs-position                      'left
	treemacs-recenter-distance             0.1
	treemacs-recenter-after-file-follow    nil
	treemacs-recenter-after-tag-follow     nil
	treemacs-recenter-after-project-jump   'always
	treemacs-recenter-after-project-expand 'on-distance
	treemacs-show-cursor                   nil
	treemacs-show-hidden-files             t
	treemacs-silent-filewatch              nil
	treemacs-silent-refresh                nil
	treemacs-sorting                       'alphabetic-asc
	treemacs-space-between-root-nodes      t
	treemacs-tag-follow-cleanup            t
	treemacs-tag-follow-delay              1.5
	treemacs-width                         35
	treemacs-find-workspace-method         'find-for-file-or-pick-first)
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t))

;; make treemacs be aware of projects
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; show git file status in treemacs
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; pop buffer to help discover what keybindings do
(use-package which-key
  :ensure
  :init
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.5
	which-key-idle-secondary-delay 0.05))


;; the best git client
(use-package magit
  :custom
  (magit-section-initial-visibility-alist '((stashes . show)
					    (unpushed . show)
					    (pullreqs . show)
					    (issues . show))))

(use-package magit-todos
  :init (magit-todos-mode))

;; a nice way to select symbols
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

;; move lines up/down
(use-package drag-stuff
  :config
  (global-set-key (kbd "C-s-p") #'drag-stuff-up)
  (global-set-key (kbd "C-s-n") #'drag-stuff-down))

(use-package tramp
  :ensure nil
  :config
  (setq tramp-verbose 10
	tramp-debug-buffer t
	tramp-default-method "ssh"
	tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package helm-tramp
  :after helm
  :bind (("C-c s" . 'helm-tramp))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-hook 'helm-tramp-pre-command-hook '(lambda ()
					    (projectile-mode 0)))
  (add-hook 'helm-tramp-quit-hook '(lambda ()
				     (projectile-mode 1)))
  )

(provide 'user-init)
