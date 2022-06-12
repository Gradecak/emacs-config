(require 'use-package)

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
  :init
  (exec-path-from-shell-initialize))

(use-package undo-tree
  :config
  (setq undo-tree-enable-undo-in-region t
	undo-tree-auto-save-history t
	undo-tree-visualizer-diff nil
	undo-tree-history-directory-alist
	`((".*" . ,(concat user-emacs-directory "undo-history"))))
  (global-undo-tree-mode))


(use-package ag
  :ensure)

;; project management
(use-package projectile
  :config
  (setq projectile-auto-discover nil
	projectile-switch-project-action 'projectile-find-file
        projectile-enable-caching t) ; Enable caching, otherwise find-file is slow
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))
  (projectile-mode 1))

(use-package treemacs
  :after projectile
  :bind (("M-0" . treemacs-display-current-project-exclusively))
  :config
  (treemacs-resize-icons 22)
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-project-follow-mode t))

;; make treemacs be aware of projects
(use-package treemacs-projectile
  :after (treemacs projectile))

;; show git file status in treemacs
(use-package treemacs-magit
  :after (treemacs magit))

(use-package magit
  :custom
  (magit-section-initial-visibility-alist '((stashes . show)
					    (unpushed . show)
					    (pullreqs . show)
					    (issues . show))))
(use-package magit-todos
  :config (magit-todos-mode))

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
  :straight (:type built-in)
  :config
  (setq tramp-verbose 10
	tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000
	vterm-timer-delay 0
        vterm-buffer-name-string "%s"))

(use-package multi-vterm
  :after consult
  :bind (("C-c m" . multi-vterm))
  :config
  (defvar consult--source-terminal
    `(:name "Terminal"
	    :narrow (?t . "Terminal")
	    :enabled (lambda () (and (boundp 'multi-vterm-buffer-list)
                                     (not (null multi-vterm-buffer-list))))
	    :category buffer
	    :state ,#'consult--buffer-preview
	    :action   ,#'consult--buffer-action
	    :items (lambda () (mapcar #'buffer-name multi-vterm-buffer-list)))
    "Terminal candidate for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-terminal 'append))

(provide 'user-init)
