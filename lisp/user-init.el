;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :demand t
  :init
  (exec-path-from-shell-initialize))

(use-package emacs
  :straight (:type built-in)
  :hook ((prog-mode . flymake-mode)
         (prog-mode . subword-mode)
         (text-mode . flymake-mode))
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc" display-buffer-at-bottom
                 (window-height . 12)))
  (add-to-list 'display-buffer-alist
               '("*Async Shell Command*" display-buffer-no-window (nil)))
  (setq create-lockfiles nil               ; disable lockfiles
        make-backup-files nil              ; disable backup files
        cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
        indent-tabs-mode nil               ; disable tab indent
        tab-width 2                        ; tab is 2 spaces
        scroll-conservatively 500          ; Avoid recentering when scrolling far
        x-stretch-cursor t                 ; when on a tab stretch the cursor to fit the tab
        scroll-margin 15                    ; Add a margin when scrolling vertically
        use-dialog-box nil ; don't use dialog boxes to ask questions
        custom-file (locate-user-emacs-file "custom-vars.el") ; move custom vars out of init.el
        default-directory (file-name-as-directory (substitute-in-file-name "$HOME/Documents"))
        switch-to-buffer-obey-display-actions t
	next-line-add-newlines t
        shell-command-switch "-ic"
	native-comp-async-report-warnings-errors nil ;; disable annoying native compilation warning
        auto-revert-use-notify nil
        ring-bell-function 'ignore) ;; disable annoying-ass bell

  (add-hook 'prog-mode #'(lambda () (setq indent-tabs-mode nil)))

  (scroll-bar-mode -1)                     ; no scroll bar
  (menu-bar-mode -1)                       ; no menu bar
  (tool-bar-mode -1)                       ; no tool bar
  (delete-selection-mode 1)                ; when pasting over region, delete it
  (global-hl-line-mode)                    ; highlight current line
  (blink-cursor-mode -1)                   ; disable cursor blinking
  (load custom-file 'noerror 'nomessage)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (setq-default cursor-type 'box)
  (setq-default eldoc-echo-area-use-multiline-p nil)
  (setq-default doc-view-resolution 400)   ; make pdf renders less blurry on high res
  (recentf-mode)                           ; enable recent files
  (global-auto-revert-mode 1)              ; auto reload files when changed on disk
  (show-paren-mode t)                      ; highlight parenthesis
  (electric-indent-mode)                   ; indent on RET
  (electric-pair-mode)                     ; make paren pairs
  (fset 'yes-or-no-p 'y-or-n-p)            ; change yes/no to y/n
  ;; swap around option and command keys when using GUI mac  client
  ;;(when (display-graphic-p)
  ;; (setq mac-option-key-is-meta nil
  ;;         mac-command-key-is-meta t
  ;;         mac-command-modifier 'meta
  ;;         mac-option-modifier 'none))
  ;; run garbage collection when focus changes
  (add-function :after after-focus-change-function
		(defun me/garbage-collect-maybe ()
		  (unless (frame-focus-state)
		    (garbage-collect))))
  ;; set the default directory for the auto-generated backup files
  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups"))))
  (let ((auto-save-dir "~/.emacs-saves/"))
    (unless (file-exists-p auto-save-dir)
      (make-directory auto-save-dir)
      (setq auto-save-file-name-transforms
	    `((".*" "~/.emacs-saves/" t)))))
  ;; enable line numbers
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (setq-default display-line-numbers-grow-only t
		display-line-numbers-width 1
		display-line-numbers-type 'relative)
  ;; delete trailing whitespaces on save
  (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace))))

(use-package undo-tree
  :config
  (setq undo-tree-enable-undo-in-region t
	undo-tree-auto-save-history t
	undo-tree-visualizer-diff nil
	undo-tree-history-directory-alist
	`((".*" . ,(concat user-emacs-directory "undo-history"))))
  (global-undo-tree-mode))

(use-package treemacs
  :bind (("M-0" . treemacs-display-current-project-exclusively))
  :config
  (treemacs-resize-icons 22)
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-git-mode 'deferred))

(use-package magit
  :after consult
  :init
  (defun magit-buffers ()
    (seq-remove
     (lambda (r) (not (string-prefix-p "magit:" r)))
     (mapcar 'buffer-name (buffer-list))))
  (defvar consult--source-magit
    `(:name "Magit"
	    :narrow (?g . "Magit")
	    :enabled (lambda () (not (eq (magit-buffers) nil)))
	    :category buffer
	    :state ,#'consult--buffer-preview
	    :action  ,#'consult--buffer-action
	    :items ,#'magit-buffers)
    "github candicates for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-magit 'append)
  :config
  (setq magit-section-initial-visibility-alist '((stashes . show)
						 (unpushed . show)
						 (pullreqs . show)
						 (issues . show))))

;; (use-package magit-todos
;;   :config (magit-todos-mode))

;; a nice way to select symbols
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(use-package tramp
  :straight (:type built-in)
  :config
  (setq tramp-verbose 3
	tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package vterm
  :after consult exec-path-from-shell
  :init
  (defun vterm-buffers ()
    "Get the names of all buffers for which vterm is a major mode."
    (mapcar
     #'buffer-name
     (-filter
      (lambda (buffer) (with-current-buffer buffer (eq major-mode 'vterm-mode)))
      (buffer-list))))
  (defvar consult--source-terminal
    `(:name "Terminal"
	    :narrow (?t . "Terminal")
	    :enabled (lambda () (not (eq (vterm-buffers) nil)))
	    :category buffer
	    :state ,#'consult--buffer-preview
	    :action  ,#'consult--buffer-action
	    :items ,#'vterm-buffers)
    "Terminal candidate for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-terminal 'append)
  :bind (("C-c m" . vterm))
  :config
  (push (list "find-file-other-window" #'find-file-other-window) vterm-eval-cmds)
  (push (list "maybe-refresh-keycloak-token" #'risk/set-token-env-var) vterm-eval-cmds)
  (setq vterm-max-scrollback 10000
	vterm-timer-delay 0
        vterm-buffer-name-string "%s"
	vterm-kill-buffer-on-exit t))

;; emacs startup profiler
(use-package esup)

;; writable grep for modifying the results of grep search
(use-package wgrep)

(use-package project
  :straight (:type built-in)
  :init (setq project-switch-commands 'project-find-file))

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :straight (:type built-in)
  :preface
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
     (proto . ("https://github.com/mitchellh/tree-sitter-proto"))
     (heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
     (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))))
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75))))
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(provide 'user-init)
