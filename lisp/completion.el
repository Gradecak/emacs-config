(require 'use-package)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode)
  (setq enable-recursive-minibuffers t
	vertico-count 20)
  (defun zap-to-slash ()
    "Replicate the Helm-like behaviour of moving back a directory"
    (interactive)
    (zap-up-to-char -1 ?\/))
  :bind (:map minibuffer-local-map
	      ("C-l" . zap-to-slash)))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key '(:debounce 0.2 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package consult-projectile
  :bind (("C-c p" . consult-projectile)))

(use-package consult-flycheck
  :bind ("C-c e" . consult-flycheck))

(use-package company
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t
	company-idle-delay 0.5
	company-minimum-prefix-length 3)
  (global-company-mode t)
  :bind (("TAB" . company-indent-or-complete-common)
	 :map company-active-map
	 ("C-n" . company-select-next-or-abort)
	 ("C-p" . company-select-previous-or-abort)
	 ("TAB" . company-complete-common-or-cycle)
	 ("<backtab>" . (lambda ()
			  (interactive)
			  (company-complete-common-or-cycle -1)))))

(use-package yasnippet
  :config
  (setq
   yas-verbosity 1
   yas-wrap-around-region t)
  (yas-reload-all)
  (yas-global-mode))

(use-package flycheck
  :init
  (setq flycheck-disabled-checkers '(python-flake8 python-mypy))
  :config
  (global-flycheck-mode)
  ;; we rely on the language servers for these checks
  :custom
  (flycheck-display-errors-delay 0))

(use-package flycheck-aspell
  :config
  (setq ispell-dictionary "en_GB"
	ispell-program-name "aspell"
	ispell-silently-savep t))

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-src-mode-hook 'flyspell-mode)

(use-package engine-mode
  :config
  (defengine ddg
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine pypi
    "https://pypi.org/search/?q=%s"
    :keybinding "p")
  (defengine melpa
    "https://melpa.org/#/?q=%s"
    :keybinding "m")
  (defengine github
    "https://github.com/search?q=%s"
    :keybinding "gh")
  (defengine npm
    "https://www.npmjs.com/search?q=%s"
    :keybinding "n")

  (engine-mode t))

(provide 'completion)
