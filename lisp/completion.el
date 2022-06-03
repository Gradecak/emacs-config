(use-package company
  :ensure t
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

(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flycheck
  :config (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3))

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
