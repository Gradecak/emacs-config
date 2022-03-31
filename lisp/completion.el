(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode t)
  :config
  (setq company-tooltip-align-annotations t
	company-dabbrev-downcase nil
	company-dabbrev-code-everywhere t
	company-dabbrev-ignore-case nil
	company-idle-delay 0.5
	company-minimum-prefix-length 3)
  :bind (:map company-active-map
	      ("C-n" . company-select-next-or-abort)
	      ("C-p" . company-select-previous-or-abort)))

(with-eval-after-load 'company
  (define-key company-active-map
              (kbd "TAB")
              #'company-complete-common-or-cycle)
  (define-key company-active-map
              (kbd "<backtab>")
              (lambda ()
                (interactive)
                (company-complete-common-or-cycle -1))))

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
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3))

(use-package helm-flycheck
  :after flycheck)

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-src-mode-hook 'flyspell-mode)

(provide 'completion)
