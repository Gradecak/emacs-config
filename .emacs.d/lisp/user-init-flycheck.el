(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :init (flycheck-pos-tip-mode))

(use-package helm-flycheck
  :ensure t
  :after flycheck)

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-src-mode-hook 'flyspell-mode)

(provide 'user-init-flycheck)
;;; user-init-flycheck.el ends here
