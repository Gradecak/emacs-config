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

(provide 'user-init-flycheck)
;;; user-init-flycheck.el ends here
