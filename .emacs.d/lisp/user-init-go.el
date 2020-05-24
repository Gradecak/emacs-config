(use-package go-mode
  :ensure t)

(provide 'user-init-go)

(add-hook 'lsp-after-initialize-hook (lambda ()
                                       (setq lsp-gopls-staticcheck t)
                                       (setq lsp-eldoc-render-all t)
                                       (setq lsp-gopls-complete-unimported t)))

