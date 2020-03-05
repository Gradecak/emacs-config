
(use-package python-mode
  :ensure t)

(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))



(defun lsp-set-cfg ()
  (let ((lsp-cfg `(:pyls
                   (:plugins (
                              (:pyls_mypy
                               (:enabled t))
                              (:pyls_black
                               (:enabled t)))))))
      ;; TODO: check lsp--cur-workspace here to decide per server / project
      (lsp--set-configuration lsp-cfg)))

(add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)

(provide 'user-init-python)
