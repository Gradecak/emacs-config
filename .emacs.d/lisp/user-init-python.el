
(use-package python
  :config
  (setq python-eldoc-get-doc nil))

(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))


(defun auto-pyvenv ()
  (let* ((project-root (projectile-project-name))
         (venvs (concat (getenv "HOME") "/.venvs/"))
         (venv-path (concat venvs project-root)))
    (message venv-path)
    (if (file-directory-p venv-path)
        (pyvenv-activate venv-path)
      (message "no venv for project"))
    )

  )

(with-eval-after-load "lsp-mode"
  (setq lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq lsp-pyls-plugins-flake8-enabled t)
  (setq lsp-pyls-plugins-flake8-config "~/.config/flake8")
  (setq lsp-pyls-plugins-autopep8-enabled nil))

;; (defun lsp-python-cfg ()
;;     (lsp-register-custom-settings '(
;;                                   ("pyls.configurationSources" ("flake8"))
;;                                   ("pyls.plugins.pyls_mypy.enabled" t t)
;;                                   ("pyls.plugins.pyls_mypy.live_mode" :json-false)
;;                                   ("pyls.plugins.pyls_isort.enabled" t t)
;;                                   ;; ("pyls.plugins.autopep8.enabled" :json-false)
;;                                   ;; ("pyls.plugins.pydocstyle.enabled" :json-false)
;;                                   ;; ("pyls.plugins.pycodestyle.enabled" t)
;;                                   ;; ("pyls.plugins.pycodestyle.max_line_length" "80")
;;                                   ;; ("pyls.plugins.rope.enabled" t)
;;                                   ;; ("pyls.plugins.mccabe.enabled" t)
;;                                   ("pyls.plugins.pyls_black.enabled" t t)
;;                                   ;; ("pyls.plugins.pyls_black.line_length" "80")
;;                                   ;; ("pyls.plugins.yapf.enabled" :json-false)
;;                                   ;; ("pyls.plugins.autopep8.enabled" :json-false)
;;                                   ;; ("pyls.plugins.pyflakes.enabled" :json-false)
;;                                   ("pyls.plugins.flake8.enabled" t t))))

;; (add-hook 'lsp-after-initialize-hook 'lsp-python-cfg)
(add-hook 'python-mode-hook #'auto-pyvenv)

(provide 'user-init-python)
