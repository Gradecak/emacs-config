(use-package lsp-mode
  :ensure t
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (progn
          (setq lsp-keymap-prefix "s-l")
          (setq lsp-report-if-no-buffer t)
          (setq lsp-log-io t)
          (setq lsp-enable-completion-at-point t)
          (setq lsp-enable-indentation t)
          (setq lsp-before-save-edits t))
  :hook (
         ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         (rjsx-mode . lsp-deferred)
         (php-mode . lsp-deferred)
        )
  :commands lsp lsp-deferred)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))
;; if you are helm user
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(provide 'user-init-lsp)
