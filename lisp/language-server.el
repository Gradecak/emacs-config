(use-package lsp-mode
  :after company
  :config
  (setq lsp-log-io nil
	lsp-enable-folding nil
	lsp-enable-snippet nil
	lsp-restart 'auto-restart
	lsp-keymap-prefix "M-RET"
	lsp-auto-guess-root t
	lsp-eldoc-enable-hover nil
	lsp-completion-enable t
	lsp-enable-indentation nil
	lsp-before-save-edits nil
	lsp-signature-render-documentation nil
	lsp-disabled-clients '(lsp-pyls)
	lsp-eldoc-hook nil)
  :commands lsp lsp-deferred)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (setq
   lsp-treemacs-errors-position-params '((side . right))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-peek-enable t
	lsp-lens-enable nil
	lsp-ui-doc-include-signature t
	lsp-headerline-breadcrumb-enable nil
	lsp-modeline-code-actions-enable nil
	lsp-eldoc-enable-hover nil
	lsp-signature-render-documentation nil
	lsp-signature-auto-activate nil
	lsp-ui-sideline-enable nil
	lsp-ui-peek-list-width 60
	lsp-ui-peek-peek-height 25))

(defun lsp-format-and-save ()
  (interactive)
  (lsp-format-buffer)
  (save-buffer))

(provide 'language-server)
