(use-package lsp-mode
  :ensure t
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (progn
          (setq lsp-keymap-prefix "M-RET")
          (setq lsp-log-io t)
          (setq lsp-enable-completion-at-point t)
          (setq lsp-enable-indentation t)
          (setq lsp-before-save-edits t)
          (setq lsp-signature-render-documentation nil)
          )
  :hook (
         ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         (rjsx-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (csharp-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)
         )
  :config
  (setq lsp-clients-elixir-server-executable "/home/maki/Documents/elixir-ls/build/language_server.sh")
  (setq lsp-eldoc-hook nil)
  :commands lsp lsp-deferred)

;; ;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        ;; lsp-ui-flycheck-list-position 'right
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  )

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
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; keybindings
(with-eval-after-load 'hydra
  (defhydra hydra-lsp (:exit t :hint nil :color blue)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)
  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)
  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown)))


(provide 'user-init-lsp)
