(use-package lsp-mode
  :ensure t
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (progn
          (setq lsp-keymap-prefix "M-RET")
          (setq lsp-completion-enable t)
          (setq lsp-enable-indentation t)
          (setq lsp-before-save-edits t)
          (setq lsp-signature-render-documentation nil)
          (setq lsp-pyls-configuration-sources ["flake8"])
          (setq lsp-pyls-plugins-pycodestyle-enabled nil)
          (setq lsp-pyls-plugins-flake8-enabled t)
          ;; (setq lsp-pyls-plugins-flake8-config "~/.config/flake8")
          (setq lsp-pyls-plugins-pylint-enabled nil)
          (setq lsp-pyls-plugins-pydocstyle-enabled nil)
          (setq lsp-pyls-plugins-autopep8-enabled nil)
          (setq lsp-pyls-server-command "pylsp")
          )
  :hook (
         ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (lsp-mode . lsp-enable-which-key-integration)
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
         (java-mode . lsp-deferred)
         )
  :config
  (lsp-register-custom-settings
   '(("pylsp.configurationSources" '("flake8"))
     ("pylsp.plugins.pyls_mypy.enabled" t t)
     ("pylsp.plugins.pyls_mypy.live_mode" :json-false)
     ("pylsp.plugins.pyls_isort.enabled" t t)
     ("pylsp.plugins.pyls_black.enabled" t t)
     ("pylsp.plugins.pycodestyle.enabled" :json-false)
     ;; ("pylsp.plugins.flake8.config" "~/.config/flake8")
     ("pylsp.plugins.pylint.enabled" :json-false)
     ("pylsp.plugins.pydocstyle.enabled" :json-false)
     ("pylsp.plugins.autopep8.enabled" :json-false)
     ("pylsp.plugins.flake8.enabled" t t)))
  (setq lsp-clients-elixir-server-executable "/home/maki/Downloads/elixir-ls/language_server.sh")
  (setq lsp-eldoc-hook nil)
  :commands lsp lsp-deferred)

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :config
  (require 'dap-java))

(use-package lsp-java 
:ensure t
:config
(add-hook 'java-mode-hook 'lsp)
(setq lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"))

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
