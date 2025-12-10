;; -*- lexical-binding: t; -*-


(use-package shell-maker
  :ensure t
  :config
  (setq shell-maker-prompt-before-killing-buffer nil))

(use-package acp
  :straight (acp :type git :host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :demand t
  :after (acp shell-maker ace-window)
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :bind (("C-c g" . agent-shell)
         :map agent-shell-mode-map
         ("C-q" . bury-buffer))
  :hook (agent-shell-mode . (lambda ()
                              (setq-local mode-line-format nil
                                          header-line-format nil)))
  :config
  (add-to-list 'aw-ignored-buffers #'agent-shell-mode)
  (add-to-list 'display-buffer-alist
               '(".+ Agent @ .+" (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (preserve-size . (t . nil))
                 (slot . 0)
                 (dedicated . t)
                 (window-width . 0.25)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)
                                       (window-size-fixed . width)))))
  (setq
   agent-shell-cursor-command `("cursor-agent-acp" "--config" ,(expand-file-name "~/.config/cursor/acp-config.json"))
   agent-shell-preferred-agent-config (agent-shell-goose-make-agent-config)
   agent-shell-goose-authentication
        ;; the key is ignored and the config file for goose is used
        (agent-shell-make-goose-authentication :openai-api-key "FAKE KEY")))

(provide 'ai)
