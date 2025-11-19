;; -*- lexical-binding: t; -*-
(use-package shell-maker
  :ensure t)

(use-package acp
  :straight (acp :type git :host github :repo "xenodium/acp.el"))


;; Open Goose Agent for current project or start it if missing.
(defun mg/agent-shell-open-or-start ()
  "Open the Goose Agent buffer for the current project, or start one if needed."
  (interactive)
  (require 'seq)
  (require 'agent-shell)
  (require 'agent-shell-goose)
  (let* ((buffers (ignore-errors (agent-shell-project-buffers)))
         (goose (seq-find (lambda (b)
                            (string-prefix-p "Goose Agent @ " (if (stringp b) b (buffer-name b))))
                          buffers)))
    (if goose
        (pop-to-buffer goose)
      (agent-shell-goose-start-agent))))

(use-package agent-shell
  :demand t
  :after (acp shell-maker ace-window agent-shell-ui)
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :bind (("C-c g" . mg/agent-shell-open-or-start)
         :map agent-shell-mode-map
         ("C-q" . bury-buffer))
  :hook (agent-shell-mode . (lambda ()
                              (setq-local mode-line-format nil
                                          header-line-format nil)))
  :config
  (add-to-list 'aw-ignored-buffers #'agent-shell-mode)
  (add-to-list 'display-buffer-alist
               '("^\\*?Goose Agent" (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (preserve-size . (t . nil))
                 (slot . 0)
                 (dedicated . t)
                 (window-width . 0.25)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)
                                       (window-size-fixed . width)))))
  (setq agent-shell-goose-authentication
        ;; the key is ignored and the config file for goose is used
        (agent-shell-make-goose-authentication :openai-api-key "FAKE KEY")))

(provide 'ai)
