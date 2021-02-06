(use-package shell-pop
  :ensure
  :bind (("C-c t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-window-size 40)
  (setq shell-pop-autocd-to-working-dir nil)
  (setq  shell-pop-full-span t)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package vterm
  :ensure t)

(add-hook 'vterm-mode-hook (lambda ()
 (display-fill-column-indicator-mode -1)))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh"))

(provide 'user-init-terminal)
