;; yaml mode
(use-package yaml-mode
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :hook
  (yaml-mode-hook . (lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package json-mode
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package terraform-mode
  :ensure
  :init
  (custom-set-variables
   '(terraform-indent-level 4)))

(use-package indent-guide
  :ensure
  :init
  (indent-guide-global-mode))

(use-package hl-todo
  :ensure
  :init
  (global-hl-todo-mode))

(use-package hungry-delete
  :ensure
  :init
  (global-hungry-delete-mode))

(use-package groovy-mode
  :ensure t)

(use-package undo-tree
  :ensure
  :init
  (global-undo-tree-mode))

(use-package restclient
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rst\\'" . restclient-mode)))

(use-package dockerfile-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config (add-hook 'gfm-mode-hook  (smartparens-global-mode)))

(use-package gh-md
  :ensure t)

(use-package shell-pop
  :ensure 
  :bind (("C-c t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-window-size 40)
  (setq  shell-pop-full-span t)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(provide 'user-init-extras)
