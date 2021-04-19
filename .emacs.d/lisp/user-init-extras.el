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

;; (use-package indent-guide
;;   :ensure
;;   :init
;;   (indent-guide-global-mode))

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
  :config
  (setq undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo-history"))))
  :init
  (global-undo-tree-mode))

(use-package restclient
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rcm\\'" . restclient-mode)))

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

(use-package elixir-mode
  :ensure t)

(use-package drag-stuff
  :ensure t
  :config
  (global-set-key (kbd "C-S-p") #'drag-stuff-up)
  (global-set-key (kbd "C-S-n") #'drag-stuff-down))

(provide 'user-init-extras)
