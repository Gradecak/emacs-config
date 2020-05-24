
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



(provide 'user-init-extras)
