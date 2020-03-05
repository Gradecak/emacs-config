
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

(use-package indent-guide
  :ensure
  :init
  (indent-guide-global-mode))

(use-package hl-todo
  :ensure
  :init
  (global-hl-todo-mode))

(provide 'user-init-extras)
