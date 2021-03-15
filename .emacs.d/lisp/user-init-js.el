(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (setq js-indent-level 2))

(use-package js2-refactor
  :ensure t
  :hook
  (rjsq-mode-hook . js2-refactor-mode)
  :init
  (setq js2-basic-offset 2)
  )

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package typescript-mode
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (setq typescript-indent-level 2))


(provide 'user-init-js)
