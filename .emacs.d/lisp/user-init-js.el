(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))

(use-package js2-refactor
  :ensure t
  :hook
  (rjsq-mode-hook . js2-refactor-mode)
  )

(use-package web-mode
  :ensure t)

(use-package typescript-mode
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  )


(provide 'user-init-js)
