(use-package rjsx-mode
  :ensure t)


;; start rjsx mode on detecting following files
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(provide 'user-init-js)
