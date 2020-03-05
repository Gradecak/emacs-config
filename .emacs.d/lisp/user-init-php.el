(use-package php-mode
  :ensure
  :init
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))

(provide 'user-init-php)
