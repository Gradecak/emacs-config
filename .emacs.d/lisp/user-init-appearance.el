;; change font size to 10pt
(set-default-font "Monospace-8")
(use-package doom-themes
  :ensure
  :init
  (load-theme 'doom-one t)
  :config
  (progn
    (doom-themes-neotree-config)
    (setq doom-neotree-line-spacing 0)
    (doom-themes-org-config)))

(use-package all-the-icons
  :ensure)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(provide 'user-init-appearance)
