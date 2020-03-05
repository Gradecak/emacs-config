;; change font size to 10pt
(add-to-list 'default-frame-alist '(font . "Monospace-8" ))
(set-face-attribute 'default t :font "Monospace-8" )
;;(set-Default-font "Monospace-14")
;; disable shitty UI elements
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

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
