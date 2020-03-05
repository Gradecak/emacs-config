;; change font size to 10pt
(set-frame-font "Monospace-14")
;; disable shitty UI elements
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(use-package doom-themes
  :ensure
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t
        doom-neotree-line-spacing 0) ; if nil, italics is universally disabled
      (doom-themes-neotree-config)
      (doom-themes-org-config)
      (doom-themes-visual-bell-config)
      )

(use-package all-the-icons
  :ensure)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(provide 'user-init-appearance)
