;; -*- lexical-binding: t; -*-

(require 'use-package)

;;;; Code:

;; (use-package color-theme-sanityinc-tomorrow
;;   :demand t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-bright))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-valley-dark t)
;;   (kaolin-treemacs-theme))

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold nil)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-bluloco-dark") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-bluloco-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (color-theme-sanityinc-tomorrow-night)

(use-package solaire-mode :config (solaire-global-mode +1))

(use-package emacs
  :straight (:type built-in)
  :config
  (setq window-divider-default-places t
        window-divider-default-right-width 4
        window-divider-default-bottom-width 4)
  (window-divider-mode 1)
  (custom-set-faces
   '(window-divider             ((t (:foreground "#7a8294"))))
   '(window-divider-first-pixel ((t (:foreground "#3a3f8c"))))
   '(window-divider-last-pixel  ((t (:foreground "#3a3f4c"))))))

(use-package auto-dim-other-buffers
  :ensure t
  :config
  (auto-dim-other-buffers-mode t))


(use-package doom-modeline
  :init
  ;; show doom-modeline at the same time with dashboard
  (add-hook 'emacs-startup-hook 'doom-modeline-mode -100)
  (setq-default doom-modeline-buffer-file-name-style 'relative-from-project)
  :custom (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 40)
  (doom-modeline-bar-width 1)
  (doom-modeline-env-python-executable "python")
  (doom-modeline-mode 1)
  :hook (dashboard-after-initialize . column-number-mode))

;; Make sure icons work properly
(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight todo keywords in buffers
(use-package hl-todo
  :config (global-hl-todo-mode))

;; highlight git diffs in buffer gutters
(use-package diff-hl
  :config (global-diff-hl-mode))

;; enable fill column line
(setq-default fill-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; nice dashboard on startup
(use-package dashboard
  :config (setq initial-buffer-choice (lambda ()
					(get-buffer "*dashboard*")))
  ;; (setq dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (setq dashboard-center-content t
	dashboard-startup-banner 2
	dashboard-projects-backend 'project-el
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-items '((agenda . 10) (projects . 7))
	dashboard-set-navigator t)
  (dashboard-setup-startup-hook))


(provide 'appearance)
;;; appearance.el ends here
