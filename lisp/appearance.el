(require 'use-package)

;;;; Code:

(use-package emacs
  :straight (:type built-in)
  :config
  ;; (load-theme 'modus-operandi)
  (load-theme 'modus-vivendi)
  :init
  (setq modus-themes-org-blocks 'gray-background
	modus-themes-fringes nil
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-diffs '(desaturated)
	modus-themes-syntax '(alt-syntax)
	modus-themes-hl-line '(intense)
	modus-themes-paren-match '(intense bold underline)
	modus-themes-mode-line '(moody borderless))
  (setq modus-themes-headings
        (quote ((1 . (rainbow variable-pitch 1.2))
                (2 . (variable-pitch 1.2))
		(3 . (variable-pitch 1.1))
		(t . (monochrome))))))

(use-package doom-modeline
  :init
  ;; show doom-modeline at the same time with dashboard
  (add-hook 'emacs-startup-hook 'doom-modeline-mode -100)
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
(setq-default fill-column 88)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; disable macos native fullscreen
(if (eq system-type 'darwin)
    (setq ns-use-native-fullscreen nil))

;; nice dashboard on startup
(use-package dashboard
  :config (setq initial-buffer-choice (lambda ()
					(get-buffer "*dashboard*")))
  ;; (setq dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (setq dashboard-center-content t
	dashboard-startup-banner 2
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-items '((agenda . 10) (projects . 7))
	dashboard-set-navigator t)
  (dashboard-setup-startup-hook))


(provide 'appearance)
;;; appearance.el ends here
