;; enable recent files
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(recentf-mode)
;; disable macos native fullscreen
(if (eq system-type 'darwin)
    (setq ns-use-native-fullscreen nil))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))
;; disable toolbar ;;
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
;; enable linum mode ;;
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode t)
  (global-linum-mode))
;; enable paren matching mode ;;
(show-paren-mode t)
;; enable electric pair mode ;;
;(electric-pair-mode t)
;; enable electric indent mode ;;
(electric-indent-mode)
;; Disable the splash screen ;;
(setq inhibit-splash-screen t)
;; change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; use spaces instead of tabs ;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; disable file backups
(setq make-backup-files nil)
;; disable line wrapping
(setq-default truncate-lines t)

;; load $PATH $MANPATH adn exec-path from shell
(use-package exec-path-from-shell
  :ensure
  :demand t
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(setq default-directory (file-name-as-directory (substitute-in-file-name "$HOME")))

;; if pasting over selection, delete it and then paste
(delete-selection-mode 1)

;; on buffer save hooks
(add-hook 'before-save-hook (lambda ()
                              (lsp-format-buffer)
                              (delete-trailing-whitespace)
                              (lsp-organize-imports)))

(use-package smartparens
  :ensure t
  :hook
  (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; set the default shell used by ansi-term
(setq explicit-shell-file-name "/usr/bin/zsh")


(provide 'user-init-default)
