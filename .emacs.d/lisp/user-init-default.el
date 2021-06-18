;; enable recent files
;; (setq recentf-max-menu-items 25)
;; (setq recentf-max-saved-items 25)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-auto-revert-mode t)

;; disable lockfiles. super annoying when working with webpack as it causes it to crash
(setq create-lockfiles nil)

(recentf-mode)
;; disable macos native fullscreen
(if (eq system-type 'darwin)
    (setq ns-use-native-fullscreen nil))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))
;; disable eldoc because its annoying
(global-eldoc-mode -1)
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
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(setq-default fill-column 88)
(add-hook 'prog-mode-hook (display-fill-column-indicator-mode))


(provide 'user-init-default)
