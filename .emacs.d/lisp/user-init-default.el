(add-to-list 'exec-path "/usr/local/bin")
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
(electric-pair-mode t)
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
  (when (memq window-system '(mac ns))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH"))))

(provide 'user-init-default)
