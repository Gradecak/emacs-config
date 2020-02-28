;;;;;;;;;;;;;;;;;;;;;
;; disable toolbar ;;
;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
;;;;;;;;;;;;;;;;;;;;;;;
;; enable linum mode ;;
;;;;;;;;;;;;;;;;;;;;;;;
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode t)
  (global-linum-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable paren matching mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable electric pair mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable electric indent mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-indent-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable the splash screen ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use spaces instead of tabs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1) 

(provide 'user-init-default)
