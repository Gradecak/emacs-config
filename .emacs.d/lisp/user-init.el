(require 'user-init-default)
;; (require 'user-init-email)
(require 'user-init-helm)
(require 'user-init-ace-window)
(require 'user-init-appearance)
(require 'user-init-projectile)
(require 'user-init-autocomplete)
(require 'user-init-flycheck)
(require 'user-init-python)
(require 'user-init-go)
(require 'user-init-cs)
(require 'user-init-lsp)
(require 'user-init-extras)
(require 'user-init-php)
(require 'user-init-magit)
(require 'user-init-js)
(require 'user-init-haskell)
(require 'user-init-treemacs)
(require 'user-init-navigation)
;; (require 'user-init-persp)
;; (require 'user-init-terminal)
(require 'user-init-org)
(require 'bloomon)
;;(require 'user-init-floobits)

;; load keybindings last.
;; it makes references to other packages
;; that must be loaded first
(require 'user-init-keybindings)

(provide 'user-init)
