(require 'user-init-config)
(require 'user-init-default)
(require 'user-init-helm)
(require 'user-init-ace-window)
(require 'user-init-appearance)
(require 'user-init-projectile)
(require 'user-init-autocomplete)
(require 'user-init-treemacs)
;; load keybindings last as it makes references to other packages that must be loaded
(require 'user-init-keybindings)

(provide 'user-init)