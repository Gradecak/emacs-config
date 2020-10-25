(use-package magit
  :ensure)

(with-eval-after-load 'hydra
  (defhydra hydra-magit (:color blue :columns 8)
  "Magit"
  ("s" magit-status "status")
  ("C" magit-checkout "checkout")
  ("v" magit-show-refs "branches")
  ("f" magit-find-file-other-window "find file")
  ("m" magit-merge "merge")
  ("l" magit-log "log")
  ("!" magit-git-command "command")
  ("$" magit-process-buffer "process")))


(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode))

(provide 'user-init-magit)
