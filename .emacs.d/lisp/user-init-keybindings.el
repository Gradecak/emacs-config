(use-package hydra
  :ensure)

(defhydra hydra-buffer (:color blue)
  "Buffers"
  ("k" (kill-buffer) "kill buffer")
  ("<tab>" (switch-to-next-buffer) :exit nil "cycle buffers"))

(defhydra hydra-magit (:color blue :columns 8)
  "Magit"
  ("s" magit-status "status")
  ("C" magit-checkout "checkout")
  ("v" magit-show-refs "branches")
  ("m" magit-merge "merge")
  ("l" magit-log "log")
  ("!" magit-git-command "command")
  ("$" magit-process-buffer "process"))

(defhydra hydra-file (:color blue)
  "Files"
  ("D" delete-file-and-buffer "delete file"))

(defhydra hydra-comments (:color blue)
  ("l" comment-line "comment line"))

(defhydra hydra-errors (:color blue)
  "Errors"
  ("l" helm-flycheck "flycheck errors"))

(defhydra hydra-toggles (:color blue)
  ("f" toggle-frame-fullscreen "fullscreen"))

(defhydra hydra-search (:color blue)
  ("s" helm-ag-this-file))

(defhydra hydra-main (:color blue :idle 0.5 :columns 8)
  "Global"
  ("<tab>" (switch-to-buffer nil) "last buffer")
  ("b" hydra-buffer/body "buffers")
  ("f" hydra-file/body "files")
  ("p" hydra-projectile/body "projects")
  ("g" hydra-magit/body "git")
  ("e" hydra-errors/body "errors")
  ("c" hydra-comments/body "comment")
  ("t" hydra-toggles/body "toggles")
  ("s" hydra-search/body "search"))


(global-set-key (kbd "M-m") 'hydra-main/body)
(global-set-key (kbd "s-o")  'other-window)

(provide 'user-init-keybindings)
;;; user-init-keybindings.el ends here
