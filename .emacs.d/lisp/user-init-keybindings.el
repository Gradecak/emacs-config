(use-package hydra
  :ensure)


(defhydra hydra-buffer ()
  "_\t_: switch to last buffer"
  ("\t" '(switch-to-buffer nil)))

(global-set-key (kbd "M-m") 'hydra-buffer/body)

(provide 'user-init-keybindings)
