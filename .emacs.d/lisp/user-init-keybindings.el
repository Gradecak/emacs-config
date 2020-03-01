(use-package hydra
  :ensure)

(defhydra hydra-buffer (:color blue)
  "

_k_: kill buffer
_<tab>_: cycle buffers

"
  ("k" (kill-buffer))
  ("<tab>" (switch-to-next-buffer) :exit nil))


(defhydra hydra-main (:color blue :idle 0.5)
  "

_<tab>_: switch to last buffer focused
_b_: buffer commands
_p_: projectile commands
"
  ("<tab>" (switch-to-buffer nil))
  ("b" hydra-buffer/body)
  ("p" hydra-projectile/body))


(global-set-key (kbd "M-m") 'hydra-main/body)
(global-set-key (kbd "s-o")  'other-window)

(provide 'user-init-keybindings)

