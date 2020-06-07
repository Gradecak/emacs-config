(require 'user-init-funcs)
(require 'user-init-lsp)
(require 'user-init-persp)

(use-package hydra
  :ensure)

(use-package which-key
  :ensure
  :init
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05))

(defhydra hydra-buffer (:color blue)
  "Buffers"
  ("k" kill-buffer "kill buffer")
  ("i" indent-buffer "indent buffer"))


(defhydra hydra-file (:color blue)
  "Files"
  ("D" delete-file-and-buffer "delete file")
  ("R" rename-file-and-buffer "rename file"))

(defhydra hydra-comments (:color blue)
  ("l" comment-line "comment line"))

(defhydra hydra-errors (:color blue)
  "Errors"
  ("l" helm-flycheck "flycheck errors"))

(defhydra hydra-toggles (:color blue)
  ("f" toggle-frame-fullscreen "fullscreen"))

(defhydra hydra-search (:color blue)
  ("s" helm-occur-from-isearch))

(defhydra hydra-ring (:color blue)
  ("y" helm-show-kill-ring))

(defhydra hydra-main (:color blue
                      :hint nil)
 "
-------------------------------------------------------------------------------------
 [_b_] buffers    [_g_] git         [_t_] toggles  [_<tab>_] last-buffer
 [_f_] files      [_e_] errors      [_s_] search   [_*_] helm-ag
 [_p_] projects   [_c_] comment     [_r_] ring     [_w_] workspaces"
 
  ("<tab>" (switch-to-buffer nil))
  ("b" hydra-buffer/body)
  ("f" hydra-file/body)
  ("p" hydra-projectile/body)
  ("g" hydra-magit/body)
  ("e" hydra-errors/body)
  ("c" hydra-comments/body)
  ("t" hydra-toggles/body)
  ("s" hydra-search/body)
  ("r" hydra-ring/body)
  ("*" helm-ag)
  ("w" hydra-persp/body))
  

(with-eval-after-load "lsp-mode"
  (define-key lsp-mode-map (kbd "M-RET") 'hydra-lsp/body))
(global-set-key (kbd "M-m") 'hydra-main/body)
(global-set-key (kbd "M-RET") 'hydra-lsp/body)
(global-set-key (kbd "M-o")  'other-window)


(provide 'user-init-keybindings)
;;; user-init-keybindings.el ends here
