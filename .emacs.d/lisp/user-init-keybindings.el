(require 'user-init-funcs)

(use-package hydra
  :ensure)

(when (display-graphic-p) 
  (setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none))

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

    (defhydra hydra-projectile (:color teal
                                :hint  nil)
      "
     PROJECTILE: %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\")
^^^^       Find               ^^   Search/Tags       ^^^^       Buffers               ^^   Cache                     ^^^^       Other
^^^^--------------------------^^---------------------^^^^-----------------------------^^------------------------------------------------------------------
^^    _f_: file               _a_: ag                ^^    _i_: Ibuffer               _c_: cache clear               ^^    _E_: edit project's .dir-locals.el
^^    _F_: file dwim          _G_: update gtags      ^^    _b_: switch to buffer      _x_: remove known project      _s-p_/_p_: switch to other project
^^    _d_: file curr dir      _o_: multi-occur       _K_/_s-k_: kill all buffers      _X_: cleanup non-existing      ^^    _g_: switch to Magit status of other project
^^    _l_: file literally     ^^                     ^^^^                             _z_: cache current             ^^    _P_: switch to an open project
^^    _r_: recent file        ^^                     ^^^^                             ^^                             ^^    _D_: find dir
"
      ("a"   projectile-ag)
      ("b"   helm-projectile-switch-to-buffer)
      ("c"   projectile-invalidate-cache)
      ("d"   projectile-find-file-in-directory)
      ("f"   helm-projectile-find-file)
      ("F"   projectile-find-file-dwim)
      ("D"   projectile-find-dir)
      ("E"   projectile-edit-dir-locals)
      ("g"   modi/projectile-switch-project-magit-status)
      ("G"   ggtags-update-tags)
      ("i"   projectile-ibuffer)
      ("K"   projectile-kill-buffers)
      ("s-k" projectile-kill-buffers)
      ("l"   modi/projectile-find-file-literally)
      ("m"   projectile-multi-occur)
      ("p"   helm-projectile-switch-project)
      ("s"   helm-projectile-ag)
      ("s-p" projectile-switch-project)
      ("P"   projectile-switch-open-project)
      ("o"   projectile-switch-project)
      ("r"   projectile-recentf)
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)
      ("4"   hydra-projectile-other-window/body "other window")
      ("q"   nil "cancel" :color blue))


(setq zsh-shell-path "/usr/bin/zsh")
(if (eq system-type 'darwin)
    (setq zsh-shell-path "/bin/zsh"))

(defhydra hydra-open (:color blue
                             :hint nil)
  "
Open process:
-------------------------------------------------------------------------------------
[_t_] ansi-term ^^ [_m_]: mail ^^ 
"
  ("t" (ansi-term zsh-shell-path))
  ("m" mu4e))

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
  ("o" hydra-open/body)
  ("s" hydra-search/body)
  ("r" hydra-ring/body)
  ("*" helm-ag)
  ("w" hydra-persp/body))
  

(with-eval-after-load "lsp-mode"
  (define-key lsp-mode-map (kbd "M-RET") 'hydra-lsp/body))
(global-set-key (kbd "M-m") 'hydra-main/body)
(global-set-key (kbd "M-RET") 'hydra-lsp/body)
(global-set-key (kbd "M-o")  'other-window)

(eval-after-load "term"
  (progn
    '(define-key term-raw-map (kbd "C-c C-y") 'term-paste)
    '(define-key term-raw-map (kbd "C-c M-x") 'helm-M-x)))


(provide 'user-init-keybindings)
;;; user-init-keybindings.el ends here
