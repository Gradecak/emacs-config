(use-package hydra
  :ensure
  :init)

(with-eval-after-load 'hydra
  (defhydra hydra-pytest (:exit t :hint nil :color blue)
    "
     ^^ Action^^                  Buffer^^
     ^^------------------------------------
     ^^ [_a_] run all tests       [_b_] Open pytest buffer
     ^^ [_f_] test current file   ^^"
    ("a" pytest-run)
    ("f" pytest-run-current-file)
    ("b" pytest-open-buffer))

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

  (defhydra hydra-search (:color blue)
    ("s" helm-occur))

  (defhydra hydra-ring (:color blue)
    ("y" helm-show-kill-ring))

  (defhydra hydra-toggles (:color blue)
    ("f" toggle-frame-fullscreen "fullscreen")
    ("r" load-init-el "reload config"))

  (defhydra hydra-projectile (:color teal :hint  nil)
    "
	  PROJECTILE: %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\")
     ^^^^       Find               ^^   Search/Tags       ^^^^       Buffers               ^^   Cache                     ^^^^       Other
     ^^^^--------------------------^^---------------------^^^^-----------------------------^^-----------------------------^^^^---------------------------------
     ^^    _f_: file               ^^ _a_: ag             ^^^^  _b_: switch to buffer      ^^ _c_: cache clear            ^^^^ _E_: edit project's .dir-locals.el
     ^^    _F_: file dwim          ^^ _s_: helm ag        ^^^^  _K_: kill all buffers      ^^ _x_: remove known project   ^^^^ _p_: switch to other project
     ^^    _o_: open project       ^^ ^^                  ^^^^  _X_: cleanup non-existing  ^^ _z_: cache current          ^^^^ _P_: switch to an open project
     ^^    ^^^^                    ^^ ^^                  ^^^^  ^^^^                       ^^ ^^^^                        ^^^^ _D_: find dir
     ^^    ^^^^                    ^^ ^^                  ^^^^  ^^^^                       ^^ ^^^^                        ^^^^ _v_: activate virtualenv for project
     ^^    ^^^^                    ^^ ^^                  ^^^^  ^^^^                       ^^ ^^^^                        ^^^^ _t_: run pytest for current project
     "
    ("a"   projectile-ag)
    ("b"   helm-projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("f"   helm-projectile-find-file)
    ("F"   projectile-find-file-dwim)
    ("D"   projectile-find-dir)
    ("E"   projectile-edit-dir-locals)
    ("i"   projectile-ibuffer)
    ("t"   hydra-pytest/body)
    ("v"   auto-pyvenv)
    ("K"   projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("p"   helm-projectile-switch-project)
    ("s"   helm-projectile-ag)
    ("P"   projectile-switch-open-project)
    ("o"   projectile-switch-project)
    ("r"   mg/projectile-run-project)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file))

  (defhydra hydra-lsp (:exit t :hint nil :color blue)
    "
      Buffer^^               Server^^                   Symbol
     ----------------------------------------------------------------------------------------------------------------
      [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
      [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
      [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
    ("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("i" lsp-ui-peek-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)
    ("f" lsp-format-and-save)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)
    ("M-s" lsp-describe-session)
    ("M-r" lsp-workspace-restart)
    ("S" lsp-workspace-shutdown))

  (defhydra hydra-magit (:color blue :columns 4)
    "Magit"
    ("s" magit-status "status")
    ("C" magit-checkout "checkout")
    ("v" magit-show-refs "branches")
    ("f" magit-find-file-other-window "find file")
    ("m" magit-merge "merge")
    ("l" magit-log "log")
    ("!" magit-git-command "command")
    ("$" magit-process-buffer "process"))

  (defhydra hydra-main (:color blue :hint nil)
    "
     ------------------------------------------------------------------------
      [_b_] buffers    [_g_] git         [_t_] toggles  [_<tab>_] last-buffer
      [_f_] files      [_e_] errors      [_s_] search   [_*_] helm-ag
      [_p_] projects   [_c_] comment     [_r_] ring"

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
    ("*" helm-ag)))

(with-eval-after-load "lsp-mode"
  (define-key lsp-mode-map (kbd "M-RET") 'hydra-lsp/body))

;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)
(global-set-key (kbd "M-m") 'hydra-main/body)
(global-set-key (kbd "M-RET") 'hydra-lsp/body)
(global-set-key (kbd "C-;")  'comment-line)
;; keybindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "M-0") 'treemacs-display-current-project-exclusively)

(provide 'keybindings)
