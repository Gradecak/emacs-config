(require 'cl)
(use-package projectile
  :ensure t
  :after helm
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (setq projectile-auto-discover nil)
    (setq projectile-switch-project-action 'projectile-find-file)
    (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))
    (setq projectile-enable-caching t) ; Enable caching, otherwise find-file is slow
    ;; ;; Git projects should be marked as projects in top-down fashion,
    ;; ;; so that each git submodule can be a projectile project.
    ;; (setq projectile-project-root-files-bottom-up
    ;;       (delete ".git" projectile-project-root-files-bottom-up))
    ;; (add-to-list 'projectile-project-root-files ".git")
    ;; ;; projectile-project-root-files #'(".git")
    ;; (setq projectile-project-root-files-functions
    ;;       '(projectile-root-local
    ;;         projectile-root-top-down ; First look for projects in top-down order
    ;;         projectile-root-bottom-up)) ; Then in bottom-up order
    (projectile-mode 1)))

(with-eval-after-load "hydra"
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
      ("q"   nil "cancel" :color blue)))



(provide 'user-init-projectile)
