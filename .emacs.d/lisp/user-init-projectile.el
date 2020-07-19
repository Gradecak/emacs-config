(use-package projectile
  :ensure
  :after (:all helm hydra)
  :config
  (progn
    (setq projectile-completion-system 'helm)
    ;(projectile-ensure-project (projectile-project-root))
    (setq projectile-switch-project-action 'projectile-find-file)
    (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))
    (setq projectile-enable-caching t) ; Enable caching, otherwise find-file is slow
    ;; Git projects should be marked as projects in top-down fashion,
    ;; so that each git submodule can be a projectile project.
    ;; (setq projectile-project-root-files-bottom-up
    ;;       (delete ".git" projectile-project-root-files-bottom-up))
    (add-to-list 'projectile-project-root-files ".git")

    (setq projectile-project-root-files-functions
          '(projectile-root-local
            projectile-root-top-down ; First look for projects in top-down order
            projectile-root-bottom-up)) ; Then in bottom-up order

    (projectile-mode 1)))


(provide 'user-init-projectile)
