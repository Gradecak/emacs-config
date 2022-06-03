(use-package transient )


(transient-define-prefix transient-projectile ()
  [["Find"
    ("f" "file" helm-projectile-find-file)
    ("o" "open project" helm-projectile-switch-project)]
   ["Search/Tags"
    ("s" "search project" helm-projectile-rg)]
   ["Buffers"
    ("b" "switch project buffer" helm-projectile-switch-to-buffer)
    ("K" "kill all buffers" projectile-kill-buffers)
    ("X" "Cleanup Deleted" projectile-cleanup-known-projects)]])

(transient-define-prefix transient-lsp ()
  [["Buffer"
    ("f" "format" lsp-format-and-save)
    ("m" "menu" lsp-ui-imenu)]
   ["Server"
    ("M-r" "restart server" lsp-workspace-restart)
    ("S" "shutdown server" lsp-workspace-shutdown)
    ("M-s" "session info" lsp-describe-session)]
   ["Symbol"
    ("d" "declaration" lsp-find-declaration)
    ("D" "definition" lsp-ui-peek-find-definitions)
    ("R" "reference" lsp-ui-peek-find-references)
    ("i" "implementation" lsp-ui-peek-find-implementation)
    ("t" "type" lsp-find-type-definition)
    ("r" "rename" lsp-rename)
    ("o" "documentation" lsp-describe-thing-at-point)]
   ])

(transient-define-prefix transient-main ()
  [["General"
    ("<tab>" "last buffer" (lambda () (interactive) (switch-to-buffer nil)))
    ("*" "helm ag" helm-ag)
    ("ss" "serach buffer" helm-occur)
    ("ry" "ring yank" helm-show-kill-ring)]

   ["Buffers (b)"
    ("bk" "kill buffer" kill-buffer)
    ("bi" "indent buffer" indent-buffer)]

   ["File (f)"
    ("fD"  "delete file" delete-file-and-buffer)
    ("fR" "rename file" rename-file-and-buffer )]

   ["Notes (n)"
    ("nc" "capture" org-roam-capture)
    ("nl" "link" org-roam-node-insert)]

   ["Project (p)"
    ("p" "project" transient-projectile)]

   ["Git (g)"
    ("gs" "git" magit-status)]

   ["Errors (e)"
    ("el" "list errors" helm-flycheck)]

   ["Comment (c)"
    ("cl" "comment line" comment-line)]

   ["Toggles (t)"
    ("tf" "fullscreen" toggle-frame-fullscreen)
    ("tr" "reload config" load-init-el)]])

;; (with-eval-after-load "lsp-mode"
;;   (define-key lsp-mode-map (kbd "M-RET") 'transient-lsp))

;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
;; (global-set-key (kbd "TAB") 'company-indent-or-complete-common)
(global-set-key (kbd "M-m") 'transient-main)
(global-set-key (kbd "M-RET") 'transient-lsp)
(global-set-key (kbd "C-;")  'comment-line)
(global-set-key (kbd "C-c <tab>") (lambda () (interactive) (switch-to-buffer nil)))
;; keybindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "M-0") 'treemacs-display-current-project-exclusively)

(provide 'keybindings)
