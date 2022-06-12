(use-package transient )


(transient-define-prefix transient-projectile ()
  [["Find"
    ("f" "file" (message "use <>"))
    ("o" "open project" (message "use <>"))]
   ["Search/Tags"
    ("s" "search project" (lambda () (interactive) (message "Use M-s r")))]
   ["Buffers"
    ("b" "switch project buffer" (message "use <>"))
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
    ("ry" "ring yank" (lambda () (interactive) (message "use M-y")))]

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
    ("el" "list errors" consult-flycheck)]

   ["Comment (c)"
    ("cl" "comment line" comment-line)]

   ["Toggles (t)"
    ("tf" "fullscreen" toggle-frame-fullscreen)
    ("tr" "reload config" load-init-el)]])

(use-package emacs
  :straight (:type built-in)
  :bind (("M-m" . transient-main)
	 ("C-;" . comment-line)
	 ("C-c <tab>" . (lambda () (interactive) (switch-to-buffer nil)))
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)))


(provide 'keybindings)
