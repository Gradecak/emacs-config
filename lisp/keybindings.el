(require 'transient)
(require 'use-package)

;;; Code:

(transient-define-prefix transient-projectile ()
  [["Buffers"
    ("K" "kill all buffers" projectile-kill-buffers)
    ("X" "Cleanup Deleted" projectile-cleanup-known-projects)]])

;; (transient-define-prefix transient-lsp ()
;;   [["Buffer"
;;     ("f" "format" lsp-format-and-save)
;;     ("m" "menu" lsp-ui-imenu)]
;;    ["Server"
;;     ("M-r" "restart server" lsp-workspace-restart)
;;     ("S" "shutdown server" lsp-workspace-shutdown)
;;     ("M-s" "session info" lsp-describe-session)]
;;    ["Symbol"
;;     ("d" "declaration" lsp-find-declaration)
;;     ("D" "definition" lsp-ui-peek-find-definitions)
;;     ("R" "reference" lsp-ui-peek-find-references)
;;     ("i" "implementation" lsp-ui-peek-find-implementation)
;;     ("t" "type" lsp-find-type-definition)
;;     ("r" "rename" lsp-rename)
;;     ("o" "documentation" lsp-describe-thing-at-point)]
;;    ])

(transient-define-prefix transient-main ()
  [["General"
    ("ar" "align regexp" align-regexp)
    ("<tab>" "last buffer" (lambda () (interactive) (switch-to-buffer nil)))]

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

   ["Errors (e)"
    ("el" "list errors" consult-flymake)]

   ["Comment (c)"
    ("cl" "comment line" comment-line)]

   ["Toggles (t)"
    ("tf" "fullscreen" toggle-frame-fullscreen)
    ("tr" "reload config" load-init-el)]])

(use-package emacs
  :straight (:type built-in)
  :bind (("M-RET" . transient-main)
	 ;; ("C-<return>" . transient-lsp)
	 ("C-;" . comment-line)
	 ("C-c <tab>" . (lambda () (interactive) (switch-to-buffer nil)))
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)))


(provide 'keybindings)
