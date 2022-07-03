(require 'transient)
(require 'use-package)

;;; Code:

(transient-define-prefix transient-projectile ()
  [["Buffers"
    ("K" "kill all buffers" projectile-kill-buffers)
    ("X" "Cleanup Deleted" projectile-cleanup-known-projects)]])

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
