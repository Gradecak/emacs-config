(require 'transient)
(require 'use-package)

;;; Code:

(transient-define-prefix transient-project ()
  [["Buffers"
    ("K" "kill project buffers" project-kill-buffers)]])

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
    ("p" "project" transient-project)]

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
