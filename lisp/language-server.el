(require 'use-package)

(use-package eglot
  :bind
  (("C-<return> f" . eglot-format-buffer)
   ("C-<return> S" . eglot-shutdown)
   ("C-<return> r" . eglot-rename))
  :config
  (setq eglot-autoshutdown t
	eglot-events-buffer-size 0))
(provide 'language-server)
