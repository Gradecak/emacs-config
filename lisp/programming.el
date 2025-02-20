(require 'use-package)

(defun pylsp-config (server)
  (if (memq 'python-mode (eglot--major-modes (eglot-current-server)))
      (let* ((jedi-extra-paths [".pip/lib/python3.11/site-packages/"
                                ".pip/lib/python3.10/site-packages/"
				".pip/lib/python3.9/site-packages/"
				"src/"
				"__pypackages__/3.9/lib/"
				"__pypackages__/3.10/lib/"]))
	`((pylsp . ((configurationSources . ["flake8"])
		    (plugins . ((pycodestyle . (enabled :json-false))
				(pyflakes . (enabled :json-false))
				(flake8 . (enabled t))
                                (isort . (enabled t))
				(black . (enabled t))
				(pylsp_mypy . ((enabled . t)
					       (live-mode . :json-false)))
                                (jedi_signature_help . (enabled :json-false))
				(jedi . (extra_paths ,jedi-extra-paths))))))))
    `()))




(use-package eglot
  :straight (:type built-in)
  :init
  (setq eglot-workspace-configuration #'pylsp-config)
  :bind
  (("C-<return> f" . eglot-format-buffer)
   ("C-<return> S" . eglot-shutdown)
   ("C-<return> r" . eglot-rename)
   ("C-<return> a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rustup" "run" "nightly" "rust-analyzer")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rustup" "run" "nightly" "rust-analyzer")))
  (setq eglot-events-buffer-size 0
	eglot-confirm-server-initiated-edits nil
        eglot-sync-connect 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode t)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . eglot-ensure))

(use-package company-box
  :after company-mode
  :hook (company-mode . company-box-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode 1)
  :bind (("<backspace>" . hungry-delete-backward)
	 ("C-d" . hungry-delete-forward)))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook (yaml-mode . eglot-ensure))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :config (add-hook 'json-mode-hook (lambda ()
				      (make-local-variable 'js-indent-level)
				      (setq js-indent-level 2))))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config (setq-default markdown-command "multimarkdown"))

(use-package go-mode
  :hook (go-mode . eglot-ensure))

(use-package haskell-mode
  :hook (go-mode . eglot-ensure))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package sqlformat
  :config
  (setq-default sqlformat-command 'pgformatter))

(use-package sql
  :after sqlformat
  :straight (:type built-in)
  :bind (:map sql-mode-map
              ("C-<return> f" . sqlformat-buffer)))

(use-package web-mode
  :init (setq-default web-mode-markup-indent-offset 2
	              web-mode-css-indent-offset 2
	              web-mode-code-indent-offset 2))

(use-package typescript-mode
  :straight (:type built-in)
  :mode (("\\.tsx?\\'" . tsx-ts-mode))
  :hook
  (tsx-ts-mode . eglot-ensure)
  :config
  (setq-default typescript-indent-level 2))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(use-package python
  :straight (:type built-in)
  :config
  (setq-default python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)
  :hook
  (python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  :mode ("\\.py\\'". python-ts-mode)
  :bind (:map python-mode-map
	      ("C-c C-p" . 'python-better-shell)
	      ("C-c t" . 'pytest-runner)
	      :map python-ts-mode-map
	      ("C-c C-p" . 'python-better-shell)
	      ("C-c t" . 'pytest-runner)))

(use-package python-better-shell
  :straight nil
  :load-path "./lisp/")

(use-package docker
  :bind ("C-c d" . docker))

(use-package clojure-mode)
(use-package cider)

(provide 'programming)
