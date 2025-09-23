;; -*- lexical-binding: t; -*-

(require 'use-package)

(defun pylsp-config ()
  (let* ((jedi-extra-paths ["src/"
                            ".site-packages"
                            ".dockerdev/site-packages"
                            ".dockerenv/site-packages"
			    "__pypackages__/3.9/lib/"
			    "__pypackages__/3.10/lib/"]))
	`((pylsp . ((configurationSources . ["flake8"])
		    (plugins . ((pycodestyle . (enabled :json-false))
				(pyflakes . (enabled :json-false))
                                (ruff . (enabled t
                                         formatEnabled t))
				;; (flake8 . (enabled t))
                                (isort . (enabled t))
				;; (black . (enabled t))
                                (rope . (ropeFolder nil))
                                (rope_autoimport . (completions (enabled :json-false)
                                                    code_actions (enabled :json-false)))
				(pylsp_mypy . ((enabled . t)
					       (live-mode . :json-false)))
                                (jedi_signature_help . (enabled :json-false))
				(jedi . (extra_paths ,jedi-extra-paths)))))))))

(defun elixir-ls-config ()
  `((:elixirLS . ((:suggestSpecs . t)
                  (:dialyzerEnabled . t)))))

(defun eglot-server-config (server)
  (let ((major-modes (eglot--major-modes (eglot-current-server))))
    (cond
     ((memq 'python-ts-mode major-modes) (pylsp-config))
     ((memq 'elixir-ts-mode major-modes) (elixir-ls-config))
     )))

(use-package eglot
  :straight (:type built-in)
  :init
  (setq eglot-workspace-configuration #'eglot-server-config)
  :bind
  (("C-<return> f" . eglot-format-buffer)
   ("C-<return> S" . eglot-shutdown)
   ("C-<return> r" . eglot-rename)
   ("C-<return> a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rustup" "run" "nightly" "rust-analyzer")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rustup" "run" "nightly" "rust-analyzer")))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode . ("~/Downloads/elixir-ls/language_server.sh")))
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
  (python-ts-mode . eglot-ensure)
  :mode ("\\.py\\'". python-ts-mode)
  :bind (:map python-mode-map
	      ("C-c C-p" . 'python-better-shell)
	      ("C-c t" . 'pytest-runner)
	      :map python-ts-mode-map
	      ("C-c C-p" . 'python-better-shell)
	      ("C-c t" . 'pytest-runner)))

(use-package protobuf-ts-mode
  :defer t)

(use-package python-better-shell
  :straight nil
  :load-path "./lisp/")

(use-package docker
  :bind ("C-c d" . docker))

(use-package clojure-mode)
  :defer t

(use-package cider
  :defer t)

(use-package elixir-ts-mode
  :straight (:type built-in)
  :ensure t
  :mode ("\\.exs?\\'" . elixir-ts-mode)
  :hook
  (elixir-ts-mode-hook . eglot-ensure))

(use-package jsonnet-mode
  :straight (jsonnet-mode
             :type git
             :host github
             :repo "tminor/jsonnet-mode")
  :mode ("\\.(j|lib)sonnet\\'"))

(provide 'programming)
