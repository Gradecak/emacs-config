(require 'use-package)

(defun pylsp-config (server)
  (if (memq 'python-mode (eglot--major-modes (eglot-current-server)))
    (let* ((project-root (project-root (project-current)))
	 (pip-dir (expand-file-name ".pip/" project-root))
	 (lib-dir (car (file-expand-wildcards (expand-file-name "lib/*" pip-dir))))
	 (jedi-extra-paths []))
    ;; make sure that when we set the env vars, they are only set for processes
    ;; spawned from this buffer and _NOT_ globally
    (make-local-variable 'process-environment)
    (when lib-dir
      (setenv "PYTHONPATH" (expand-file-name "site-packages/" lib-dir))
      (setenv "PATH" (concat (expand-file-name ".pip/bin/" project-root) ":" (getenv "PATH")))
      (setq jedi-extra-paths (vector (format "%S" (expand-file-name "site-packages/" lib-dir)))))
    `((pylsp . ((configurationSources . ["flake8"])
			    (plugins . ((pycodestyle . (enabled :json-false))
					(pyflakes . (enabled :json-false))
					(flake8 . (enabled t))
					(pylsp_mypy . ((enabled . t)
						       (live-mode . :json-false)))
					(jedi . (extra_paths ,jedi-extra-paths))))))))
    `()))




(use-package eglot
  :init
  (setq eglot-workspace-configuration #'pylsp-config)
  :bind
  (("C-<return> f" . eglot-format-buffer)
   ("C-<return> S" . eglot-shutdown)
   ("C-<return> r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rustup" "run" "nightly" "rust-analyzer")))
  (setq eglot-events-buffer-size 0
	eglot-confirm-server-initiated-edits nil)
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

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (("M-i" . sp-change-enclosing)
	 ("C-\\" . sp-change-inner)
	 ("C-S-k" . sp-kill-sexp))
  :config
  (require 'smartparens-config))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode 1)
  :bind (("<backspace>" . hungry-delete-backward)
	 ("C-d" . hungry-delete-forward)))

(use-package csharp-mode)

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
  :config (setq markdown-command "multimarkdown"))

(use-package go-mode
  :hook (go-mode . eglot-ensure))

(use-package haskell-mode
  :hook (go-mode . eglot-ensure))

(use-package js2-mode
  :straight (:type built-in)
  :mode (("\\.tsx?\\'" . js2-mode))
  :hook (js2-mode . eglot-ensure))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package web-mode
  :init (setq web-mode-markup-indent-offset 2
	      web-mode-css-indent-offset 2
	      web-mode-code-indent-offset 2))

(use-package typescript-mode
  :after tree-sitter
  :mode (("\\.tsx?\\'" . typescriptreact-mode))
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2)
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(use-package python
  :straight (:type built-in)
  :config
  (setq-default python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)
  :hook
  (python-mode . eglot-ensure)
  :bind (:map python-mode-map
	      ("C-c C-p" . 'python-better-shell)
	      ("C-c t" . 'pytest-runner)))

(use-package python-better-shell
  :straight nil
  :load-path "./lisp/")

(use-package docker
  :bind ("C-c d" . docker))

(provide 'programming)
