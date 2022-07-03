(require 'use-package)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config (require 'smartparens-config))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode 1)
  :bind (("<backspace>" . hungry-delete-backward)
	 ("C-d" . hungry-delete-forward)))

(use-package csharp-mode)

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

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
  :hook (haskell-mode . eglot-ensure))

(use-package rjsx-mode
  :mode ("\\.js\\'" . rjsx-mode)
  :config (setq js-indent-level 2)
  :hook (rjsx-mode . eglot-ensure))

(use-package js2-refactor
  :hook (rjsq-mode-hook . js2-refactor-mode)
  :config (setq js2-basic-offset 2))

(use-package web-mode
  :hook (web-mode . eglot-ensure)
  :mode (("\\.tsx\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode))
  :init (setq web-mode-markup-indent-offset 2 web-mode-css-indent-offset 2
	      web-mode-code-indent-offset 2))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :config (setq typescript-indent-level 2)
  :hook (typescript-mode . eglot-ensure))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :hook (php-mode . eglot-ensure))

(defun pylsp-init ()
    (interactive)
    (message "running init")
    (let* ((pip-dir (expand-file-name ".pip/" (projectile-project-root)))
	   (lib-dir (car (file-expand-wildcards (expand-file-name "lib/*" pip-dir))))
	   (jedi-extra-paths []))
      ;; make sure that when we set the env vars, they are only set for processes
      ;; spawned from this buffer and _NOT_ globally
      (make-local-variable 'process-environment)
      (when lib-dir
	(setenv "PYTHONPATH" (expand-file-name "site-packages/" lib-dir))
	(setenv "PYTHONUSERBASE" pip-dir)
	;; (setenv "PATH" (concat (expand-file-name ".pip/bin/" (projectile-project-root)) ":" (getenv "PATH")))
	(setq jedi-extra-paths (vector (format "%S" (expand-file-name "site-packages/" lib-dir)))))
      (setq-local eglot-workspace-configuration
		  `((pylsp . ((configurationSources . ["flake8"])
			      (plugins . ((pycodestyle . (enabled :json-false))
					  (pyflakes . (enabled :json-false))
					  (flake8 . (enabled t))
					  (pylsp_mypy . ((enabled . t)
							 (live-mode . :json-false)
							 ;; (overrides . ["--config-file",(format "%S" (expand-file-name "setup.cfg" (projectile-project-root)))])
							 (overrides . ["--config-file" ,(expand-file-name "setup.cfg" (projectile-project-root))])
							 ))
					  (jedi . (extra_paths ,jedi-extra-paths))))))))
      (eglot-ensure)
      ;; for some reason eglot does not pick up the workspace configuration on first load
      (run-with-timer 2 nil (lambda ()
			      (message "disabling eglot")
			      (eglot-signal-didChangeConfiguration (eglot--current-server-or-lose))
			      (eldoc-mode -1)))))

(use-package python
  :straight (:type built-in)
  :config
  (setq-default python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil
	python-eldoc-get-doc nil)
  :hook (python-mode . pylsp-init)
  :bind (:map python-mode-map
	      ("C-c C-p" . 'python-better-shell)
	      ("C-c t" . 'pytest-runner)))

(use-package python-better-shell
  :straight nil
  :load-path "./lisp/")

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator
	'(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "]")))
  (pyvenv-mode 1))

(use-package docker
  :bind ("C-c d" . docker))

(use-package plantuml-mode)


(provide 'programming)
