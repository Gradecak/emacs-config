(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

(use-package hungry-delete
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode 1)
  (c-toggle-hungry-state))

(use-package csharp-mode)

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :hook
  (yaml-mode-hook . (lambda ()
		      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package go-mode
  :after lsp
  :hook
  (go-mode . lsp-deferred)
  :config
  (setq lsp-gopls-staticcheck t
	lsp-eldoc-render-all t
	lsp-gopls-complete-unimported t))


(use-package haskell-mode
  :hook
  (haskell-mode . lsp-deferred))

(use-package lsp-haskell)


(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (setq js-indent-level 2)
  :hook
  (rjsx-mode . lsp-deferred))

(use-package js2-refactor
  :hook
  (rjsq-mode-hook . js2-refactor-mode)
  :init
  (setq js2-basic-offset 2))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2)
  :hook
  (web-mode . lsp-deferred))

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (setq typescript-indent-level 2)
  :hook
  (typescript-mode . lsp-deferred))


(use-package php-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  :hook (php-mode . lsp-deferred))


;; ----------
;; PYTHON
;; ----------
(use-package python
  :config
  (setq-default python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil
	python-eldoc-get-doc nil)
  :hook (python-mode . lsp-deferred)
  :bind
  (:map python-mode-map
	("C-c C-p" . 'python-better-shell)))

(use-package python-better-shell
  :ensure nil
  :load-path "./")


(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator
	'(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode 1))

(with-eval-after-load "lsp-mode"
  (lsp-register-custom-settings '(("pylsp.plugins.pycodestyle.enabled" nil t)
				  ("pylsp.plugins.pyls_mypy.enabled" t t)
				  ("pylsp.plugins.pyls_mypy.live_mode" nil t))))

(defun auto-pyvenv ()
  (interactive)
  (let* ((project-root (projectile-project-name))
	 (venvs (concat (getenv "HOME") "/.venvs/"))
	 (venv-path (concat venvs project-root)))
    (message venv-path)
    (if (file-directory-p venv-path)
	(pyvenv-activate venv-path)
      (message "no venv for project"))))


(provide 'programming)
