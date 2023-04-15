;; Install straight.el

(setq straight-repository-branch "rr-fix-renamed-variable")
(setq straight-use-package-by-default t)
(eval-when-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  (straight-use-package 'bind-key)
  (setq straight-check-for-modifications '(check-on-save)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(mapc #'require '(user-init
                  custom-functions
                  appearance
                  programming
                  completion
                  org-custom
                  navigation
		  bloomon
                  keybindings))

(use-package pytest
  :straight nil
  :load-path "~/Documents/emacs-pytest/")

(provide 'init)
