;; Install straight.el

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

  ;; Disable package.el in favor of straight.el
  (setq package-enable-at-startup nil)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1410978f21ad4be59873f27d5020500803a747d2f6ef617608d53c3cf63cc75b" "de43637da82e6127fd76472ae58682927f25693fcccb16161be12f2331bcc7cc" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
