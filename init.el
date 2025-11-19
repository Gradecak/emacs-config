;; Install straight.el  -*- lexical-binding: t; -*-

(setq straight-use-package-by-default t)
(eval-when-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
                  keybindings
                  risk
                  grpc
                  ai))

(provide 'init)
