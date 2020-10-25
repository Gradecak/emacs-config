;; increase threshold before garbage collector runs
;; improves performance for lsp which generates lots of garbage
(setq load-prefer-newer t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(package-initialize)
(unless (package-installed-p 'use-package)
  ;; only fetch the archives if you don't have use-package installed
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'user-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "cf3d5d77679f7daed6a2c863e4f2e30427d5e375b254252127be9359957502ec" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "fe00bb593cb7b8c015bb2eafac5bfc82a9b63223fbc2c66eddc75c77ead7c7c1" default))
 '(global-display-fill-column-indicator-mode t)
 '(helm-ag-base-command "ag -Q --vimgrep")
 '(helm-ag-use-temp-buffer t)
 '(helm-execute-persistent-action "<tab>")
 '(helm-follow-mode-persistent t)
 '(helm-scroll-amount 4)
 '(package-selected-packages
   '(org-projectile diff-hl fill-column-indicator floobits elpy shell-pop gh-md multi-term org-gcal mu4e-alert sass-mode dockerfile-mode elixir-mode typescript typescript-mode web-mode js2-refactor csharp-mode undo-tree company-restclient restclient terraform-mode groovy-mode hungry-delete hl-todo hl-todo-mode indent-guide ace-jump-mode yaml-mode rjsx-mode rjx-mode ag helm-ag helm-sys company-box pyvenv python-mode helm-lsp lsp-treemacs company-lsp lsp-ui lsp-mode helm-flycheck flycheck-pos-tip flycheck treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs magit exec-path-from-shell hydra projectile company helm-utils ace-window use-package))
 '(send-mail-function 'smtpmail-send-it)
 '(terraform-indent-level 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq gnutls-log-level 1)
