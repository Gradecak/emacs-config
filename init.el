(eval-when-compile
  (require 'package)

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (unless (package-installed-p 'bind-key)
    (package-refresh-contents)
    (package-install 'bind-key))
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(mapc #'require '(user-init
                  custom-functions
                  bloomon
                  appearance
                  programming
                  completion
                  org-custom
                  navigation
                  language-server
                  keybindings))

(use-package pytest
  :ensure nil
  :load-path "~/Documents/emacs-pytest/")

(provide 'init)
