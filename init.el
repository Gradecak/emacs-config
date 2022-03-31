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

(use-package user-init
  :ensure nil
  :load-path "./lisp/")

(use-package custom-functions
  :ensure nil
  :load-path "./lisp/")

(use-package bloomon
  :ensure nil
  :load-path "./lisp/")

(use-package pytest
  :ensure nil
  :load-path "~/Documents/emacs-pytest/")

(use-package appearance
  :ensure nil
  :load-path "./lisp/")

(use-package programming
  :ensure nil
  :load-path "./lisp/")

(use-package completion
  :ensure nil
  :load-path "./lisp/")

(use-package org-custom
  :ensure nil
  :load-path "./lisp/")

(use-package navigation
  :ensure nil
  :load-path "./lisp/")

(use-package language-server
  :after completion
  :ensure nil
  :load-path "./lisp/")

(use-package keybindings
  :after (:all user-init programming custom-functions navigation completion)
  :ensure nil
  :load-path "./lisp/")


(use-package elfeed
  :ensure
  :config

  (setq elfeed-search-filter "@2-days-ago +unread"
	elfeed-feeds '(
		       ("https://www.dutchnews.nl/rss" dutchnews)
		       ("https://nltimes.nl/rss" nltimes)
		       ("https://www.reddit.com/r/emacs.rss" emacs-reddit))))
