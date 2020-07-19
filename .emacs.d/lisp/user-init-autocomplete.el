(use-package company
  :ensure
  :init (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort)))


(use-package company-restclient
  :ensure
  :init
  (add-to-list 'company-backends 'company-restclient))

(use-package yasnippet
  :ensure t
  :config
  (setq
   yas-verbosity 1
   yas-wrap-around-region t)

;  (with-eval-after-load 'yasnippet
;    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(provide 'user-init-autocomplete)
