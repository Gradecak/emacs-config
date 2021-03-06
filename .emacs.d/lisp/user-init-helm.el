(use-package helm
  :ensure
  :demand t ;stop lazy loading which breaks projectile
  :bind( ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list))
  :config
  (progn
    (require 'helm-config)
    (require 'helm-for-files)
    (helm-mode 1)))

(use-package ag
  :ensure)

(use-package helm-ag
  :ensure t
  :after ag
  :config
  (progn
    (custom-set-variables
     ;; '(helm-follow-mode-persistent t)
     '(helm-ag-base-command "ag --vimgrep")
     '(helm-scroll-amount 4)
     '(helm-ag-use-temp-buffer t)
     '(helm-execute-persistent-action "<tab>"))
    ;; ensure helm window always opens at bottom of frame
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (* not-newline) "*" eos)
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (window-height . 0.4)))))

;; set autocomplete in helm to tab
(with-eval-after-load 'helm-files
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  )

(use-package helm-projectile
  :ensure t
  :after (:all helm projectile)
  :init
  (helm-projectile-on))

(provide 'user-init-helm)
