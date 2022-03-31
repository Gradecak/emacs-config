(defun maki/helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
		   (let ((bg-color (face-background 'default nil)))
		     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :demand t ;stop lazy loading which breaks projectile
  :bind( ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x b" . helm-buffers-list))
  :config
  (require 'helm-config)
  (require 'helm-for-files)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (setq helm-input-idle-delay                     0.01
	helm-split-window-inside-p                t
	helm-commands-using-frame                 '(completion-at-point
						    helm-apropos
						    helm-eshell-prompts helm-imenu
						    helm-imenu-in-all-buffers)
	helm-show-action-window-other-window      'left
	;;helm-move-to-line-cycle-in-source         t
	helm-autoresize-max-height                80 ; it is %.
	helm-autoresize-min-height                20 ; it is %.
	helm-follow-mode-persistent               t
	helm-candidate-number-limit               500
	helm-visible-mark-prefix                  "✓"
	helm-scroll-amount                        4
	;; helm-execute-persistent-action            "<tab>"
	helm-echo-input-in-header-line            t)
  (add-hook 'helm-minibuffer-set-up-hook 'maki/helm-hide-minibuffer-maybe)
  (helm-mode 1))

;; (with-eval-after-load 'helm-files
;;        (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;        )


(use-package helm-ag
  :after (:all ag helm)
  :config
  (progn
    (custom-set-variables
     ;; '(helm-follow-mode-persistent t)
     '(helm-ag-base-command "ag --vimgrep")
     '(helm-ag-use-temp-buffer t))
    ;; ensure helm window always opens at bottom of frame
    (add-to-list 'display-buffer-alist
		 `(,(rx bos "*helm" (* not-newline) "*" eos)
		   (display-buffer-in-side-window)
		   (inhibit-same-window . t)
		   (window-height . 0.4)))))


(use-package helm-projectile
  :after (:all helm projectile)
  :init
  (helm-projectile-on))

(use-package helm-icons
  :after helm
  :init
  (helm-icons-enable))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winum)

(use-package embark
  :bind
  (("C-." . embark-act))         ;; pick some comfortable binding
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package avy
  :config
  (setq avy-background t)
  :bind (("C-:" . avy-goto-line)
	 ("C-j" . avy-goto-char)
	 :map org-mode-map
	 ("C-j" . avy-goto-char)))


(with-eval-after-load "avy"
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
	  (bounds-of-thing-at-point 'line)
	(copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)


  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-next-line (pt)
    (goto-char pt)
    (forward-line))

  (defun avy-action-embark (pt)
    (unwind-protect
	(save-excursion
	  (goto-char pt)
	  (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
	(alist-get ?w avy-dispatch-alist) 'avy-action-copy
	(alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
	(alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
	(alist-get ?. avy-dispatch-alist) 'avy-action-embark
	(alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
	(alist-get ?n avy-dispatch-alist) 'avy-action-next-line))

(provide 'navigation)
