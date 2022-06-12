(require 'use-package)

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
  (setq prefix-help-command #'embark-prefix-help-command
	embark-indicators '(embark-minimal-indicator
			    embark-highlight-indicator
			    embark-isearch-highlight-indicator))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook '((embark-collect-mode . consult-preview-at-point-mode)))

(use-package avy
  :config
  (setq avy-background t)
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
	(alist-get ?n avy-dispatch-alist) 'avy-action-next-line)

  :bind (("C-:" . avy-goto-line)
	 ("C-j" . avy-goto-char)
	 :map org-mode-map
	 ("C-j" . avy-goto-char)))

(provide 'navigation)
