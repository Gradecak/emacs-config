;; -*- lexical-binding: t; -*-


(use-package shell-maker
  :ensure t
  :config
  (setq shell-maker-prompt-before-killing-buffer nil))

(use-package acp
  :straight (acp :type git :host github :repo "xenodium/acp.el"))

(defun mg-agent-shell-buffers ()
  "Return the names of agent-shell buffers with active processes."
  (mapcar #'buffer-name
          (seq-filter
           (lambda (buf) (get-buffer-process buf))
           (agent-shell-buffers))))

(defvar consult--source-agent-shell
  `(:name "Agent Shell"
    :narrow (?c . "Agent Shell")
    :enabled ,(lambda () (mg-agent-shell-buffers))
    :category buffer
    :state ,#'consult--buffer-preview
    :action ,#'consult--buffer-action
    :items ,#'mg-agent-shell-buffers)
  "Agent shell candidates for `consult-buffer'.")

(defun mg-agent-shell--persp-has-shell-p ()
  "Return non-nil if the current perspective already has an agent-shell buffer."
  (and (bound-and-true-p persp-mode)
       (seq-some (lambda (buf)
                   (and (buffer-live-p buf)
                        (with-current-buffer buf
                          (derived-mode-p 'agent-shell-mode))))
                 (persp-current-buffers))))

(defun mg-agent-shell (arg)
  "Open agent shell, optionally for a specific project.
Without prefix, open agent shell for current project.  If the
current perspective has no agent-shell buffer, force a new one so
shells from other perspectives are not reused.
With \\[universal-argument], prompt for project and open/reuse shell.
With \\[universal-argument] \\[universal-argument], prompt for project and create new shell."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (let ((default-directory (completing-read "Project: " (project-known-project-roots))))
      (agent-shell '(4))))
   ((equal arg '(4))
    (let ((default-directory (completing-read "Project: " (project-known-project-roots))))
      (agent-shell)))
   ((not (mg-agent-shell--persp-has-shell-p))
    (agent-shell '(4)))
   (t
    (agent-shell))))

(use-package agent-shell
  :demand t
  :after (acp shell-maker)
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :bind (("C-c g" . mg-agent-shell)
         :map agent-shell-mode-map
         ("C-q" . bury-buffer)
         ("C-c b" . agent-shell-queue-peek-back))
  ;; :hook (agent-shell-mode . (lambda ()
  ;;                             (setq-local mode-line-format nil
  ;;                                         header-line-format nil)))
  :config
  (add-to-list 'consult-buffer-sources 'consult--source-agent-shell 'append)
  (mg-persp-filter-source 'consult--source-agent-shell)
  ;; (add-to-list 'aw-ignored-buffers #'agent-shell-mode)
  (add-to-list 'display-buffer-alist
               '(".+ Agent @ .+" (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (preserve-size . (t . nil))
                 (slot . 0)
                 (dedicated . t)
                 (window-width . 0.25)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)
                                       (window-size-fixed . width)
                                       (agent-shell-mode-dedicated . t)))))
  (add-to-list 'display-buffer-alist
               '("\\*Agent Queue\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . right)
                 (slot . -1)
                 (preserve-size . (t . nil))
                 (window-height . 0.3)
                 (window-width . 0.25)
                 (window-parameters . ((no-delete-other-windows . t)))))
  (setq
   agent-shell-anthropic-default-model-id "claude-opus-4-7"
   agent-shell-prefer-viewport-interaction nil
   ;; agent-shell-preferred-agent-config (agent-shell-goose-make-agent-config)
   agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)
   agent-shell-goose-authentication
        ;; the key is ignored and the config file for goose is used
        (agent-shell-make-goose-authentication :openai-api-key "FAKE KEY")))

;; (use-package agent-shell-queue
;;   :after agent-shell
;;   :straight nil
;;   :load-path "./lisp/"
;;   :config
;;   (agent-shell-queue-mode 1))

(provide 'ai)
