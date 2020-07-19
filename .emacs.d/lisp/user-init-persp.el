(with-eval-after-load "persp-mode"
    (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

    ;; (setq persp-add-buffer-on-find-file 'if-not-autopersp)

    (persp-def-auto-persp "projectile"
      :parameters '((dont-save-to-file . t)
                    (persp-mode-projectile-bridge . t))
      :hooks '(projectile-before-switch-project-hook
               projectile-after-switch-project-hook
               projectile-find-file-hook
               find-file-hook)
      :dyn-env '((after-switch-to-buffer-adv-suspend t))
      :switch 'frame
      :predicate
      #'(lambda (buffer &optional state)
          (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
            (and
             projectile-mode
             (buffer-live-p buffer)
             (buffer-file-name buffer)
             ;; (not git-commit-mode)
             (projectile-project-p)
             (or state t))))
      :get-name
      #'(lambda (state)
          (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
            (push (cons 'persp-name
                        (concat ""
                                (with-current-buffer (alist-get 'buffer state)
                                  (projectile-project-name))))
                  state)
            state))
      :on-match
      #'(lambda (state)
          (let ((hook (alist-get 'hook state))
                (persp (alist-get 'persp state))
                (buffer (alist-get 'buffer state)))
            (case hook
              (projectile-before-switch-project-hook
               (let ((win (if (minibuffer-window-active-p (selected-window))
                              (minibuffer-selected-window)
                            (selected-window))))
                 (when (window-live-p win)
                   (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
                         (window-buffer win)))))

              (projectile-after-switch-project-hook
               (when (buffer-live-p
                      persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                 (let ((win (selected-window)))
                   (unless (eq (window-buffer win)
                               persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                     (set-window-buffer
                      win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

              (find-file-hook
               (setcdr (assq :switch state) nil)))
            (if (case hook
                  (projectile-before-switch-project-hook nil)
                  (t t))
                (persp--auto-persp-default-on-match state)
              (setcdr (assq :after-match state) nil)))
          state)
      :after-match
      #'(lambda (state)
          (when (eq 'find-file-hook (alist-get 'hook state))
            (run-at-time 0.5 nil
                         #'(lambda (buf persp)
                             (when (and (eq persp (get-current-persp))
                                        (not (eq buf (window-buffer (selected-window)))))
                               ;; (switch-to-buffer buf)
                               (persp-add-buffer buf persp t nil)))
                         (alist-get 'buffer state)
                         (get-current-persp)))
          (persp--auto-persp-default-after-match state)))

    ;; (add-hook 'persp-after-load-state-functions
    ;;           #'(lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)
    )

(use-package persp-mode
  :ensure t
  :demand t
  :config
  (setq persp-auto-resume-time -1 ;; No autoload buffers
        persp-set-last-persp-for-new-frames nil
        persp-reset-windows-on-nil-window-conf t
        persp-autokill-buffer-on-remove t
        persp-add-buffer-on-after-change-major-mode t
        persp-kill-foreign-buffer-behaviour 'kill)
  (persp-mode 1)
  (with-eval-after-load "term"
    (persp-def-auto-persp "term"
                          :parameters '((dont-save-to-file . t))
                          :mode 'term-mode
                          :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                                     (persp-add-buffer-on-find-file nil)
                                     persp-add-buffer-on-after-change-major-mode)
                          :hooks '(after-switch-to-buffer-functions)
                          :switch 'window)))

(global-set-key (kbd "C-x b") #'(lambda (arg)
                                  (interactive "P")
                                  (with-persp-buffer-list () (ibuffer arg))))


(with-eval-after-load "persp-mode"
  (with-eval-after-load "helm-mode"
    (setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found))
    (defun helm-buffers-toggle-persp-filter ()
      (interactive)
      (with-helm-alive-p
        (let ((filter-attrs (helm-attr 'candidate-transformer
                                       helm-source-persp-buffers)))
          (if (memq #'helm-persp-buffers-filter-transformer filter-attrs)
              (progn
                (helm-attrset 'candidate-transformer
                              (delq #'helm-persp-buffers-filter-transformer
                                    filter-attrs)
                              helm-source-persp-buffers t)
                (helm-attrset 'name
                              "Buffers"
                              helm-source-persp-buffers t)
                (setq helm-persp-filtered-buffers-cache nil))
            (helm-attrset 'candidate-transformer
                          (cons #'helm-persp-buffers-filter-transformer
                                filter-attrs)
                          helm-source-persp-buffers t)
            (helm-attrset 'name
                          "Current perspective buffers"
                          helm-source-persp-buffers t))
          (helm-force-update))))
    (put 'helm-buffers-toggle-persp-filter 'helm-only t)
    (define-key helm-buffer-map
      persp-toggle-read-persp-filter-keys #'helm-buffers-toggle-persp-filter)
    (defvar helm-persp-filtered-buffers-cache nil)
    (defun helm-persp-buffers-filter-transformer (candidates)
      (setq helm-persp-filtered-buffers-cache nil)
      (let* ((persp (get-current-persp))
             (ret
              (cl-remove-if-not #'(lambda (bn)
                                    (let* ((ret (persp-contain-buffer-p (get-buffer bn) persp)))
                                      (unless ret
                                        (push bn helm-persp-filtered-buffers-cache))
                                      ret))
                                candidates)))
        ret))
    (defclass helm-persp-buffers-source (helm-source-buffers)
      ((buffer-list
        :initarg :buffer-list
        :initform #'(lambda () (mapcar #'buffer-name (buffer-list)))
        :custom function
        :documentation
        " A function with no arguments to create buffer list.")
       (cleanup :initform #'(lambda () (setq helm-persp-filtered-buffers-cache nil
                                        helm-buffers-list-cache nil)))
       (candidate-transformer :initform '(helm-persp-buffers-filter-transformer))))
    (defvar helm-source-persp-buffers
      (helm-make-source "Current perspective buffers"
          'helm-persp-buffers-source
        :fuzzy-match t))
    (defclass helm-persp-filtered-buffers-source (helm-source-buffers)
      ((candidates :initform #'(lambda ()
                                 (if helm-persp-filtered-buffers-cache
                                     helm-persp-filtered-buffers-cache
                                   (setq helm-persp-filtered-buffers-cache
                                         (mapcar #'buffer-name (persp-buffer-list-restricted nil 1))))))
       (cleanup :initform #'(lambda () (setq helm-persp-filtered-buffers-cache nil)))))
    (defvar helm-source-persp-filtered-buffers
      (helm-make-source "Other buffers"
          'helm-persp-filtered-buffers-source
        :fuzzy-match t))
    (defun helm-persp-buffer-list-bridge
        (prompt _collection &optional test _require-match init hist default _inherit-im name buffer)
      (let ((dflt (or default ""))
            (cbuf (current-buffer))
            helm-candidate-number-limit)
        (or
         (helm :sources (cond
                         ((eq this-command 'persp-add-buffer) '(helm-source-persp-filtered-buffers))
                         ((eq this-command 'persp-remove-buffer) '(helm-source-persp-buffers))
                         (t '(helm-source-persp-buffers helm-source-persp-filtered-buffers)))
               :fuzzy-match helm-mode-fuzzy-match
               :prompt prompt
               :buffer buffer
               :input init
               :history hist
               :resume 'noresume
               :keymap helm-buffer-map
               :truncate-lines helm-buffers-truncate-lines
               :default dflt
               :preselect #'(lambda ()
                              (if (and (eq this-command 'persp-temporarily-display-buffer)
                                       (persp-contain-buffer-p cbuf))
                                  (helm-next-source)
                                (helm-mark-current-line)
                                (let ((buffer-name-truncated-regexp (helm-buffers--quote-truncated-buffer cbuf))
                                      (start (point)) mp)
                                  (helm-awhile (re-search-forward buffer-name-truncated-regexp nil t)
                                    (when (helm-pos-header-line-p) (forward-line 1))
                                    (helm-mark-current-line)
                                    (when (eq cbuf (helm-get-selection)) (cl-return (setq mp it))))
                                  (goto-char (or mp start))
                                  (helm-mark-current-line)))))
         (helm-mode--keyboard-quit))))
    (defvar helm-persp-mini-default-sources
      (cons 'helm-source-persp-buffers
            (cons 'helm-source-persp-filtered-buffers
                  (remove 'helm-source-buffers-list helm-mini-default-sources))))
    (defun helm-persp-mini ()
      (interactive)
      (let* ((cbuf (current-buffer))
             (cbuf-name (buffer-name cbuf))
             helm-candidate-number-limit)
        (or
         (helm :sources helm-persp-mini-default-sources
               :ff-transformer-show-only-basename nil
               :fuzzy-match helm-mode-fuzzy-match
               :buffer "*helm persp mini*"
               :keymap helm-buffer-map
               :truncate-lines helm-buffers-truncate-lines
               :default cbuf-name
               :preselect #'(lambda ()
                              (let ((buffer-name-truncated-regexp (helm-buffers--quote-truncated-buffer cbuf))
                                    (start (point)) mp)
                                (helm-awhile (re-search-forward buffer-name-truncated-regexp nil t)
                                  (when (helm-pos-header-line-p) (forward-line 1))
                                  (helm-mark-current-line)
                                  (when (eq cbuf (helm-get-selection)) (cl-return (setq mp it))))
                                (goto-char (or mp start))
                                (helm-mark-current-line))))
         (helm-mode--keyboard-quit))))
    (global-set-key (kbd "C-x b") #'helm-persp-mini)
    (setq helm-completing-read-handlers-alist
          (append '((switch-to-buffer                 . helm-persp-buffer-list-bridge)
                    (persp-switch-to-buffer           . helm-persp-buffer-list-bridge)
                    (kill-buffer                      . helm-persp-buffer-list-bridge)
                    (persp-kill-buffer                . helm-persp-buffer-list-bridge)
                    (persp-temporarily-display-buffer . helm-persp-buffer-list-bridge)
                    (persp-add-buffer                 . helm-persp-buffer-list-bridge)
                    (persp-remove-buffer              . helm-persp-buffer-list-bridge))
                  helm-completing-read-handlers-alist))))


(with-eval-after-load "hydra"
  (defhydra hydra-persp (:color blue :hint nil)
  "
------------------------------------------------------------
[_s_] switch  [_k_] kill
[_c_] create  [_p_] open project
"
  ("s" persp-switch)
  ("c" persp-add-new)
  ("k" persp-kill)
  ("p" projectile-persp-switch-project)))


(provide 'user-init-persp)
