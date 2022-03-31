;;; bloomon --- stuff

;;; Commentary:

(require  'projectile)
(require 'magit)
(require 'helm)

;;; Code:


(defun bloomon-dismiss-window-or-kill-buffer ()
  "If the process associated with the buffer is finished, kill buffer, otherwise quit window."
  (interactive)
  (if (get-process (buffer-name))
      (quit-window)
    (kill-buffer-and-window)))

(define-minor-mode dismissable-window-mode
  "test"
  :lighter dismissable
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-g") 'bloomon-dismiss-window-or-kill-buffer)
            map))

(defun split-window-horizontal (window)
  (split-window (frame-root-window) (frame-root-window) 'below))

(defun bloomon-cli-cmd ()
  "Get the bloomon-cli binary location."
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "which bloomon")))

(defun bloomon-exec-interactive-process (command proc-name)
  (pcase (split-string-and-unquote command)
	(`(,cmd . ,switches) (apply 'term-ansi-make-term proc-name cmd nil switches))))

(defun bloomon-run-process-with-focus (command proc-name)
  (let ((split-window-preferred-function 'split-window-horizontal)
	(*buffer* (bloomon-exec-interactive-process command proc-name)))
    (with-current-buffer *buffer*
      (term-mode)
      (term-char-mode)
      (dismissable-window-mode))
    (switch-to-buffer-other-window *buffer*)))

(defun bloomon-regexp-candidates (content regexp)
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp content pos)
        (push (match-string 1 content) matches)
        (setq pos (match-end 1)))
      matches)))

(defun bloomon-buffer-name (&optional action)
  "Create a buffer name for a PROJECT.  If ACTION is specified it will be included in name."
  (if action
      (format "*%s-%s*" (projectile-project-name) action)
    (format "*%s*" (projectile-project-name))))

(defun bloomon-env-candidates ()
  "Use regexp matching to extract all of the secrets environments from the project root."
  (let ((files (directory-files (projectile-project-root))))
    (remq nil (mapcar
	       (lambda (file)
		 (save-match-data
		   (and (string-match "secrets.\\(.*\\).enc" file)
			(message (match-string 1 file)))))
	       files))))

(defun bloomon-prompt-env (action)
  (helm :sources
	(helm-make-source "Secrets Env" 'helm-source-sync
	  :candidates (bloomon-env-candidates)
	  :action (lambda (env) (mg/bloomon-exec (format "%s %s" action env))))))

;; -----------------------------
;; Project Running
;; -----------------------------
(defun bloomon-prompt-project (action)
  (helm :sources
        (helm-make-source "Bloomon Project" 'helm-source-sync
          :candidates (bloomon-project-candidates)
          :action action
          )))

(defun bloomon-project-candidates ()
  "Get all of the project names which contain a docker-compose.yaml file in root."
  (seq-filter
   (lambda (path) (directory-files path nil "docker-compose.yaml"))
   projectile-known-projects))

(defun bloomon-run-project ()
  (interactive)
  (bloomon-prompt-project
   (lambda (project)
     (let ((default-directory project))
       (bloomon-run-process-with-focus
	"docker-compose up app"
	(projectile-project-name))))))

(defun bloomon-run-current-project ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (bloomon-run-process-with-focus "docker-compose up app" (projectile-project-name))))

;; -----------------------------
;; Bin/Cli wrapper
;; -----------------------------

(defun bloomon-bincli-candidates (project)
  "Use regexp to extract all action candidates of the bin/cli script for PROJECT."
  (let* ((default-directory project)
         (output (shell-command-to-string "bin/cli")))
    (bloomon-regexp-candidates
     (shell-command-to-string "bin/cli")
     "^[[:space:]]+\\(.*\\)")))

(defun bloomon-prompt-bin/cli-action (action)
  (helm :sources
        (helm-make-source "Bin/Cli Action" 'helm-source-sync
          :candidates (bloomon-bincli-candidates (projectile-project-root))
          :action action)))

(defun bloomon-project-bin/cli ()
  (interactive)
  (bloomon-prompt-bin/cli-action
   (lambda (action)
     (bloomon-run-process-with-focus
      (concat "docker-compose run --rm app bin/cli " action)
      (bloomon-buffer-name action)))))

;; -----------------------------
;; Bloomon cli wrapper
;; -----------------------------

(defun bloomon-cli-candidates ()
  "Use regexp to extract all action candidates of the bloomon cli script."
  (bloomon-regexp-candidates (shell-command-to-string (bloomon-cli-cmd))
			     "^\s+\\(\\w\\{2,\\}\\)"))

(defun bloomon-prompt-cli-action (action)
  (helm :sources
	(helm-make-source "Bloomon Cli" 'helm-source-sync
	  :candidates (bloomon-cli-candidates)
	  :action action)))

(defun bloomon-diff ()
  "Diff bloomon project secrets."
  (interactive)
  (helm :sources
	(helm-make-source "Branches" 'helm-source-sync
	  :candidates (magit-list-branch-names)
	  :action (lambda (branch)
		    (let* ((buff-name (bloomon-buffer-name "diff"))
			   (default-directory (projectile-project-root))
			   (*buffer* (bloomon-exec-interactive-process
				      (concat "bloomon diff " branch) buff-name)))
		      (switch-to-buffer-other-window *buffer*))))))

(defun bloomon-decrypt ()
  (interactive)
  (helm :sources
	(helm-make-source "Decrypt Env" 'helm-source-sync
	  :candidates (bloomon-env-candidates)
	  :action (lambda (env)
		    (message
		     (string-join
		      (seq-filter
		       (lambda (s) (message s) (string-prefix-p "?rw" s))
		       (split-string (shell-command-to-string (concat "bloomon decrypt " env)) "\n"))
		      "\n"))))))

(defun bloomon-encrypt ()
  (interactive)
  (helm :sources
	(helm-make-source "Encrypt Env" 'helm-source-sync
	  :candidates (bloomon-env-candidates)
	  :action (lambda (env)
		    (delete-directory
		     (concat (projectile-project-root) "secrets/.mypy_cache") t)
		    (shell-command-to-string (concat "bloomon encrypt " env))
		    (message "Encrypt Done")))))

(provide 'bloomon)
;;; bloomon.el ends here
