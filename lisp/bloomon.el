;; -*- lexical-binding: t; -*-

;;; bloomon --- stuff

;;; Commentary:

(require  'project)
(require 'custom-functions)

;;; Code:


(defvar bloomon-cli-cmd "bloomon")

(define-minor-mode bloomon-process-mode
  "bloomon-process"
  :global nil
  :lighter bloomon-process
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-q") 'bloomon-dismiss-window-or-kill-buffer)
	    (define-key map (kbd "C-k") 'bloomon-kill-process)
	    (define-key map (kbd "C-S-r") 'bloomon-restart-process)
            map))

(defun bloomon-dismiss-window-or-kill-buffer ()
  "If the process associated with the buffer is finished, kill
buffer, otherwise quit window."
  (interactive)
  (if (get-process (buffer-name))
      (quit-window)
    (kill-buffer-and-window)))

(defun bloomon-kill-process ()
  "Kill the process of the current buffer only when the status code is
130 (cancelled).  This is specifically tailored to exiting docker-compose"
  (interactive)
  (let ((proc (get-process (buffer-name))))
    (when proc
      (set-process-sentinel proc (lambda (proc msg)
				   (when (and (memq (process-status proc) '(signal exit))
					      (memq (process-exit-status proc) '(1 130)))
				     (with-current-buffer
					 (process-buffer proc)
				       (kill-buffer-and-window)))))
      (interrupt-process proc))))

(defun bloomon-restart-process ()
  (interactive)
  (let ((proc (get-process (buffer-name))))
    (when proc
      (set-process-sentinel proc (lambda (proc msg)
				   (when (memq (process-status proc) '(signal exit))
				     (with-current-buffer (process-buffer proc)
				       (let ((inhibit-read-only t))
					 (erase-buffer)))
				     (bloomon-run-process-with-focus "docker-compose up app" (project-root (project-current))))))
      (interrupt-process proc))))

(defun split-window-horizontal (window)
  (split-window (frame-root-window) (frame-root-window) 'below))

(defun bloomon-exec-process (command proc-name)
  "Run the COMMAND with the given PROC-NAME."
  (pcase (split-string-and-unquote command)
    (`(,cmd . ,switches) (apply 'term-ansi-make-term proc-name cmd nil switches))))

(defun bloomon-existing-process-window ()
  "Try find a window with an existing bloomon process."
  (cl-loop for window in (window-list)
	   when (with-current-buffer (window-buffer window) (bound-and-true-p bloomon-process-mode))
	   return window))

(defun bloomon-run-process-with-focus (command proc-name)
  "Run the COMMAND with the given PROC-NAME and focus the buffer."
  (let ((split-window-preferred-function 'split-window-horizontal)
	(*buffer* (bloomon-exec-process command proc-name)))
    (with-current-buffer *buffer*
      (setq buffer-read-only t)
      (term-mode)
      (term-line-mode)
      (bloomon-process-mode))
    ;; try find existing window that has a bloomon-process running
    (if-let ((window (bloomon-existing-process-window)))
	(window--display-buffer *buffer* window 'window)
      (switch-to-buffer-other-window *buffer*))))

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
  (let (project-name (project-name))
    (if action
	(format "*%s-%s*" project-name action)
      (format "*%s*" project-name))))

(defun bloomon-env-candidates ()
  "Use regexp matching to extract secrets environments from the project root."
  (let ((files (directory-files (project-root (project-current)))))
    (remq nil (mapcar (lambda (file)
			(save-match-data
			  (string-match "secrets.\\(.*\\).enc" file)
			  (match-string 1 file)))
		      files))))

;; -----------------------------
;; Project Running
;; -----------------------------
(defun bloomon-prompt-project (action)
  (funcall action (consult--read
		   (bloomon-project-candidates)
		   :prompt "Bloomon Project: ")))

(defun bloomon-project-candidates ()
  "Get all of the project names which contain a docker-compose.yaml file in root."
  (seq-filter
   (lambda (path) (directory-files path nil "docker-compose.yaml"))
   (project-known-project-roots)))

(defun bloomon-run-project ()
  (interactive)
  (bloomon-prompt-project
   (lambda (project)
     (let ((default-directory project))
       (bloomon-run-process-with-focus
	"docker-compose up app"
	(project-name))))))

(defun bloomon-run-current-project ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (bloomon-run-process-with-focus "docker-compose up app" (project-name))))

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
  (funcall action (consult--read
		   (bloomon-bincli-candidates (project-root (project-current)))
		   :prompt "Bin/Cli Action: ")))

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

(defun bloomon-diff ()
  (interactive)
  (let* ((branch (consult--read (magit-list-branch-names)))
	 (buff-name (bloomon-buffer-name "diff"))
	 (default-directory (project-root (project-current)))
	 (*buffer* (bloomon-exec-process
		    (concat "bloomon diff " branch) buff-name)))
    (switch-to-buffer-other-window *buffer*)))


(defun bloomon-env-action (action)
  (funcall action (consult--read
		   (bloomon-env-candidates)
		   :prompt "Secrets Env")))

(defun bloomon-decrypt ()
  (interactive)
  (bloomon-env-action
   (lambda (env)
     (message
      (string-join
       (seq-filter
	(lambda (s) (message s) (string-prefix-p "?rw" s))
	(split-string (shell-command-to-string (concat "bloomon decrypt " env)) "\n"))
       "\n")))))

(defun bloomon-encrypt ()
  (interactive)
  (bloomon-env-action
   (lambda (env)
     (delete-directory
      (concat (project-root (project-current)) "secrets/.mypy_cache") t)
     (shell-command-to-string (concat "bloomon encrypt " env))
     (message "Encrypt Done"))))

(provide 'bloomon)
;;; bloomon.el ends here
