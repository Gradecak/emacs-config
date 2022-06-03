(require 'projectile)
(require 'term)

(defcustom python-better-shell-command "docker-compose run --rm app bin/cli shell"
  "Shell command to execute for python-better-shell."
  :group 'python)

(defvar python-better-shell-buffer nil)

(defun python-better-shell-sentinel (proc msg)
  "Improved sentinel for the shell process.
kills the shell window once the process ends with a 0 status code"
  (term-sentinel proc msg)
  (when (and (memq (process-status proc) '(signal exit))
	     (eq (process-exit-status proc) 0))
    (kill-buffer (process-buffer proc))))


(defun python-shell (&optional switch-focus-p file)
  (let* ((default-directory (projectile-project-root))
	 (args (split-string-and-unquote python-better-shell-command))
	 (cmd (car args))
         (switches (cdr args))
	 (process-name (format "%s-shell" (projectile-project-name)))
         (shellbuf (apply 'make-term process-name cmd file switches)))
    (setq python-better-shell-buffer shellbuf)
    (with-current-buffer shellbuf
      (term-mode)
      (term-char-mode))
    (set-process-sentinel (get-buffer-process shellbuf) 'python-better-shell-sentinel)
    (if switch-focus-p
	(switch-to-buffer-other-window shellbuf)
      (display-buffer shellbuf t))
    (get-buffer-process shellbuf)))

(defun python-better-shell ()
  "Run python shell in a `term' buffer."
  (interactive)
  (python-shell t))

(defun python-better-shell-send-buffer ()
  (interactive)
  (python-shell t (buffer-file-name)))

(defun python-better-shell-send-region ()
  (interactive)
  (let ((proc (get-buffer-process python-better-shell-buffer))
	min max command)
    (unless proc
      (error "No active shell process"))
    (if (use-region-p)
	(setq min (region-beginning)
	      max (region-end)))
    (setq command (concat (buffer-substring min max) "\n"))
    (term-simple-send proc command)))


(provide 'python-better-shell)
