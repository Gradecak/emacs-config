(defun _bloomon-cli-path ()
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "which bloomon")))

(defun _bloomon-exec (cmd)
  (start-process-shell-command "" "out" (concat (_bloomon-cli-path) cmd)))

(defun _bloomon-prompt-secrets (action)
  (helm :sources
        (helm-make-source "Bloomon" 'helm-source-sync
          :candidates `("dev" "stg" "prod")
          :action action
          )))

(defun bloomon-encrypt-decrypt (cb)
  (let* ((root (projectile-project-root))
        (default-directory root))
    (_bloomon-prompt-secrets (lambda (env)
                               (message "operation %s for %s secrets (%s)" cb env root)
                               (_bloomon-exec (concat cb env))))))

(defun bloomon-decrypt ()
  (interactive)
  (bloomon-encrypt-decrypt " decrypt "))

(defun bloomon-encrypt ()
  (interactive)
  (let ((root (projectile-project-root)))
    (delete-file (concat root "secrets/.keep"))
    (delete-directory (concat root "secrets/.mypy_cache") t))
  (bloomon-encrypt-decrypt " encrypt "))

(defun msg-me (process event)
   (princ
     (format "Process: %s had the event '%s'" process event)))

(defun _bloomon-branch-prompt (action)
  )

(defun _bloomon-diff (branch)
  (let* ((process-connection-type t)
         (buff-name "*bloomon diff*")
         (diff-proc nil))
    (with-output-to-temp-buffer buff-name
      (setq diff-proc
            (start-process "diff" buff-name (_bloomon-cli-path) "diff" branch))
      (switch-to-buffer-other-window buff-name)
      (set-process-sentinel diff-proc 'msg-me)
      (special-mode)
      (diff-mode)
      )))

(defun bloomon-diff ()
  (interactive)
  (helm :sources
        (helm-make-source "Branches" 'helm-source-sync
          :candidates (magit-list-branch-names)
          :action '_bloomon-diff)))

(provide 'bloomon)
