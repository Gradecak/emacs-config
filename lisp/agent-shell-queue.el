;;; agent-shell-queue.el --- Notification queue for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Surfaces blocking events from all agent-shell buffers in a side-window buffer.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'agent-shell)

(defgroup agent-shell-queue nil
  "Notification queue for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-queue-")

(defcustom agent-shell-queue-max-preview-lines 5
  "Maximum number of lines to show in response preview."
  :type 'integer)

(defcustom agent-shell-queue-max-response-chars 500
  "Maximum characters to capture from agent's last response."
  :type 'integer)

(defcustom agent-shell-queue-notify t
  "When non-nil, send a system notification for new events if Emacs is unfocused."
  :type 'boolean)

;;; Faces

(defface agent-shell-queue-card-selected
  '((t :inherit highlight :extend t))
  "Face for the background of the currently selected card."
  :group 'agent-shell-queue)

(defface agent-shell-queue-type-permission
  '((t :inherit warning :weight bold))
  "Face for the permission type indicator."
  :group 'agent-shell-queue)

(defface agent-shell-queue-type-prompt
  '((t :inherit success :weight bold))
  "Face for the prompt/awaiting-input type indicator."
  :group 'agent-shell-queue)

(defface agent-shell-queue-shell-label
  '((t :inherit font-lock-function-name-face))
  "Face for the shell buffer name and perspective label."
  :group 'agent-shell-queue)

(defface agent-shell-queue-preview
  '((t :inherit font-lock-doc-face))
  "Face for the last-response preview text."
  :group 'agent-shell-queue)

(defface agent-shell-queue-separator
  '((t :inherit font-lock-comment-face))
  "Face for horizontal rule separators between cards."
  :group 'agent-shell-queue)

(defface agent-shell-queue-empty
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the empty-state message."
  :group 'agent-shell-queue)

;;; State variables

(defvar agent-shell-queue--entries nil
  "Global list of pending queue entries, ordered by timestamp.")

(defvar agent-shell-queue--selected-id nil
  "The :id of the currently selected entry.")


(defconst agent-shell-queue--buffer-name "*Agent Queue*"
  "Name of the queue buffer.")

(defvar agent-shell-queue--id-counter 0
  "Counter for generating unique entry IDs.")

(defvar agent-shell-queue--peek-return nil
  "When non-nil, a plist (:window W :orig-buf B :foreign-buf F) to restore after peek.")

(defun agent-shell-queue--peek-restore (&optional replied-buffer)
  "Restore the peek window.
When REPLIED-BUFFER is non-nil, only restore if that buffer is
currently displayed in the peek window.  When nil, restore
unconditionally."
  (when-let* ((peek agent-shell-queue--peek-return)
              (win (plist-get peek :window))
              (orig-buf (plist-get peek :orig-buf))
              ((window-live-p win))
              ((buffer-live-p orig-buf)))
    (when (or (null replied-buffer)
              (eq (window-buffer win) replied-buffer))
      (agent-shell-queue--set-side-window-buffer win orig-buf)
      ;; Remove the foreign buffer from the current perspective.
      ;; perspective.el's set-window-buffer advice auto-added it;
      ;; use direct list removal to avoid bury-buffer side effects.
      (when-let* ((foreign (plist-get peek :foreign-buf))
                  ((bound-and-true-p persp-mode))
                  ((buffer-live-p foreign))
                  ((persp-is-current-buffer foreign)))
        (setf (persp-current-buffers) (remq foreign (persp-current-buffers))))
      (setq agent-shell-queue--peek-return nil))))

(defun agent-shell-queue-peek-back ()
  "Return to the agent-shell buffer you were viewing before a peek jump.
Restores the original buffer and focuses the queue window."
  (interactive)
  (if agent-shell-queue--peek-return
      (progn
        (agent-shell-queue--peek-restore)
        (agent-shell-queue-focus))
    (message "No peek to return from.")))

;;; Major mode for the queue buffer

(define-derived-mode agent-shell-queue-list-mode special-mode "ASQ"
  "Major mode for the agent-shell notification queue buffer."
  :group 'agent-shell-queue
  (setq-local cursor-type nil
              truncate-lines t
              buffer-read-only t))

(let ((map agent-shell-queue-list-mode-map))
  (define-key map (kbd "n") #'agent-shell-queue-next)
  (define-key map (kbd "p") #'agent-shell-queue-prev)
  (define-key map (kbd "<down>") #'agent-shell-queue-next)
  (define-key map (kbd "<up>") #'agent-shell-queue-prev)
  (define-key map (kbd "y") #'agent-shell-queue-allow)
  (define-key map (kbd "d") #'agent-shell-queue-reject)
  (define-key map (kbd "RET") #'agent-shell-queue-jump)
  (define-key map (kbd "o") #'agent-shell-queue-jump)
  (define-key map (kbd "v") #'agent-shell-queue-peek)
  (define-key map (kbd "g") #'agent-shell-queue-refresh))

;;; System notifications

(defun agent-shell-queue--emacs-focused-p ()
  "Return non-nil if any Emacs frame currently has focus."
  (seq-some #'frame-focus-state (frame-list)))

(defun agent-shell-queue--notify (type title shell-buffer)
  "Send a system notification for a new queue event.
Only sends if `agent-shell-queue-notify' is non-nil and Emacs is
not focused.  TYPE is `permission' or `prompt', TITLE is the event
title, SHELL-BUFFER is the originating buffer."
  (when (and agent-shell-queue-notify
             (not (agent-shell-queue--emacs-focused-p)))
    (let* ((label (agent-shell-queue--shell-label shell-buffer))
           (persp (agent-shell-queue--persp-name shell-buffer))
           (title-str (if persp
                          (format "Agent Shell — %s (%s)" label persp)
                        (format "Agent Shell — %s" label)))
           (message (pcase type
                      ('permission (format "Permission: %s" title))
                      ('prompt "Awaiting input"))))
      (start-process "asq-notify" nil
                     "terminal-notifier"
                     "-title" title-str
                     "-message" message
                     "-sender" "org.gnu.Emacs"
                     "-group" "agent-shell-queue"))))

;;; Mode-based window dedication

(defun agent-shell-queue--set-side-window-buffer (win buf)
  "Set WIN's buffer to BUF, handling strong dedication.
Temporarily un-dedicates WIN, swaps the buffer, then re-dedicates.
Side windows must stay dedicated or Emacs deletes them."
  (let ((ded (window-dedicated-p win)))
    (when ded (set-window-dedicated-p win nil))
    (set-window-buffer win buf)
    (when ded (set-window-dedicated-p win ded))))

(defun agent-shell-queue--guard-agent-window (_frame)
  "Ensure windows with `agent-shell-mode-dedicated' only show agent-shell buffers.
Added to `window-buffer-change-functions'.  When a non-agent-shell
buffer is placed in a dedicated window, reverts to the last known
agent-shell buffer."
  (dolist (win (window-list))
    (when (window-parameter win 'agent-shell-mode-dedicated)
      (let ((buf (window-buffer win)))
        (if (with-current-buffer buf (derived-mode-p 'agent-shell-mode))
            (set-window-parameter win 'agent-shell--prev-buffer buf)
          (when-let* ((prev (window-parameter win 'agent-shell--prev-buffer))
                      ((buffer-live-p prev)))
            (agent-shell-queue--set-side-window-buffer win prev)))))))

;;; Entry management

(cl-defun agent-shell-queue--push (&key type shell-buffer title data)
  "Push a new entry to the queue.
TYPE is `permission' or `prompt'.
SHELL-BUFFER is the originating agent-shell buffer.
TITLE is a short description string.
DATA is a type-specific alist."
  ;; Prevent duplicate permission entries (same request-id)
  (when (and (eq type 'permission)
             (alist-get :request-id data)
             (seq-find (lambda (e)
                         (and (eq 'permission (alist-get :type e))
                              (equal (alist-get :request-id data)
                                     (alist-get :request-id (alist-get :data e)))))
                       agent-shell-queue--entries))
    (cl-return-from agent-shell-queue--push nil))
  ;; Prevent duplicate prompt entries (same shell buffer)
  (when (and (eq type 'prompt)
             (seq-find (lambda (e)
                         (and (eq 'prompt (alist-get :type e))
                              (eq shell-buffer (alist-get :shell-buffer e))))
                       agent-shell-queue--entries))
    (cl-return-from agent-shell-queue--push nil))
  (let ((entry (list (cons :id (format "asq-%d" (cl-incf agent-shell-queue--id-counter)))
                     (cons :type type)
                     (cons :shell-buffer shell-buffer)
                     (cons :timestamp (float-time))
                     (cons :title title)
                     (cons :data data))))
    (setq agent-shell-queue--entries
          (append agent-shell-queue--entries (list entry)))
    ;; Auto-select first entry if nothing selected
    (unless agent-shell-queue--selected-id
      (setq agent-shell-queue--selected-id (alist-get :id entry)))
    (agent-shell-queue--notify type title shell-buffer)
    (agent-shell-queue--refresh)
    entry))

(defun agent-shell-queue--remove (id)
  "Remove the entry with ID from the queue."
  (setq agent-shell-queue--entries
        (seq-remove (lambda (entry) (string= id (alist-get :id entry)))
                    agent-shell-queue--entries))
  ;; If we removed the selected entry, select the first remaining
  (when (and agent-shell-queue--selected-id
             (string= id agent-shell-queue--selected-id))
    (setq agent-shell-queue--selected-id
          (alist-get :id (car agent-shell-queue--entries))))
  (agent-shell-queue--refresh))

(defun agent-shell-queue--clear-for-buffer (shell-buffer)
  "Remove all entries originating from SHELL-BUFFER."
  (setq agent-shell-queue--entries
        (seq-remove (lambda (entry) (eq shell-buffer (alist-get :shell-buffer entry)))
                    agent-shell-queue--entries))
  (when (and agent-shell-queue--selected-id
             (not (seq-find (lambda (e) (string= agent-shell-queue--selected-id (alist-get :id e)))
                            agent-shell-queue--entries)))
    (setq agent-shell-queue--selected-id
          (alist-get :id (car agent-shell-queue--entries))))
  (agent-shell-queue--refresh))

(defun agent-shell-queue--find (id)
  "Return the entry with ID, or nil."
  (seq-find (lambda (entry) (string= id (alist-get :id entry)))
            agent-shell-queue--entries))

;;; Navigation

(defun agent-shell-queue--selected-index ()
  "Return the index of the selected entry, or 0."
  (or (seq-position agent-shell-queue--entries
                    agent-shell-queue--selected-id
                    (lambda (entry id) (string= (alist-get :id entry) id)))
      0))

(defun agent-shell-queue--selected-entry ()
  "Return the currently selected entry."
  (when agent-shell-queue--selected-id
    (agent-shell-queue--find agent-shell-queue--selected-id)))

(defun agent-shell-queue-next ()
  "Select the next entry in the queue."
  (interactive)
  (when agent-shell-queue--entries
    (let* ((idx (agent-shell-queue--selected-index))
           (next (min (1+ idx) (1- (length agent-shell-queue--entries)))))
      (setq agent-shell-queue--selected-id
            (alist-get :id (nth next agent-shell-queue--entries)))
      (agent-shell-queue--refresh))))

(defun agent-shell-queue-prev ()
  "Select the previous entry in the queue."
  (interactive)
  (when agent-shell-queue--entries
    (let* ((idx (agent-shell-queue--selected-index))
           (prev (max 0 (1- idx))))
      (setq agent-shell-queue--selected-id
            (alist-get :id (nth prev agent-shell-queue--entries)))
      (agent-shell-queue--refresh))))

;;; Actions (keyboard commands delegate to --act-* helpers)

(defun agent-shell-queue-allow ()
  "Allow the selected permission request."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry)))
    (agent-shell-queue--act-allow (alist-get :id entry))))

(defun agent-shell-queue-reject ()
  "Reject the selected permission request."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry)))
    (agent-shell-queue--act-reject (alist-get :id entry))))

(defun agent-shell-queue-jump ()
  "Jump to the originating buffer of the selected entry.
Switches to the perspective owning the shell buffer."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry)))
    (agent-shell-queue--act-jump (alist-get :id entry))))

(defun agent-shell-queue-peek ()
  "Peek at the originating buffer of the selected entry.
Shows the shell buffer in the current agent-shell window without
switching perspective.  After replying, the window reverts."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry)))
    (agent-shell-queue--act-peek (alist-get :id entry))))

(defun agent-shell-queue-refresh ()
  "Manually refresh the queue buffer."
  (interactive)
  (agent-shell-queue--refresh))

;;; Button action helpers (called from card buttons, take explicit ID)

(defun agent-shell-queue--act-allow (id)
  "Allow the permission entry with ID."
  (when-let* ((entry (agent-shell-queue--find id))
              ((eq 'permission (alist-get :type entry)))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer))
              (data (alist-get :data entry))
              (request-id (alist-get :request-id data))
              (tool-call-id (alist-get :tool-call-id data))
              (option-id (alist-get :allow-option-id data)))
    (condition-case err
        (with-current-buffer shell-buffer
          (agent-shell--send-permission-response
           :client (map-elt (agent-shell--state) :client)
           :request-id request-id
           :option-id option-id
           :state (agent-shell--state)
           :tool-call-id tool-call-id
           :message-text "Allowed from queue"))
      (error (message "Queue: allow failed (stale entry?): %s" err)))
    (agent-shell-queue--remove id)))

(defun agent-shell-queue--act-reject (id)
  "Reject the permission entry with ID."
  (when-let* ((entry (agent-shell-queue--find id))
              ((eq 'permission (alist-get :type entry)))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer))
              (data (alist-get :data entry))
              (request-id (alist-get :request-id data))
              (tool-call-id (alist-get :tool-call-id data)))
    (let ((option-id (alist-get :reject-option-id data)))
      (condition-case err
          (with-current-buffer shell-buffer
            (agent-shell--send-permission-response
             :client (map-elt (agent-shell--state) :client)
             :request-id request-id
             :option-id option-id
             :cancelled (not option-id)
             :state (agent-shell--state)
             :tool-call-id tool-call-id
             :message-text "Rejected from queue")
            (agent-shell-interrupt t))
        (error (message "Queue: reject failed (stale entry?): %s" err))))
    (agent-shell-queue--remove id)))

(defun agent-shell-queue--act-jump (id)
  "Jump to the shell buffer of entry with ID.
Switches to the perspective that owns the shell buffer, then
displays it.  Prompt entries stay in the queue until the user
actually submits a reply."
  (when-let* ((entry (agent-shell-queue--find id))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer)))
    ;; Switch perspective if the buffer lives in a different one
    (when (and (bound-and-true-p persp-mode)
               (not (persp-is-current-buffer shell-buffer)))
      (when-let ((other (persp-buffer-in-other-p shell-buffer)))
        (persp-switch (cdr other))))
    (pop-to-buffer shell-buffer)
    (goto-char (point-max))))

(defun agent-shell-queue--act-peek (id)
  "Peek at the shell buffer of entry with ID.
Shows the buffer in the existing agent-shell side window without
switching perspective.  Saves window state so it can be restored
via `agent-shell-queue-peek-back' or automatically after a reply.
Prompt entries stay in the queue until the user actually submits."
  (when-let* ((entry (agent-shell-queue--find id))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer)))
    (let ((agent-win (seq-find
                      (lambda (w)
                        (with-current-buffer (window-buffer w)
                          (derived-mode-p 'agent-shell-mode)))
                      (window-list)))
          ;; Remember whether this buffer was already in our perspective
          (was-in-persp (or (not (bound-and-true-p persp-mode))
                            (persp-is-current-buffer shell-buffer))))
      ;; Fall back to display-buffer if no agent-shell window exists yet
      (unless agent-win
        (display-buffer shell-buffer)
        (setq agent-win (get-buffer-window shell-buffer)))
      (when agent-win
        ;; Save current state for peek-restore
        (when (not (eq (window-buffer agent-win) shell-buffer))
          (setq agent-shell-queue--peek-return
                (list :window agent-win
                      :orig-buf (window-buffer agent-win)
                      ;; Track foreign buffer so peek-restore can clean
                      ;; up the perspective association that
                      ;; perspective.el's set-window-buffer advice adds.
                      :foreign-buf (unless was-in-persp shell-buffer))))
        (agent-shell-queue--set-side-window-buffer agent-win shell-buffer)
        (select-window agent-win)
        (goto-char (point-max))))))

;;; Card rendering

(defun agent-shell-queue--shell-label (shell-buffer)
  "Return a short label for SHELL-BUFFER like [claude@project-name]."
  (if (buffer-live-p shell-buffer)
      (let ((name (buffer-name shell-buffer)))
        (format "[%s]" (replace-regexp-in-string "\\*\\|Agent @ " "" name)))
    "[dead]"))

(defun agent-shell-queue--persp-name (shell-buffer)
  "Return the perspective name that owns SHELL-BUFFER, or nil."
  (when (and (bound-and-true-p persp-mode)
             (buffer-live-p shell-buffer))
    (if (persp-is-current-buffer shell-buffer)
        (persp-current-name)
      (when-let ((other (persp-buffer-in-other-p shell-buffer)))
        (cdr other)))))

(defcustom agent-shell-queue-separator-width 40
  "Width of the horizontal separator between cards."
  :type 'integer
  :group 'agent-shell-queue)

(defun agent-shell-queue--insert-separator ()
  "Insert a horizontal rule between cards."
  (insert (propertize (make-string agent-shell-queue-separator-width ?─)
                      'face 'agent-shell-queue-separator))
  (insert "\n"))

(defun agent-shell-queue--insert-card (entry selected-p)
  "Insert a card for ENTRY into the current buffer.
SELECTED-P non-nil means this card is the active selection."
  (let* ((type (alist-get :type entry))
         (title (alist-get :title entry))
         (data (alist-get :data entry))
         (shell-buf (alist-get :shell-buffer entry))
         (id (alist-get :id entry))
         (label (agent-shell-queue--shell-label shell-buf))
         (persp (agent-shell-queue--persp-name shell-buf))
         (beg (point)))
    ;; Line 1: Type indicator + title
    (pcase type
      ('permission
       (insert (propertize "  Permission: " 'face 'agent-shell-queue-type-permission))
       (insert (propertize (or title "unknown") 'face 'agent-shell-queue-type-permission)))
      ('prompt
       (insert (propertize "  Awaiting input" 'face 'agent-shell-queue-type-prompt))))
    (insert "\n")

    ;; Line 2: Shell label + perspective name
    (insert (propertize (concat "  " label
                                (when persp (format "  %s" persp)))
                        'face 'agent-shell-queue-shell-label))
    (insert "\n")

    ;; Line 3+: Response preview (prompt entries only)
    (when (and (eq type 'prompt) (alist-get :last-response data))
      (let* ((response (alist-get :last-response data))
             (truncated (if (> (length response) agent-shell-queue-max-response-chars)
                            (concat (substring response 0 agent-shell-queue-max-response-chars) "...")
                          response))
             (preview-lines (split-string truncated "\n"))
             (limited (seq-take preview-lines agent-shell-queue-max-preview-lines)))
        (when (> (length preview-lines) agent-shell-queue-max-preview-lines)
          (setq limited (append limited (list "..."))))
        (dolist (line limited)
          (insert (propertize (concat "  │ " line) 'face 'agent-shell-queue-preview))
          (insert "\n"))))

    ;; Buttons row
    (insert "  ")
    (when (eq type 'permission)
      (insert-text-button "Allow [y]"
                          'action (lambda (_btn) (agent-shell-queue--act-allow id))
                          'face 'custom-button
                          'follow-link t)
      (insert "  ")
      (insert-text-button "Reject [d]"
                          'action (lambda (_btn) (agent-shell-queue--act-reject id))
                          'face 'custom-button
                          'follow-link t)
      (insert "  "))
    (insert-text-button "Jump [RET]"
                        'action (lambda (_btn) (agent-shell-queue--act-jump id))
                        'face 'custom-button
                        'follow-link t)
    (when (eq type 'prompt)
      (insert "  ")
      (insert-text-button "Peek [v]"
                          'action (lambda (_btn) (agent-shell-queue--act-peek id))
                          'face 'custom-button
                          'follow-link t))
    (insert "\n")

    ;; Selection overlay on entire card
    (when selected-p
      (let ((ov (make-overlay beg (point))))
        (overlay-put ov 'face 'agent-shell-queue-card-selected)
        (overlay-put ov 'agent-shell-queue-selection t)))

    ;; Text property for point-based card identification
    (put-text-property beg (point) 'agent-shell-queue-entry-id id)))

(defun agent-shell-queue--render ()
  "Render all queue entries into the queue buffer."
  (let ((buf (get-buffer-create agent-shell-queue--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'agent-shell-queue-list-mode)
        (agent-shell-queue-list-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (if (null agent-shell-queue--entries)
            (insert (propertize "  No pending events."
                                'face 'agent-shell-queue-empty))
          (let ((first t))
            (dolist (entry agent-shell-queue--entries)
              (unless first
                (agent-shell-queue--insert-separator))
              (setq first nil)
              (agent-shell-queue--insert-card
               entry
               (string= (alist-get :id entry)
                         agent-shell-queue--selected-id))))))
      ;; Position point on the selected card
      (goto-char (point-min))
      (when agent-shell-queue--selected-id
        (let ((pos (text-property-any (point-min) (point-max)
                                      'agent-shell-queue-entry-id
                                      agent-shell-queue--selected-id)))
          (when pos (goto-char pos)))))
    buf))

;;; Display management

(defun agent-shell-queue--prune-dead ()
  "Remove entries whose shell buffer no longer exists."
  (setq agent-shell-queue--entries
        (seq-filter (lambda (entry)
                      (buffer-live-p (alist-get :shell-buffer entry)))
                    agent-shell-queue--entries)))

(defun agent-shell-queue--ensure-displayed ()
  "Ensure the queue buffer is visible in a side window."
  (when-let ((buf (get-buffer agent-shell-queue--buffer-name)))
    (unless (get-buffer-window buf t)
      (display-buffer buf))))

(defun agent-shell-queue--refresh ()
  "Re-render the queue buffer and ensure it is displayed."
  (agent-shell-queue--prune-dead)
  (agent-shell-queue--render)
  (agent-shell-queue--ensure-displayed))

(defun agent-shell-queue-focus ()
  "Switch focus to the queue buffer window.
Auto-selects the first entry if nothing is currently selected."
  (interactive)
  (agent-shell-queue--ensure-displayed)
  (when (and agent-shell-queue--entries
             (not (agent-shell-queue--selected-entry)))
    (setq agent-shell-queue--selected-id
          (alist-get :id (car agent-shell-queue--entries)))
    (agent-shell-queue--render))
  (when-let ((win (get-buffer-window agent-shell-queue--buffer-name t)))
    (select-window win)))

;;; Event interception via advice and subscriptions

(defun agent-shell-queue--advice-on-request (&rest args)
  "After-advice on `agent-shell--on-request' to capture permission requests."
  (when-let* ((acp-request (plist-get args :acp-request))
              ((equal (map-elt acp-request 'method) "session/request_permission"))
              (state (plist-get args :state))
              (tool-call-id (map-nested-elt acp-request '(params toolCall toolCallId)))
              (title (or (map-nested-elt acp-request '(params toolCall title))
                         tool-call-id))
              (request-id (map-elt acp-request 'id)))
    (let* ((options (map-nested-elt acp-request '(params options)))
           (allow-option (seq-find (lambda (o) (equal (map-elt o 'kind) "allow_once")) options))
           (reject-option (seq-find (lambda (o) (equal (map-elt o 'kind) "reject_once")) options)))
      (agent-shell-queue--push
       :type 'permission
       :shell-buffer (map-elt state :buffer)
       :title title
       :data (list (cons :request-id request-id)
                   (cons :tool-call-id tool-call-id)
                   (cons :allow-option-id (and allow-option (map-elt allow-option 'optionId)))
                   (cons :reject-option-id (and reject-option (map-elt reject-option 'optionId))))))))

(defun agent-shell-queue--advice-send-permission-response (&rest args)
  "After-advice on `agent-shell--send-permission-response'.
Removes the matching permission entry from the queue when a
permission is answered from any source (shell buffer UI or queue)."
  (when-let* ((request-id (plist-get args :request-id)))
    (when-let ((entry (seq-find
                       (lambda (e)
                         (and (eq 'permission (alist-get :type e))
                              (equal request-id
                                     (alist-get :request-id (alist-get :data e)))))
                       agent-shell-queue--entries)))
      (agent-shell-queue--remove (alist-get :id entry)))))

(defun agent-shell-queue--advice-send-request (orig-fn &rest args)
  "Around-advice on `agent-shell--send-request' to detect idle state."
  (let* ((orig-on-success (plist-get args :on-success))
         (state (plist-get args :state))
         (request (plist-get args :request))
         (method (and request (map-elt request :method)))
         (new-args (copy-sequence args)))
    (when (equal method "session/prompt")
      ;; A new prompt is being sent — clear any stale "awaiting input" entry
      ;; for this buffer (handles the case where user replied directly in shell)
      (when-let ((buf (map-elt state :buffer)))
        (setq agent-shell-queue--entries
              (seq-remove (lambda (e)
                            (and (eq 'prompt (alist-get :type e))
                                 (eq buf (alist-get :shell-buffer e))))
                          agent-shell-queue--entries))
        (agent-shell-queue--refresh)
        ;; Restore peek window if the reply came from the peeked buffer
        (agent-shell-queue--peek-restore buf))
      (setq new-args
            (plist-put new-args :on-success
                       (lambda (acp-response)
                         (when orig-on-success
                           (funcall orig-on-success acp-response))
                         (agent-shell-queue--on-agent-idle state)))))
    (apply orig-fn new-args)))

(defun agent-shell-queue--on-agent-idle (state)
  "Called when an agent finishes its turn.
Captures last response and pushes to queue."
  (when-let* ((buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((last-response (agent-shell-queue--capture-last-response)))
        (agent-shell-queue--push
         :type 'prompt
         :shell-buffer buffer
         :title "Awaiting input"
         :data (list (cons :last-response last-response)))))))

(defun agent-shell-queue--capture-last-response ()
  "Capture the last agent response text from the current shell buffer.
Returns a string truncated to `agent-shell-queue-max-response-chars'."
  (save-excursion
    (goto-char (point-max))
    (let* ((end (point))
           (start (or (save-excursion
                        (when (re-search-backward comint-prompt-regexp nil t 1)
                          (match-end 0)))
                      (point-min)))
           (text (string-trim (buffer-substring-no-properties start end))))
      (if (> (length text) agent-shell-queue-max-response-chars)
          (substring text 0 agent-shell-queue-max-response-chars)
        text))))

(defun agent-shell-queue--on-shell-killed ()
  "Hook for `kill-buffer-hook' in agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-queue--clear-for-buffer (current-buffer))
))

(defun agent-shell-queue--setup-shell ()
  "Set up queue integration for the current agent-shell buffer.
Called from `agent-shell-mode-hook'."
  (when (derived-mode-p 'agent-shell-mode)
    (add-hook 'kill-buffer-hook #'agent-shell-queue--on-shell-killed nil t)))

(defun agent-shell-queue--teardown-shell (buf)
  "Remove queue integration for shell BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (remove-hook 'kill-buffer-hook #'agent-shell-queue--on-shell-killed t)))
  (agent-shell-queue--clear-for-buffer buf))

;;; Global minor mode

;;;###autoload
(define-minor-mode agent-shell-queue-mode
  "Global minor mode for agent-shell notification queue."
  :global t
  :lighter " ASQ"
  (if agent-shell-queue-mode
      (progn
        ;; Install advice
        (advice-add 'agent-shell--on-request :after #'agent-shell-queue--advice-on-request)
        (advice-add 'agent-shell--send-request :around #'agent-shell-queue--advice-send-request)
        (advice-add 'agent-shell--send-permission-response :after #'agent-shell-queue--advice-send-permission-response)
        ;; Guard agent-shell windows to only accept agent-shell-mode buffers
        (add-hook 'window-buffer-change-functions #'agent-shell-queue--guard-agent-window)
        ;; Hook into new shell creation
        (add-hook 'agent-shell-mode-hook #'agent-shell-queue--setup-shell)
        ;; Set up existing shells
        (dolist (buf (agent-shell-buffers))
          (with-current-buffer buf
            (agent-shell-queue--setup-shell))))
    ;; Disable: remove advice, hooks
    (remove-hook 'window-buffer-change-functions #'agent-shell-queue--guard-agent-window)
    (advice-remove 'agent-shell--on-request #'agent-shell-queue--advice-on-request)
    (advice-remove 'agent-shell--send-request #'agent-shell-queue--advice-send-request)
    (advice-remove 'agent-shell--send-permission-response #'agent-shell-queue--advice-send-permission-response)
    (remove-hook 'agent-shell-mode-hook #'agent-shell-queue--setup-shell)
    (dolist (buf (agent-shell-buffers))
      (agent-shell-queue--teardown-shell buf))
    (setq agent-shell-queue--entries nil
          agent-shell-queue--selected-id nil
          agent-shell-queue--peek-return nil)
    ;; Kill the queue buffer and its window
    (when-let ((buf (get-buffer agent-shell-queue--buffer-name)))
      (when-let ((win (get-buffer-window buf t)))
        (delete-window win))
      (kill-buffer buf))))

(provide 'agent-shell-queue)
;;; agent-shell-queue.el ends here
