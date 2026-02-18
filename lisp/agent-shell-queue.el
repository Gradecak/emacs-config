;;; agent-shell-queue.el --- Notification queue for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Surfaces blocking events from all agent-shell buffers in a posframe overlay.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'agent-shell)
(require 'posframe)

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

(defcustom agent-shell-queue-posframe-width 80
  "Width of the queue posframe in columns."
  :type 'integer)

(defface agent-shell-queue-selected
  '((t :inherit highlight))
  "Face for the currently selected queue entry.")

(defface agent-shell-queue-header
  '((t :inherit bold))
  "Face for entry header lines.")

(defface agent-shell-queue-preview
  '((t :inherit font-lock-comment-face))
  "Face for response preview text.")

(defvar agent-shell-queue--entries nil
  "Global list of pending queue entries, ordered by timestamp.")

(defvar agent-shell-queue--selected-id nil
  "The :id of the currently selected entry.")

(defvar agent-shell-queue--subscriptions nil
  "Alist of (shell-buffer . subscription-token) for cleanup.")

(defvar agent-shell-queue--dismissed nil
  "Non-nil when the posframe has been manually dismissed with `q'.")

(defconst agent-shell-queue--buffer-name " *agent-shell-queue*"
  "Name of the buffer used for the posframe.")

(defvar agent-shell-queue--id-counter 0
  "Counter for generating unique entry IDs.")

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
                              (string= (alist-get :request-id data)
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

(defvar agent-shell-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'agent-shell-queue-prev)
    (define-key map (kbd "<down>") #'agent-shell-queue-next)
    (define-key map (kbd "p") #'agent-shell-queue-prev)
    (define-key map (kbd "n") #'agent-shell-queue-next)
    (define-key map (kbd "y") #'agent-shell-queue-allow)
    (define-key map (kbd "d") #'agent-shell-queue-reject)
    (define-key map (kbd "RET") #'agent-shell-queue-reply)
    (define-key map (kbd "o") #'agent-shell-queue-jump)
    (define-key map (kbd "q") #'agent-shell-queue-dismiss)
    map)
  "Keymap for the agent-shell-queue posframe buffer.")

;;; Navigation

(defun agent-shell-queue--selected-index ()
  "Return the index of the selected entry, or 0."
  (or (seq-position agent-shell-queue--entries
                    agent-shell-queue--selected-id
                    (lambda (entry id) (string= (alist-get :id entry) id)))
      0))

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

;;; Actions

(defun agent-shell-queue--selected-entry ()
  "Return the currently selected entry."
  (when agent-shell-queue--selected-id
    (agent-shell-queue--find agent-shell-queue--selected-id)))

(defun agent-shell-queue-allow ()
  "Allow the selected permission request."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry))
              ((eq 'permission (alist-get :type entry)))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer))
              (data (alist-get :data entry))
              (request-id (alist-get :request-id data))
              (tool-call-id (alist-get :tool-call-id data)))
    (with-current-buffer shell-buffer
      (agent-shell--send-permission-response
       :client (map-elt (agent-shell--state) :client)
       :request-id request-id
       :option-id "allow_once"
       :state (agent-shell--state)
       :tool-call-id tool-call-id
       :message-text "Allowed from queue"))
    (agent-shell-queue--remove (alist-get :id entry))))

(defun agent-shell-queue-reject ()
  "Reject the selected permission request."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry))
              ((eq 'permission (alist-get :type entry)))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer))
              (data (alist-get :data entry))
              (request-id (alist-get :request-id data))
              (tool-call-id (alist-get :tool-call-id data)))
    (with-current-buffer shell-buffer
      (agent-shell--send-permission-response
       :client (map-elt (agent-shell--state) :client)
       :request-id request-id
       :cancelled t
       :state (agent-shell--state)
       :tool-call-id tool-call-id
       :message-text "Rejected from queue")
      (agent-shell-interrupt t))
    (agent-shell-queue--remove (alist-get :id entry))))

(defun agent-shell-queue-reply ()
  "Reply to the selected prompt entry via minibuffer."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry))
              ((eq 'prompt (alist-get :type entry)))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer)))
    (let ((input (read-string "Reply: ")))
      (when (and input (not (string-empty-p input)))
        (with-current-buffer shell-buffer
          (goto-char (point-max))
          (insert input)
          (shell-maker-submit))
        (agent-shell-queue--remove (alist-get :id entry))))))

(defun agent-shell-queue-jump ()
  "Jump to the originating buffer of the selected entry."
  (interactive)
  (when-let* ((entry (agent-shell-queue--selected-entry))
              (shell-buffer (alist-get :shell-buffer entry))
              ((buffer-live-p shell-buffer)))
    (agent-shell-queue-dismiss)
    (pop-to-buffer shell-buffer)))

(defun agent-shell-queue--shell-label (shell-buffer)
  "Return a short label for SHELL-BUFFER like [claude@project-name]."
  (if (buffer-live-p shell-buffer)
      (let ((name (buffer-name shell-buffer)))
        (format "[%s]" (replace-regexp-in-string "\\*\\|Agent @ " "" name)))
    "[dead]"))

(defun agent-shell-queue--format-entry (entry selected-p)
  "Format ENTRY as a string for display. SELECTED-P highlights it."
  (let* ((type (alist-get :type entry))
         (title (alist-get :title entry))
         (shell-buffer (alist-get :shell-buffer entry))
         (data (alist-get :data entry))
         (label (agent-shell-queue--shell-label shell-buffer))
         (header (pcase type
                   ('permission
                    (format "%s Permission: %s  [y] [d]" label title))
                   ('prompt
                    (format "%s Awaiting input  [RET] [o]" label))))
         (header-face (if selected-p 'agent-shell-queue-selected 'agent-shell-queue-header))
         (lines (list (propertize header 'face header-face
                                  'agent-shell-queue-entry-id (alist-get :id entry)))))
    ;; Add response preview for prompt entries
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
          (push (propertize (concat "  | " line) 'face 'agent-shell-queue-preview
                            'agent-shell-queue-entry-id (alist-get :id entry))
                lines))))
    (string-join (nreverse lines) "\n")))

(defun agent-shell-queue--render ()
  "Render all queue entries into the queue buffer."
  (let ((buf (get-buffer-create agent-shell-queue--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null agent-shell-queue--entries)
            (insert "No pending events.")
          (let ((formatted (mapcar
                            (lambda (entry)
                              (agent-shell-queue--format-entry
                               entry
                               (string= (alist-get :id entry)
                                        agent-shell-queue--selected-id)))
                            agent-shell-queue--entries)))
            (insert (string-join formatted "\n\n"))))
        (setq buffer-read-only t)
        (when agent-shell-queue-mode-map
          (use-local-map agent-shell-queue-mode-map))))
    buf))

(defun agent-shell-queue--prune-dead ()
  "Remove entries whose shell buffer no longer exists."
  (setq agent-shell-queue--entries
        (seq-filter (lambda (entry)
                      (buffer-live-p (alist-get :shell-buffer entry)))
                    agent-shell-queue--entries)))

(defun agent-shell-queue--refresh ()
  "Re-render the queue buffer and show/hide the posframe."
  (agent-shell-queue--prune-dead)
  (if agent-shell-queue--entries
      (progn
        (agent-shell-queue--render)
        (unless agent-shell-queue--dismissed
          (when (fboundp 'posframe-show)
            (agent-shell-queue-show))))
    (agent-shell-queue--hide)))

(defun agent-shell-queue-show ()
  "Show the queue posframe."
  (interactive)
  (when agent-shell-queue--entries
    (agent-shell-queue--render)
    (setq agent-shell-queue--dismissed nil)
    (posframe-show agent-shell-queue--buffer-name
                   :position (point-min)
                   :poshandler #'posframe-poshandler-frame-top-right-corner
                   :width agent-shell-queue-posframe-width
                   :border-width 1
                   :border-color "gray50"
                   :accept-focus t)))

(defun agent-shell-queue--hide ()
  "Hide the queue posframe."
  (when (fboundp 'posframe-hide)
    (posframe-hide agent-shell-queue--buffer-name)))

(defun agent-shell-queue-dismiss ()
  "Dismiss the posframe without clearing the queue."
  (interactive)
  (setq agent-shell-queue--dismissed t)
  (agent-shell-queue--hide))

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
    (agent-shell-queue--push
     :type 'permission
     :shell-buffer (map-elt state :buffer)
     :title title
     :data (list (cons :request-id request-id)
                 (cons :tool-call-id tool-call-id)))))

(defun agent-shell-queue--advice-send-request (orig-fn &rest args)
  "Around-advice on `agent-shell--send-request' to detect idle state."
  (let* ((orig-on-success (plist-get args :on-success))
         (state (plist-get args :state))
         (request (plist-get args :request))
         (method (and request (map-elt request 'method)))
         (new-args (copy-sequence args)))
    (when (equal method "session/message")
      (setq new-args
            (plist-put new-args :on-success
                       (lambda (acp-response)
                         (when orig-on-success
                           (funcall orig-on-success acp-response))
                         (agent-shell-queue--on-agent-idle state)))))
    (apply orig-fn new-args)))

(defun agent-shell-queue--on-agent-idle (state)
  "Called when an agent finishes its turn. Captures last response and pushes to queue."
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
                        (when (re-search-backward shell-maker-prompt-regexp nil t 1)
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
    (setq agent-shell-queue--subscriptions
          (assq-delete-all (current-buffer) agent-shell-queue--subscriptions))))

(defun agent-shell-queue--on-permission-response (event)
  "Remove queue entry when permission is answered directly in shell buffer."
  (when-let* ((data (alist-get :data event))
              (request-id (alist-get :request-id data)))
    (when-let ((entry (seq-find
                       (lambda (e)
                         (string= request-id
                                  (alist-get :request-id (alist-get :data e))))
                       agent-shell-queue--entries)))
      (agent-shell-queue--remove (alist-get :id entry)))))

(defun agent-shell-queue--setup-shell ()
  "Set up queue integration for the current agent-shell buffer.
Called from `agent-shell-mode-hook'."
  (when (derived-mode-p 'agent-shell-mode)
    (let ((buf (current-buffer)))
      ;; Subscribe to permission-response to sync direct answers
      (let ((token (agent-shell-subscribe-to
                    :shell-buffer buf
                    :event 'permission-response
                    :on-event #'agent-shell-queue--on-permission-response)))
        (push (cons buf token) agent-shell-queue--subscriptions))
      ;; Clean up on buffer kill
      (add-hook 'kill-buffer-hook #'agent-shell-queue--on-shell-killed nil t))))

(defun agent-shell-queue--teardown-shell (buf)
  "Remove queue integration for shell BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when-let ((token (alist-get buf agent-shell-queue--subscriptions)))
        (agent-shell-unsubscribe token))
      (remove-hook 'kill-buffer-hook #'agent-shell-queue--on-shell-killed t)))
  (agent-shell-queue--clear-for-buffer buf)
  (setq agent-shell-queue--subscriptions
        (assq-delete-all buf agent-shell-queue--subscriptions)))

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
        ;; Hook into new shell creation
        (add-hook 'agent-shell-mode-hook #'agent-shell-queue--setup-shell)
        ;; Set up existing shells
        (dolist (buf (agent-shell-buffers))
          (with-current-buffer buf
            (agent-shell-queue--setup-shell))))
    ;; Disable: remove advice, hooks, subscriptions
    (advice-remove 'agent-shell--on-request #'agent-shell-queue--advice-on-request)
    (advice-remove 'agent-shell--send-request #'agent-shell-queue--advice-send-request)
    (remove-hook 'agent-shell-mode-hook #'agent-shell-queue--setup-shell)
    (dolist (entry agent-shell-queue--subscriptions)
      (agent-shell-queue--teardown-shell (car entry)))
    (setq agent-shell-queue--entries nil
          agent-shell-queue--selected-id nil
          agent-shell-queue--subscriptions nil
          agent-shell-queue--dismissed nil)
    (agent-shell-queue--hide)))

(provide 'agent-shell-queue)
;;; agent-shell-queue.el ends here
