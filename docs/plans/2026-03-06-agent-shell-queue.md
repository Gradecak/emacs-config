# Agent Shell Queue Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** A global minor mode that captures blocking events from all agent-shell buffers and surfaces them in a posframe overlay for cross-perspective interaction.

**Architecture:** Advice-based interception of `agent-shell--on-request` (permissions) and `agent-shell--send-request` callbacks (idle detection). Events are pushed to a global queue and rendered in a `*agent-shell-queue*` buffer displayed via posframe. User actions dispatch responses back to the originating shell buffer via existing agent-shell APIs.

**Tech Stack:** Emacs Lisp, posframe, agent-shell (ACP protocol)

---

### Task 1: Package skeleton and customizable variables

**Files:**
- Create: `lisp/agent-shell-queue.el`

**Step 1: Create the package file with header, requires, defgroup, and defcustom variables**

```elisp
;;; agent-shell-queue.el --- Notification queue for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Surfaces blocking events from all agent-shell buffers in a posframe overlay.

;;; Code:

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

(provide 'agent-shell-queue)
;;; agent-shell-queue.el ends here
```

**Step 2: Verify the file loads without error**

Run: `emacs -Q -batch -l /Users/mgradecak/.emacs.d/lisp/agent-shell-queue.el` (will fail on requires — that's expected in batch mode, but the syntax should be clean)

**Step 3: Commit**

```bash
git add lisp/agent-shell-queue.el
git commit -m "feat(agent-shell-queue): package skeleton with defcustom variables"
```

---

### Task 2: Queue data model and management functions

**Files:**
- Modify: `lisp/agent-shell-queue.el`

**Step 1: Write ERT tests for queue operations**

Create test file:

**Files:**
- Create: `tests/agent-shell-queue-test.el`

```elisp
;;; agent-shell-queue-test.el --- Tests for agent-shell-queue -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-queue)

(ert-deftest agent-shell-queue-test-push-and-entries ()
  "Pushing an entry adds it to the queue."
  (let ((agent-shell-queue--entries nil))
    (agent-shell-queue--push
     :type 'permission
     :shell-buffer (current-buffer)
     :title "test tool call"
     :data '((:request-id . "req-1")))
    (should (= 1 (length agent-shell-queue--entries)))
    (should (eq 'permission (alist-get :type (car agent-shell-queue--entries))))))

(ert-deftest agent-shell-queue-test-push-ordering ()
  "Entries are ordered by timestamp (oldest first)."
  (let ((agent-shell-queue--entries nil))
    (agent-shell-queue--push :type 'permission :shell-buffer (current-buffer)
                             :title "first" :data nil)
    (agent-shell-queue--push :type 'prompt :shell-buffer (current-buffer)
                             :title "second" :data nil)
    (should (string= "first" (alist-get :title (car agent-shell-queue--entries))))
    (should (string= "second" (alist-get :title (cadr agent-shell-queue--entries))))))

(ert-deftest agent-shell-queue-test-remove ()
  "Removing an entry by id works."
  (let ((agent-shell-queue--entries nil))
    (agent-shell-queue--push :type 'permission :shell-buffer (current-buffer)
                             :title "keep" :data nil)
    (agent-shell-queue--push :type 'prompt :shell-buffer (current-buffer)
                             :title "remove" :data nil)
    (let ((id (alist-get :id (cadr agent-shell-queue--entries))))
      (agent-shell-queue--remove id)
      (should (= 1 (length agent-shell-queue--entries)))
      (should (string= "keep" (alist-get :title (car agent-shell-queue--entries)))))))

(ert-deftest agent-shell-queue-test-clear-for-buffer ()
  "Clearing entries for a specific buffer removes only those."
  (let ((agent-shell-queue--entries nil)
        (buf-a (generate-new-buffer "test-a"))
        (buf-b (generate-new-buffer "test-b")))
    (unwind-protect
        (progn
          (agent-shell-queue--push :type 'permission :shell-buffer buf-a
                                   :title "from-a" :data nil)
          (agent-shell-queue--push :type 'prompt :shell-buffer buf-b
                                   :title "from-b" :data nil)
          (agent-shell-queue--clear-for-buffer buf-a)
          (should (= 1 (length agent-shell-queue--entries)))
          (should (eq buf-b (alist-get :shell-buffer (car agent-shell-queue--entries)))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))
```

**Step 2: Run tests to verify they fail**

Run: `emacs -Q -batch -l ert -l lisp/agent-shell-queue.el -l tests/agent-shell-queue-test.el -f ert-run-tests-batch-and-exit`
Expected: FAIL — functions not defined yet

**Step 3: Implement queue management functions**

Add to `lisp/agent-shell-queue.el` before the `(provide)` line:

```elisp
(defvar agent-shell-queue--id-counter 0
  "Counter for generating unique entry IDs.")

(cl-defun agent-shell-queue--push (&key type shell-buffer title data)
  "Push a new entry to the queue.
TYPE is `permission' or `prompt'.
SHELL-BUFFER is the originating agent-shell buffer.
TITLE is a short description string.
DATA is a type-specific alist."
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

(defun agent-shell-queue--refresh ()
  "Stub for UI refresh — implemented in Task 4."
  nil)
```

**Step 4: Run tests to verify they pass**

Run: `emacs -Q -batch -l ert -l lisp/agent-shell-queue.el -l tests/agent-shell-queue-test.el -f ert-run-tests-batch-and-exit`
Expected: All 4 tests PASS

**Step 5: Commit**

```bash
git add lisp/agent-shell-queue.el tests/agent-shell-queue-test.el
git commit -m "feat(agent-shell-queue): queue data model with push/remove/clear"
```

---

### Task 3: Posframe rendering

**Files:**
- Modify: `lisp/agent-shell-queue.el`

**Step 1: Write a test for entry formatting**

Add to `tests/agent-shell-queue-test.el`:

```elisp
(ert-deftest agent-shell-queue-test-format-permission-entry ()
  "Permission entry renders with shell identity and title."
  (let* ((buf (generate-new-buffer "test-shell"))
         (entry (list (cons :id "asq-99")
                      (cons :type 'permission)
                      (cons :shell-buffer buf)
                      (cons :timestamp (float-time))
                      (cons :title "grep -r 'foo' src/")
                      (cons :data nil))))
    (unwind-protect
        (let ((text (agent-shell-queue--format-entry entry nil)))
          (should (string-match-p "Permission" text))
          (should (string-match-p "grep" text)))
      (kill-buffer buf))))

(ert-deftest agent-shell-queue-test-format-prompt-entry-with-preview ()
  "Prompt entry renders with response preview."
  (let* ((buf (generate-new-buffer "test-shell"))
         (entry (list (cons :id "asq-100")
                      (cons :type 'prompt)
                      (cons :shell-buffer buf)
                      (cons :timestamp (float-time))
                      (cons :title "Awaiting input")
                      (cons :data (list (cons :last-response "I finished the refactoring."))))))
    (unwind-protect
        (let ((text (agent-shell-queue--format-entry entry nil)))
          (should (string-match-p "Awaiting input" text))
          (should (string-match-p "I finished the refactoring" text)))
      (kill-buffer buf))))
```

**Step 2: Run tests to verify they fail**

Expected: FAIL — `agent-shell-queue--format-entry` not defined

**Step 3: Implement rendering functions**

Add to `lisp/agent-shell-queue.el`:

```elisp
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
                    (format "%s Permission: %s  [y] [n]" label title))
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
        (use-local-map agent-shell-queue-mode-map)))
    buf))
```

Now replace the `agent-shell-queue--refresh` stub:

```elisp
(defun agent-shell-queue--refresh ()
  "Re-render the queue buffer and show/hide the posframe."
  (if agent-shell-queue--entries
      (progn
        (agent-shell-queue--render)
        (unless agent-shell-queue--dismissed
          (agent-shell-queue-show)))
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
  (posframe-hide agent-shell-queue--buffer-name))

(defun agent-shell-queue-dismiss ()
  "Dismiss the posframe without clearing the queue."
  (interactive)
  (setq agent-shell-queue--dismissed t)
  (agent-shell-queue--hide))
```

**Step 4: Run tests to verify they pass**

Run: `emacs -Q -batch -l ert -l lisp/agent-shell-queue.el -l tests/agent-shell-queue-test.el -f ert-run-tests-batch-and-exit`
Expected: All tests PASS (posframe calls are only in refresh/show which aren't called during tests since we use the stub)

Note: The format tests will require the rendering functions but NOT posframe. The `--refresh` stub is replaced, but push/remove tests don't trigger posframe because posframe isn't loaded in batch mode. Tests that call `--push` directly will call `--refresh` which calls `posframe-show` — so we need to handle this. Update the `--refresh` function to guard against missing posframe in batch:

```elisp
(defun agent-shell-queue--refresh ()
  "Re-render the queue buffer and show/hide the posframe."
  (when (fboundp 'posframe-show)
    (if agent-shell-queue--entries
        (progn
          (agent-shell-queue--render)
          (unless agent-shell-queue--dismissed
            (agent-shell-queue-show)))
      (agent-shell-queue--hide))))
```

**Step 5: Commit**

```bash
git add lisp/agent-shell-queue.el tests/agent-shell-queue-test.el
git commit -m "feat(agent-shell-queue): posframe rendering and entry formatting"
```

---

### Task 4: Keymap and interaction actions

**Files:**
- Modify: `lisp/agent-shell-queue.el`

**Step 1: Implement the keymap and navigation**

Add to `lisp/agent-shell-queue.el`:

```elisp
(defvar agent-shell-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'agent-shell-queue-prev)
    (define-key map (kbd "<down>") #'agent-shell-queue-next)
    (define-key map (kbd "p") #'agent-shell-queue-prev)
    (define-key map (kbd "n") #'agent-shell-queue-next)
    (define-key map (kbd "y") #'agent-shell-queue-allow)
    (define-key map (kbd "RET") #'agent-shell-queue-reply)
    (define-key map (kbd "o") #'agent-shell-queue-jump)
    (define-key map (kbd "q") #'agent-shell-queue-dismiss)
    map)
  "Keymap for the agent-shell-queue posframe buffer.")
```

Note: `n` is used for both navigation AND reject. These conflict. Resolution: use `n`/`p` for navigation, `y` for allow, `C-n` or `d` for reject (deny). Update the design accordingly:

```elisp
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
```

**Step 2: Implement navigation commands**

```elisp
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
```

**Step 3: Implement action commands**

```elisp
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
```

**Step 4: Run tests to verify nothing broke**

Run: `emacs -Q -batch -l ert -l lisp/agent-shell-queue.el -l tests/agent-shell-queue-test.el -f ert-run-tests-batch-and-exit`
Expected: All tests PASS

**Step 5: Commit**

```bash
git add lisp/agent-shell-queue.el tests/agent-shell-queue-test.el
git commit -m "feat(agent-shell-queue): keymap, navigation, and action commands"
```

---

### Task 5: Event interception via advice

**Files:**
- Modify: `lisp/agent-shell-queue.el`

**Step 1: Implement the permission request interceptor**

This advises `agent-shell--on-request` to capture `session/request_permission` requests before the UI renders them. Reference: `agent-shell.el:1444`.

```elisp
(defun agent-shell-queue--intercept-request (orig-fn &rest args)
  "Advice around `agent-shell--on-request' to capture permission requests.
Calls ORIG-FN with ARGS, then pushes a queue entry if it was a permission request."
  ;; Let the original function run first (renders the UI in the shell buffer)
  (apply orig-fn args)
  ;; Now check if this was a permission request and push to queue
  (let* ((kwargs (car args))  ;; cl-defun uses &key, need to extract
         (acp-request (plist-get args :acp-request)))
    ;; The function uses cl-defun with &key, so args come as plist
    ;; We need to extract from the keyword args
    (when-let* ((acp-request (cl-getf (flatten-list args) :acp-request))
                ((equal (map-elt acp-request 'method) "session/request_permission"))
                (state (cl-getf (flatten-list args) :state))
                (tool-call-id (map-nested-elt acp-request '(params toolCall toolCallId)))
                (title (or (map-nested-elt acp-request '(params toolCall title))
                           (map-nested-elt acp-request '(params toolCall toolCallId))))
                (request-id (map-elt acp-request 'id)))
      (agent-shell-queue--push
       :type 'permission
       :shell-buffer (map-elt state :buffer)
       :title title
       :data (list (cons :request-id request-id)
                   (cons :tool-call-id tool-call-id)
                   (cons :description (map-nested-elt acp-request '(params toolCall title))))))))
```

Note: `cl-defun` with `&key` compiles to different argument passing than regular `defun`. The advice needs to handle keyword argument extraction properly. The actual arguments passed will be a plist. Study how `agent-shell--on-request` is called at `agent-shell.el:3795` to determine the exact call convention:

```elisp
(acp-subscribe-to-requests
 :client client
 :on-request (lambda (acp-request)
               (agent-shell--on-request
                :state (agent-shell--state)
                :acp-request acp-request)))
```

So the function is called as `(agent-shell--on-request :state STATE :acp-request REQUEST)`. The advice will receive these as a plist in `args`.

Corrected version:

```elisp
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
```

**Step 2: Implement idle detection**

When a request completes, `:active-request` is set to nil in the `on-success` and `on-failure` callbacks inside `agent-shell--send-request` (`agent-shell.el:3214`). We advise `agent-shell--send-request` to wrap those callbacks with our idle detector.

However, advising the callbacks inside `agent-shell--send-request` is fragile. A simpler approach: use a periodic check, OR advise `map-put!` — also fragile. Better approach: advise `agent-shell--send-request` `:after` and watch for the request type. When the request is a `session/message` (user prompt), its completion means the agent is idle.

Simplest reliable approach: advise `agent-shell--send-request` with `:around` to wrap the `on-success` callback:

```elisp
(defun agent-shell-queue--advice-send-request (orig-fn &rest args)
  "Around-advice on `agent-shell--send-request' to detect idle state."
  (let* ((orig-on-success (plist-get args :on-success))
         (state (plist-get args :state))
         (request (plist-get args :request))
         (method (and request (map-elt request 'method)))
         (wrapped-on-success
          (lambda (acp-response)
            (when orig-on-success
              (funcall orig-on-success acp-response))
            ;; After a session/message completes, agent is idle
            (when (and (equal method "session/message") state)
              (agent-shell-queue--on-agent-idle state)))))
    (apply orig-fn (plist-put (copy-sequence args) :on-success wrapped-on-success))))
```

Note: `plist-put` modifies in place — use `copy-sequence` to avoid corrupting the original args list. Actually, `plist-put` on a list copy is fine. But `copy-sequence` only does a shallow copy of the top-level list. Since plists are flat lists, this works.

```elisp
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
           ;; Search backward for the shell prompt to find response boundary
           (start (or (save-excursion
                        (when (re-search-backward shell-maker-prompt-regexp nil t 1)
                          (match-end 0)))
                      (point-min)))
           (text (string-trim (buffer-substring-no-properties start end))))
      (if (> (length text) agent-shell-queue-max-response-chars)
          (substring text 0 agent-shell-queue-max-response-chars)
        text))))
```

**Step 3: Implement cleanup on buffer kill**

```elisp
(defun agent-shell-queue--on-shell-killed ()
  "Hook for `kill-buffer-hook' in agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-queue--clear-for-buffer (current-buffer))
    ;; Remove subscription tracking
    (setq agent-shell-queue--subscriptions
          (assq-delete-all (current-buffer) agent-shell-queue--subscriptions))))
```

**Step 4: Also remove the queue entry when a permission is answered directly in the shell buffer**

If the user answers a permission directly in the agent-shell buffer (not via the queue), we should remove the corresponding queue entry. Subscribe to `permission-response` events:

```elisp
(defun agent-shell-queue--on-permission-response (event)
  "Remove queue entry when permission is answered directly in shell buffer."
  (when-let* ((data (alist-get :data event))
              (request-id (alist-get :request-id data)))
    ;; Find and remove any queue entry with this request-id
    (when-let ((entry (seq-find
                       (lambda (e)
                         (string= request-id
                                  (alist-get :request-id (alist-get :data e))))
                       agent-shell-queue--entries)))
      (agent-shell-queue--remove (alist-get :id entry)))))
```

**Step 5: Commit**

```bash
git add lisp/agent-shell-queue.el
git commit -m "feat(agent-shell-queue): event interception via advice and subscriptions"
```

---

### Task 6: Global minor mode and integration

**Files:**
- Modify: `lisp/agent-shell-queue.el`
- Modify: `lisp/ai.el`

**Step 1: Implement the global minor mode**

```elisp
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
```

**Step 2: Add to ai.el configuration**

Add to `lisp/ai.el` inside the `(use-package agent-shell ...)` `:config` block, or as a separate use-package form after it:

```elisp
(use-package agent-shell-queue
  :after agent-shell
  :straight nil
  :config
  (agent-shell-queue-mode 1))
```

**Step 3: Verify everything loads**

Start Emacs, open an agent-shell with `C-c g`. Verify:
- `agent-shell-queue-mode` is active (check with `M-x agent-shell-queue-mode RET`)
- Mode line shows " ASQ"
- No errors in `*Messages*`

**Step 4: Manual integration test**

1. Open agent-shell with `C-c g`
2. Send a prompt that triggers a tool call permission
3. Verify the posframe appears with the permission entry
4. Press `y` in the posframe to allow
5. Verify the entry is removed and posframe hides
6. Wait for agent to finish, verify "Awaiting input" entry appears with response preview
7. Press `RET`, type a reply, verify it's sent to the shell buffer

**Step 5: Commit**

```bash
git add lisp/agent-shell-queue.el lisp/ai.el
git commit -m "feat(agent-shell-queue): global minor mode and ai.el integration"
```

---

### Task 7: Edge cases and polish

**Files:**
- Modify: `lisp/agent-shell-queue.el`

**Step 1: Handle dead buffers in the queue**

When rendering, skip entries whose shell buffer is dead:

In `agent-shell-queue--refresh`, before rendering, prune dead entries:

```elisp
(defun agent-shell-queue--prune-dead ()
  "Remove entries whose shell buffer no longer exists."
  (setq agent-shell-queue--entries
        (seq-filter (lambda (entry)
                      (buffer-live-p (alist-get :shell-buffer entry)))
                    agent-shell-queue--entries)))
```

Call `agent-shell-queue--prune-dead` at the start of `agent-shell-queue--refresh`.

**Step 2: Prevent duplicate permission entries**

If the same `request-id` is already in the queue, don't push again:

Add to `agent-shell-queue--push`:

```elisp
;; Before appending, check for duplicate request-id (permission entries)
(when (and (eq type 'permission)
           (alist-get :request-id data))
  (when (seq-find (lambda (e)
                    (string= (alist-get :request-id (alist-get :data e))
                             (alist-get :request-id data)))
                  agent-shell-queue--entries)
    (cl-return-from agent-shell-queue--push nil)))
```

Requires adding `cl-block` to the function, or restructuring with an early return via `when-let`.

**Step 3: Prevent duplicate idle entries**

Don't push a `prompt` entry if one already exists for the same shell buffer:

```elisp
(when (eq type 'prompt)
  (when (seq-find (lambda (e)
                    (and (eq 'prompt (alist-get :type e))
                         (eq shell-buffer (alist-get :shell-buffer e))))
                  agent-shell-queue--entries)
    (cl-return-from agent-shell-queue--push nil)))
```

**Step 4: Run all tests**

Run: `emacs -Q -batch -l ert -l lisp/agent-shell-queue.el -l tests/agent-shell-queue-test.el -f ert-run-tests-batch-and-exit`
Expected: All tests PASS

**Step 5: Commit**

```bash
git add lisp/agent-shell-queue.el
git commit -m "fix(agent-shell-queue): handle dead buffers and prevent duplicates"
```
