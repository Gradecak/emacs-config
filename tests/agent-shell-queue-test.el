;;; agent-shell-queue-test.el --- Tests for agent-shell-queue -*- lexical-binding: t; -*-

(require 'ert)

;; Provide stubs for dependencies that aren't available in batch mode
(unless (featurep 'agent-shell) (provide 'agent-shell))
(unless (featurep 'posframe) (provide 'posframe))

(require 'agent-shell-queue)

(ert-deftest agent-shell-queue-test-push-and-entries ()
  "Pushing an entry adds it to the queue."
  (let ((agent-shell-queue--entries nil)
        (agent-shell-queue--selected-id nil)
        (agent-shell-queue--id-counter 0))
    (agent-shell-queue--push
     :type 'permission
     :shell-buffer (current-buffer)
     :title "test tool call"
     :data '((:request-id . "req-1")))
    (should (= 1 (length agent-shell-queue--entries)))
    (should (eq 'permission (alist-get :type (car agent-shell-queue--entries))))))

(ert-deftest agent-shell-queue-test-push-ordering ()
  "Entries are ordered by timestamp (oldest first)."
  (let ((agent-shell-queue--entries nil)
        (agent-shell-queue--selected-id nil)
        (agent-shell-queue--id-counter 0))
    (agent-shell-queue--push :type 'permission :shell-buffer (current-buffer)
                             :title "first" :data nil)
    (agent-shell-queue--push :type 'prompt :shell-buffer (current-buffer)
                             :title "second" :data nil)
    (should (string= "first" (alist-get :title (car agent-shell-queue--entries))))
    (should (string= "second" (alist-get :title (cadr agent-shell-queue--entries))))))

(ert-deftest agent-shell-queue-test-remove ()
  "Removing an entry by id works."
  (let ((agent-shell-queue--entries nil)
        (agent-shell-queue--selected-id nil)
        (agent-shell-queue--id-counter 0))
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
        (agent-shell-queue--selected-id nil)
        (agent-shell-queue--id-counter 0)
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

;;; agent-shell-queue-test.el ends here
