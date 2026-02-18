# Agent Shell Project Switching Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add project-aware agent-shell invocation with C-u prefix and a consult source for switching between active agent shells.

**Architecture:** Three new elisp functions added to the existing `agent-shell` use-package block in `lisp/ai.el`. The binding for `C-c g` changes from `agent-shell-toggle` to a custom wrapper that delegates to `agent-shell`.

**Tech Stack:** Emacs Lisp, agent-shell, consult, project.el

---

### Task 1: Add `mg-agent-shell-buffers` helper function

**Files:**
- Modify: `lisp/ai.el` (add function before `use-package agent-shell`)

**Step 1: Write the function**

Add before the `(use-package agent-shell ...)` form:

```elisp
(defun mg-agent-shell-buffers ()
  "Return shortened names of agent-shell buffers with active processes.
Extracts the project name portion from buffer names like
\"Claude Code Agent @ my-project\"."
  (let ((bufs (seq-filter
               (lambda (buf) (get-buffer-process buf))
               (agent-shell-buffers))))
    (mapcar (lambda (buf)
              (let ((name (buffer-name buf)))
                (if (string-match " Agent @ \\(.+\\)" name)
                    (match-string 1 name)
                  name)))
            bufs)))
```

**Step 2: Commit**

```bash
git add lisp/ai.el
git commit -m "feat: add mg-agent-shell-buffers helper"
```

### Task 2: Add `consult--source-agent-shell` consult source

**Files:**
- Modify: `lisp/ai.el` (add after `mg-agent-shell-buffers`)

**Step 1: Write the consult source**

Add after `mg-agent-shell-buffers`:

```elisp
(defun mg-agent-shell-lookup (name cands &rest _)
  "Look up agent-shell buffer by shortened NAME from CANDS.
Maps shortened display name back to the actual buffer."
  (seq-find (lambda (buf)
              (let ((bname (buffer-name buf)))
                (or (string= bname name)
                    (and (string-match " Agent @ \\(.+\\)" bname)
                         (string= (match-string 1 bname) name)))))
            (agent-shell-buffers)))

(defvar consult--source-agent-shell
  `(:name "Agent Shell"
    :narrow (?c . "Agent Shell")
    :category buffer
    :state ,#'consult--buffer-preview
    :action ,#'consult--buffer-action
    :lookup ,#'mg-agent-shell-lookup
    :items ,#'mg-agent-shell-buffers)
  "Agent shell candidates for `consult-buffer'.")
```

**Step 2: Commit**

```bash
git add lisp/ai.el
git commit -m "feat: add consult source for agent-shell buffers"
```

### Task 3: Add `mg-agent-shell` command

**Files:**
- Modify: `lisp/ai.el` (add after the consult source)

**Step 1: Write the command**

Add after the consult source definition:

```elisp
(defun mg-agent-shell (arg)
  "Open agent shell, optionally for a specific project.
Without prefix, open agent shell for current project.
With \\[universal-argument], prompt for project and open/reuse shell.
With \\[universal-argument] \\[universal-argument], prompt for project and create new shell."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (let ((default-directory (project-known-project-roots)))
      (agent-shell '(4))))
   ((equal arg '(4))
    (let ((default-directory (project-known-project-roots)))
      (agent-shell)))
   (t
    (agent-shell))))
```

Wait — `project-known-project-roots` returns a list, not a single directory. We need `completing-read` to pick one. Corrected:

```elisp
(defun mg-agent-shell (arg)
  "Open agent shell, optionally for a specific project.
Without prefix, open agent shell for current project.
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
   (t
    (agent-shell))))
```

**Step 2: Commit**

```bash
git add lisp/ai.el
git commit -m "feat: add mg-agent-shell with project-aware prefix arg"
```

### Task 4: Update use-package binding and register consult source

**Files:**
- Modify: `lisp/ai.el` (change `:bind` and add `add-to-list` in `:config`)

**Step 1: Change binding**

In the `use-package agent-shell` form, change:

```elisp
;; FROM:
:bind (("C-c g" . agent-shell-toggle)
       :map agent-shell-mode-map
       ("C-q" . bury-buffer))

;; TO:
:bind (("C-c g" . mg-agent-shell)
       :map agent-shell-mode-map
       ("C-q" . bury-buffer))
```

**Step 2: Register consult source**

Add to the `:config` section:

```elisp
(add-to-list 'consult-buffer-sources 'consult--source-agent-shell 'append)
```

**Step 3: Commit**

```bash
git add lisp/ai.el
git commit -m "feat: rebind C-c g to mg-agent-shell, register consult source"
```

### Task 5: Verify configuration loads

**Step 1: Check for syntax errors**

```bash
emacs --batch -l ~/.emacs.d/init.el --eval '(message "OK")' 2>&1
```

Expected: prints "OK" with no errors about ai.el

**Step 2: Commit final state if any fixes needed**

```bash
git add lisp/ai.el
git commit -m "fix: address ai.el load issues"
```
