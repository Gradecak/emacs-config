# Agent Shell Queue — Design Document

## Problem

When running multiple agent-shell sessions across different perspectives, each session may block waiting for user input (tool call permissions or next prompt). Currently you must switch to each agent-shell buffer to notice and respond. This breaks flow when working in a different perspective.

## Solution

A global minor mode that captures blocking events from all agent-shell buffers and surfaces them in a posframe overlay, allowing interaction without leaving your current context.

## Data Model

A single global queue (elisp list). Each entry is an alist:

```elisp
((:id . "uuid-string")
 (:type . permission)                ;; 'permission or 'prompt
 (:shell-buffer . #<buffer>)         ;; originating buffer
 (:timestamp . 1709750400)
 (:title . "grep -r 'pattern' ...")  ;; short description
 (:data . (...)))                    ;; type-specific payload
```

**Permission entries** — `:data` contains ACP `request-id`, `tool-call-id`, and tool description text.

**Prompt entries** — `:data` contains `:last-response` (agent's final message, truncated to configurable max chars) so the user has context without jumping to the buffer.

Items ordered by timestamp (oldest first). Any item can be selected regardless of position.

## Event Subscription & Lifecycle

### Hooking in

On `agent-shell-mode-hook`, subscribe to each new shell buffer. Two interception points:

- **Permission requests**: Advice on `agent-shell--on-request` (`:after`) to intercept incoming `session/request_permission` ACP requests and push a `permission` entry to the queue.
- **Agent idle**: Advice on the function that transitions to prompt-ready state (`:active-request` becomes nil after response is rendered) to push a `prompt` entry with the last response text captured from the buffer.

### Cleanup

- When an agent-shell buffer is killed: remove all queue entries for that buffer, unsubscribe.
- When a queue entry is acted upon:
  - **Permission**: call `agent-shell--send-permission-response` with stored request-id, remove entry.
  - **Prompt reply**: send text to shell buffer via shell-maker submit, remove entry.
- When queue becomes empty: auto-hide posframe.

## Posframe UI

### Position & Sizing

- Top-right of frame
- Configurable width (default 80 columns)
- Height determined by content

### Layout

```
[claude@project-a] Permission: grep -r 'pattern' src/   [y] [n]
[claude@project-b] Awaiting input                       [RET] [o]
  | I've finished refactoring the auth module. The tests
  | pass but I noticed the session timeout config is
  | hardcoded. Want me to make it configurable?
```

- Header line per entry: shell identity (agent @ project/perspective) + event description + action hints
- Prompt entries show last response preview indented below header, truncated to max lines with `...`
- Currently selected entry highlighted with a distinct face

### Keymap

| Key | Action |
|-----|--------|
| `↑`/`p` | Previous entry |
| `↓`/`n` | Next entry |
| `y` | Allow permission (permission entries only) |
| `n` | Reject permission (permission entries only, context-aware) |
| `RET` | Open minibuffer to type reply (prompt entries) |
| `o` | Jump to originating agent-shell buffer |
| `q` | Dismiss posframe (events stay queued, reappears on next new event) |

Navigation moves between entries, skipping over response preview lines.

### Behavior

- Auto-shows when first event pushed to empty queue
- Auto-hides when last event removed
- Does not steal focus
- Dismissing with `q` hides temporarily; reappears on next new event

## Module Structure

Single file: `lisp/agent-shell-queue.el`

### Public API

- `agent-shell-queue-mode` — global minor mode (enable/disable)
- `agent-shell-queue-show` — manually show posframe if events exist
- `agent-shell-queue-dismiss` — manually hide without clearing

### Customizable Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `agent-shell-queue-max-preview-lines` | 5 | Max lines of response preview |
| `agent-shell-queue-max-response-chars` | 500 | Max chars captured from last response |
| `agent-shell-queue-posframe-position` | top-right | Posframe position on frame |
| `agent-shell-queue-posframe-width` | 80 | Posframe width in columns |

### Dependencies

- `posframe` (new, via straight.el)
- `agent-shell` (existing)
