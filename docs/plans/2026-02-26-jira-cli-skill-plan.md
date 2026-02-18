# Jira CLI Skill Implementation Plan

> **For Claude:** This is a reference skill (documentation). No code tests apply. Follow tasks sequentially.

**Goal:** Create a `jira-cli` reference skill that teaches Claude how to use the jira command-line tool.

**Architecture:** Two-file skill: lean SKILL.md (~200 words) always loaded with agent best practices and quick reference table; full command reference in jira-cli-reference.md (~800 words) loaded on demand.

**Location:** `~/.claude/skills/jira-cli/`

---

### Task 1: Create skill directory

**Step 1: Create directory**

Run: `mkdir -p ~/.claude/skills/jira-cli`

**Step 2: Verify**

Run: `ls -la ~/.claude/skills/jira-cli/`
Expected: empty directory exists

---

### Task 2: Write SKILL.md

**Files:**
- Create: `~/.claude/skills/jira-cli/SKILL.md`

**Step 1: Write SKILL.md**

Write the file with this exact content:

```markdown
---
name: jira-cli
description: Use when performing Jira operations - creating issues, managing sprints, querying boards, or any task involving the jira command-line tool
---

# Jira CLI Reference

Command reference for [jira-cli](https://github.com/ankitpokhrel/jira-cli). Assumes `jira` is pre-configured with project, board, and authentication.

## Agent Best Practices

- **Always** pass `--no-input` on mutation commands (`create`, `edit`, `clone`, `comment add`). Without it, the command will hang waiting for interactive input.
- **Always** use `--plain` on list commands for parseable table output. Default is interactive/explorer view.
- Use `--no-headers` when piping or parsing output programmatically.
- Use `--columns` to limit output to relevant fields. Available columns: TYPE, KEY, SUMMARY, STATUS, ASSIGNEE, REPORTER, PRIORITY, RESOLUTION, CREATED, UPDATED, LABELS.

## Quick Reference

| Task | Command |
|------|---------|
| List my issues | `jira issue list -a$(jira me) --plain` |
| Create issue | `jira issue create -tStory -s"Summary" --no-input` |
| View issue | `jira issue view ISSUE-1 --plain` |
| Edit issue | `jira issue edit ISSUE-1 -s"New summary" --no-input` |
| Assign issue | `jira issue assign ISSUE-1 "assignee"` |
| Move issue | `jira issue move ISSUE-1 "In Progress"` |
| Add comment | `jira issue comment add ISSUE-1 "text" --no-input` |
| List epics | `jira epic list --table --plain` |
| Current sprint | `jira sprint list --current --plain` |
| Open in browser | `jira open ISSUE-1` |

For full command details, flags, and examples see @jira-cli-reference.md in this skill directory.
```

**Step 2: Verify word count**

Run: `wc -w ~/.claude/skills/jira-cli/SKILL.md`
Expected: ~200 words (under 250)

---

### Task 3: Write jira-cli-reference.md

**Files:**
- Create: `~/.claude/skills/jira-cli/jira-cli-reference.md`

**Step 1: Write jira-cli-reference.md**

Write the file with the full command reference. Source material is in `/Users/mgradecak/.emacs.d/lisp/jira-claude.md` lines 30-151. Adapt by:
- Removing all `-p "$JIRA_PROJECT"` from examples (pre-configured)
- Removing env var references (`$JIRA_USERNAME`, `$JIRA_BOARD`)
- Removing server-specific details (`jira.drwholdings.com`)
- Removing issue type list (project-specific)
- Keeping all command groups: global/shared flags, issue, epic, sprint, board/project/release, utilities
- Each command: one-line description, flags, one example

**Step 2: Verify word count**

Run: `wc -w ~/.claude/skills/jira-cli/jira-cli-reference.md`
Expected: ~800 words (under 1000)

---

### Task 4: Verify skill structure

**Step 1: Check files exist**

Run: `ls -la ~/.claude/skills/jira-cli/`
Expected: SKILL.md and jira-cli-reference.md

**Step 2: Check frontmatter is valid**

Run: `head -4 ~/.claude/skills/jira-cli/SKILL.md`
Expected: YAML frontmatter with name and description fields

**Step 3: Commit**

```bash
git -C ~/.claude/skills add jira-cli/
git -C ~/.claude/skills commit -m "feat: add jira-cli reference skill"
```

Note: Only if ~/.claude/skills is a git repo. Otherwise just verify files are in place.
