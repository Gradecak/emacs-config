# Jira CLI Skill Design

## Overview

Reference skill for `jira-cli` (ankitpokhrel/jira-cli) to teach Claude how to interact with Jira via the `jira` command-line tool.

## Skill Identity

- **Name:** `jira-cli`
- **Type:** Reference
- **Location:** `~/.claude/skills/jira-cli/`
- **Description:** Use when performing Jira operations - creating issues, managing sprints, querying boards, or any task involving the jira command-line tool

## Key Decisions

- **Project-agnostic:** Assumes `jira` is pre-configured with project, board, and auth. No env vars or `-p` flags in the skill.
- **Non-interactive:** `--no-input` is a prominent rule for all mutation commands, since Claude cannot interact with prompts.
- **Token-efficient:** Lean SKILL.md (~200 words) always loaded; full reference in supporting file loaded on demand.

## File Structure

```
~/.claude/skills/jira-cli/
  SKILL.md                # ~200 words, always loaded
  jira-cli-reference.md   # ~800 words, loaded on demand
```

## SKILL.md Contents

### Frontmatter
```yaml
name: jira-cli
description: Use when performing Jira operations - creating issues, managing sprints, querying boards, or any task involving the jira command-line tool
```

### Sections
1. **Overview** — One sentence: reference for jira-cli, assumes pre-configured.
2. **Agent Best Practices** — Rules for non-interactive usage:
   - Always `--no-input` on mutations (create, edit, clone, comment add)
   - Always `--plain` for parseable output
   - `--no-headers` when piping/parsing
   - `--columns` to limit output noise
   - Available columns listed
3. **Quick Reference** — Table of ~10 most common operations with compact command examples.
4. **Footer** — Note to read `jira-cli-reference.md` for full details.

## jira-cli-reference.md Contents

Full command reference organized by group:

1. **Global & Shared Flags** — debug, help, list flags (plain, no-headers, columns, csv, raw, order-by, reverse, paginate, jql), mutation flags (no-input, web, template)
2. **Issue Commands** — list, create, view, edit, assign, move, delete, clone, comment add, link, unlink, watch, worklog add
3. **Epic Commands** — list, create, add, remove
4. **Sprint Commands** — list, add, close
5. **Board, Project, Release** — board list, project list, release list
6. **Utility Commands** — me, open, version, serverinfo

Each command: one-line description, flags, one example where useful.
