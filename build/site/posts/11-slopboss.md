---
title: "SlopBoss: A Board-Driven Orchestrator for Multi-Agent Development"
tags: ["2026", "Go", "AI", "Tools"]
date: Oct 30, 2026
---

A few months ago I got tired of babysitting a single AI coding agent. The loop is always the same: prompt, wait, review, prompt again. Meanwhile the backlog keeps growing and the agents keep forgetting what they were doing. So I built **SlopBoss** — a small Go binary that treats a Markdown task board as the single source of truth and reconciles a fleet of coding agents against it, continuously, until the work is done.

The idea is borrowed from Kubernetes: you declare *desired state* (the board), and a control loop makes *observed state* (running sessions) converge to it. No database, no hidden state — everything is a file you can read, edit, and commit.

## The board

SlopBoss operates on a handful of Markdown files at the repo root:

- `BACKLOG.md` — prioritized backlog, groomed by a Team Lead agent.
- `TASKS.md` — one lane per Dev Agent with the task currently in flight.
- `ARCHIVE.md` — completed work history.
- `AGENTS.md`, `DEV_AGENT.md`, `TEAM_LEAD_AGENT.md` — role instructions shared with every agent.
- `TECH.md` — project tech context.
- `CONFIG.md` — repo, base branch, backend, agent count, per-role models.

A task is just a small Markdown block:

```markdown
### Add dark-mode toggle to settings
Owner: (unassigned)
Branch: agent/1/dark-mode-toggle
Status: Backlog

Add a theme toggle to the settings page that persists the choice
to localStorage and respects prefers-color-scheme on first load.
```

## How it works

Every ten seconds SlopBoss reads the board, diffs it against the sessions that are actually running, and starts or cancels agent processes until reality matches the board:

1. **Poll** — read the board files.
2. **Reconcile** — compare each task's assigned lane against running sessions.
3. **Converge** — start a session for any assigned-but-not-running task; cancel sessions whose task moved, changed, or already merged to `main`. Cancellation is debounced so a board rewrite at completion time doesn't kill live work.
4. **Repeat** — the TUI refreshes continuously; the loop runs until you stop it.

A task that merged to `main` but was never archived still counts as done, so dependent tasks never get stuck behind a cancelled session.

## Division of labor

- **Team Lead** — grooms the backlog, promotes the top item into a Dev Agent lane, runs as a one-off interactive session (`slopboss groom`).
- **Dev Agents** — N parallel agents, each in its own cloned workspace and branch, implementing, verifying, and archiving their assigned task.

## Backends

SlopBoss is backend-agnostic. Run on Codex (default), Claude Code, OpenCode, or Antigravity with a flag:

```bash
slopboss run --provider claude
```

Per-role models are configured in `CONFIG.md`:

```markdown
- Codex dev-agent model: gpt-6-luna
- Codex team-lead model: gpt-6-luna
```

## Quickstart

```bash
# 1. Provision the board, workspaces, and TECH.md
slopboss setup

# 2. Groom the backlog with the Team Lead
slopboss groom

# 3. Start the control loop with a live TUI
slopboss run
```

`run` keeps going until `Ctrl-C`, then cancels in-flight sessions and exits cleanly.

## Evals

You can A/B models, prompts, and backends on the same ticket. Each variant runs in an isolated git worktree, and SlopBoss collects token and diff metrics into `report.md`. You don't hand-write the config — the Team Lead designs it interactively via `slopboss eval groom`.

## Why files over state

The whole point is that there is no database and no shadow state. Restart SlopBoss at any time and it picks up exactly where it left off, because the board *is* the state. Hand edits and agent edits both take effect on the next poll. That property — convergence toward a declarative source of truth — is what made me reach for a Kubernetes-style reconcile loop instead of a fire-and-forget dispatcher.

Source: [github.com/de-angelov/slopboss](https://github.com/de-angelov/slopboss)
