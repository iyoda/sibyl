# Draft: Session Persistence

## Requirements (confirmed)
- "終了時に会話履歴をファイルへ保存し、次回起動時に引数でセッションIDを指定することで前回の続きから作業を再開できる機能を実装する"
- Save conversation history to file on REPL exit
- Resume from previous session by specifying session ID on startup

## Research Findings

### Data Model (from explore agent 1)
- **memory class**: conversation (messages list), max-messages, summary, compaction-strategy
- **agent class**: name, client, memory, system-prompt, max-steps, step-count, hooks, token-tracker
- **message struct**: role, content, tool-calls, tool-call-id, timestamp, thinking, thinking-signature
- **tool-call class**: id, name, arguments (hash-table)
- **conversation struct**: messages (list), lock (bt:lock)

### Essential Fields for Persistence
- messages (role, content, timestamp, tool-calls, tool-call-id) — REQUIRED
- memory summary — OPTIONAL (can be regenerated)
- thinking/thinking-signature — NOT NEEDED (API-specific)

### Existing Serialization Patterns
- **S-expression**: plan->sexp / sexp->plan in src/plan/core.lisp
- **JSON**: yason for cost-log in src/repl.lisp
- **API format**: messages-to-anthropic-format in src/llm/providers.lisp

### REPL Architecture
- `start-repl` signature: `(&key client system-prompt name)` — need to add `:session-id`
- Command registry: `*repl-commands*` (string->keyword) + `*command-handlers*` (keyword->function)
- `/reset` clears: conversation messages + memory summary + step counter
- Exit flow: `exit-repl` closure → `return-from repl-loop` → `sb-ext:exit`
- Package exports: `sibyl:start-repl`, `sibyl.repl:handle-repl-command`

### Storage Convention
- `~/.sibyl/` is the existing convention for user data
  - `~/.sibyl/config.lisp` — config
  - `~/.sibyl/cost-log.json` — costs
  - `~/.sibyl/_history` — readline history
- Proposed: `~/.sibyl/sessions/<session-id>/`

## Technical Decisions (confirmed)
- **Session ID format**: タイムスタンプベース自動生成 (e.g., "session-20250219-143022")
- **Storage format**: S式 (plan->sexp パターンに合わせる)
- **Restore scope**: 会話履歴のみ (messages list only; system-prompt/model は起動時の設定を使う)
- **Save timing**: 数分毎に自動保存 + 終了時に保存
- **New commands**: /sessions (一覧) + /save (手動保存) + /load (セッション切り替え)
- **CLI invocation**: `(start-repl :client ... :session-id "session-20250219-143022")`

## Decisions (Round 2 — confirmed)
- **Auto-save interval**: 5分ごと
- **Default behavior (no session-id)**: 新規セッション自動生成。/quit 時に session-id を表示して再開方法を案内
- **/load behavior**: 現在のセッションを自動保存してから切替
- **CLI invocation**: `(start-repl :client ... :session-id "session-20250220-143022")`

## Metis Gap Resolutions (Round 3)
- **Memory summary**: summary も保存する (messages + summary で完全復元)
- **/reset behavior**: 現在のセッションを自動保存してからクリア (同じ session-id 内)
- **tool-call serialization**: CLOS → plist converter を実装 (自動解決)
- **Timer lifecycle**: spinner.lisp の stop-flag パターンを流用 (自動解決)
- **Concurrent save**: lock で保護 (自動解決)
- **System messages**: :system role を除外して保存 (自動解決)
- **Auto-save interval**: configurable (:auto-save-interval keyword, default 300 seconds)

## Test Strategy Decision
- **Infrastructure exists**: YES (FiveAM, tests/rich-repl-test.lisp, tests/suite.lisp)
- **Automated tests**: YES (tests-after)
- **Framework**: FiveAM
- **Agent-Executed QA**: ALWAYS

## Open Questions
(None — all requirements clarified)

## Scope Boundaries
- INCLUDE: session save/load, auto-save timer, /sessions /save /load commands, start-repl :session-id param
- EXCLUDE: session sharing between different models, session export/import, session search
