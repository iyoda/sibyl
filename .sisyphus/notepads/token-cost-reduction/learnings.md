# Learnings: token-cost-reduction

## 2026-02-18 — Session Start

### Key Codebase Facts
- `parse-anthropic-response` (providers.lisp:120-144): currently extracts only `content` blocks, ignores `usage` field
- `parse-anthropic-sse-events` (providers.lisp:146-171): ignores `message_start`/`message_delta` events (where usage lives in streaming)
- `complete-anthropic-streaming` (providers.lisp:173-245): returns only message struct, no usage
- `agent-step` (agent/core.lisp:139-183): calls `complete-with-tools` at line 153-156, no multiple-value-bind
- `memory-context-window` (memory.lisp:43-59): concatenates system prompt + summary with `format nil "~a~%~%## Previous conversation summary:~%~a"` — this DESTROYS cache hits
- `tools-as-schema` (protocol.lisp:116-118): returns ALL registered tools every time, no filtering
- `model-selector.lisp`: Light/Medium/Heavy tiers defined BUT NOT connected to agent loop
- `*default-system-prompt*`: ~2000 tokens, text validated by 6 tests in agent-test.lisp — DO NOT CHANGE TEXT

### Guardrails (CRITICAL)
1. DO NOT change `*default-system-prompt*` text content (6 agent-test.lisp tests validate specific strings)
2. DO NOT change `complete`/`complete-with-tools` argument signatures
3. DO NOT add caching to OpenAI provider (Anthropic only)
4. DO NOT make real API calls in tests
5. `start-repl` signature: keyword args only, no positional changes
6. Tool result compression: OUT OF SCOPE (size tracking only)

### Architecture Decision: Usage Data Flow
- Use `(values message usage-plist)` pattern — first value stays `message` for backward compat
- Callers that don't care use single-value binding, they get `message` as before
- `agent-step` uses `multiple-value-bind` to capture usage and accumulate in tracker

### Test Infrastructure
- Framework: FiveAM
- Test command: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`
- sibyl.asd lists test files: must update when adding new test files

## [2026-02-18] Task 1: Token Usage Tracking Foundation — COMPLETED

### What Was Implemented
- `src/llm/token-tracker.lisp`: `token-tracker` struct (5 integer slots) + `tracker-add-usage` (nil-safe) + `tracker-cache-hit-rate`
- `parse-anthropic-response` → now returns `(values message usage-plist)`. Keys: `:input-tokens`, `:output-tokens`, `:cache-read-tokens`, `:cache-write-tokens`
- `parse-anthropic-sse-events` → handles `message_start` (input) and `message_delta` (output) SSE events
- `complete-anthropic-streaming` → collects token counts from SSE events, returns `(values message usage-plist)`
- `agent` class → new `token-tracker` slot (initform fresh tracker per agent), accessor `agent-token-tracker`
- `agent-step` → uses `multiple-value-bind` to capture `(response usage)`, calls `tracker-add-usage`

### Key Findings
- Streaming cache tokens are NOT available in SSE events — streaming path sets cache tokens to 0 (limitation)
- `complete-anthropic-streaming` needs 6 closing parens at function end: defun + mvb + outer-let* + labels + inner-let* + values
- Large edit operations can accidentally delete code after the matched region — always grep for expected definitions after editing
- `sibyl.asd` `:serial t` means `token-tracker.lisp` must precede `providers.lisp` in llm module
- OpenAI methods NOT modified — they return single value (no usage plist) — forward compat maintained

### Results
- 1497 checks, 1497 pass, 0 failures
- `input=150 output=50 content=hello` ✓

## [2026-02-18] Task 7: Extended Thinking Content Block Parsing — COMPLETED

### What Was Implemented
- `src/llm/message.lisp`: `message` defstruct gets `(thinking nil :type (or string null))` slot; `make-message` and `assistant-message` accept `&key thinking`
- `src/packages.lisp`: `#:message-thinking` exported from `sibyl.llm` package
- `src/llm/providers.lisp` `parse-anthropic-response`: added `thinking-parts nil` to let*, added `"thinking"` block case (reads `(gethash "thinking" block)`), passes `:thinking` to `assistant-message`
- `src/llm/providers.lisp` `parse-anthropic-sse-events`: `content_block_delta` now includes `:thinking (gethash "thinking" delta)` in plist
- `src/llm/providers.lisp` `complete-anthropic-streaming`: added `thinking-parts nil`, handles `thinking_delta` delta-type, handles `"thinking"` in `content_block_start`, passes `:thinking` to `assistant-message`
- `tests/token-tracking-test.lisp`: Tests 7 and 8 added (thinking block parsing, nil when absent)

### Key Findings
- `defstruct` auto-generates accessor `message-thinking` — must be exported from package manually (not auto-exported)
- SBCL caches FASLs; struct redefinition causes fatal error if old FASL loaded — clear `~/.cache/common-lisp/*sibyl*` when changing struct layout
- `content_block_start` for `"thinking"` type needs no special state setup (unlike `tool_use` which sets current-tool-id/name)
- `thinking_delta` field name is `"thinking"` (not `"text"`) — same name as the block type

### Results
- 1501 checks, 1501 pass, 0 failures (4 new checks from 2 new tests)

## [2026-02-18] Task 2: System Prompt Static/Dynamic Split — COMPLETED

### What Was Implemented
- `src/llm/message.lisp`: `message` defstruct `content` slot type extended to `(or string list null)`. `system-message` docstring clarified to accept string OR list of content-block alists.
- `src/agent/memory.lisp` `memory-context-window`: replaced string-concat with content-block list:
  - No summary → `(list '(("type" . "text") ("text" . ,system-prompt)))` — 1 block
  - With summary → 2 blocks: static prompt + dynamic summary block (separate, so static stays cache-stable)
- `src/llm/providers.lisp` `messages-to-anthropic-format`: added doc comment explaining that `system` can be string or list; code unchanged (already passes through `message-content` directly).
- `tests/token-tracking-test.lisp`: Tests 9 and 10 added (single-block/no-summary, two-blocks/with-summary).

### Key Findings
- `to-json-value` in client.lisp handles nested alists correctly: a list of alists → JSON array of objects. No changes needed to serialization path.
- The Anthropic API accepts `"system"` as either `"string"` or `[{"type":"text","text":"..."},...]` — confirmed by Anthropic docs.
- All three Anthropic code paths (streaming append, non-streaming push, complete-with-tools) correctly propagate the system value via `alist-to-hash` → `to-json-value` → yason.
- OpenAI `messages-to-openai-format` also passes `message-content` directly as the `"content"` value; OpenAI accepts content arrays too — no breakage.
- SBCL FASL cache must be cleared when changing struct slot types, but in this case the `content` type was only a `list` addition so no runtime incompatibility occurred.

### Results
- 1510 checks, 1510 pass, 0 failures (9 new checks from 2 new tests)

## [2026-02-18] Task 3: Anthropic Prompt Caching

### What was implemented
- `config-set` added to `sibyl.config` as a convenience wrapper around `(setf config-value)`.
  Exported from both `sibyl.config` and `sibyl` packages.
- `"optimization.cache-enabled"` default set to `t` in `set-defaults`.
- `tools-to-anthropic-format` now appends `cache_control ephemeral` to the LAST tool's alist
  when caching is enabled — this caches the entire tools prefix in Anthropic's KV cache.
- `add-cache-control-to-system` helper: marks the FIRST system content block (static prompt)
  with `cache_control ephemeral`. Second block (dynamic summary) is intentionally NOT marked.
- Helper applied in all 3 Anthropic API paths: `complete-anthropic-streaming`,
  `complete` (non-streaming), `complete-with-tools` (non-streaming).

### Key design decisions
- Only the LAST tool gets `cache_control` — Anthropic caches everything UP TO a marked point.
- Only the FIRST system block (static prompt) is marked — the second block (summary) changes
  every compaction so marking it would waste cache writes.
- Cache invalidation hierarchy: tools → system → messages. If tools change, tools cache and
  system cache both invalidate.
- `"optimization.cache-enabled"` defaults to `t`; test cleanup sets it to `nil` to avoid
  polluting sibling tests.

### Package structure note
- `sibyl.config` uses `(setf config-value)` as the setter form; `config-set` is the new
  plain-function alias used in tests.
- `sibyl.llm` uses `config-value` directly because it `:use`s `#:sibyl.config`.

### Test results
- 1514 checks, 0 failures after implementation (tests 11, 12, 13 pass).
- Tests use `unwind-protect` to restore `optimization.cache-enabled` to nil after each test
  to prevent caching state leaking into other tests.

## [2026-02-18] Task 4: Category-Based Tool Filtering — COMPLETED

### What Was Implemented
- `src/tools/protocol.lisp`:
  - `tool` defstruct: added `(category :general :type keyword)` slot
  - `deftool` macro: added `(category :general)` to `&key` destructuring; passes `:category` to `make-tool`
  - `list-tools`: added `&key categories` parameter; filters via `remove-if-not` + `member` when provided
  - `tools-as-schema`: added `&key categories` parameter; delegates to `list-tools :categories categories`
- `src/packages.lisp`: exported `#:tool-category` from `sibyl.tools`
- `src/tools/builtin.lisp`: all 13 tools categorized (`:file` → read-file, write-file, list-directory, file-info; `:analysis` → grep, analyze-task-complexity; `:general` → shell + 7 agent tools)
- `src/tools/lisp-tools.lisp`: all 10 tools get `:code` category
- `src/tools/analysis-tools.lisp`: all 5 tools get `:analysis` category
- `src/tools/refactor-tools.lisp`: both tools get `:code` category
- `src/mcp/tools.lisp`: `make-tool` call gets `:category :external` for all MCP-registered tools
- `src/agent/core.lisp`:
  - Added `infer-tool-categories` function: returns `'(:general)` + adds `:file`/`:code`/`:analysis` based on regex patterns
  - `agent-step`: computes `inferred-cats` from `user-input`; uses `tools-as-schema :categories inferred-cats` when user-input is non-nil, else all tools

### Key Design Decisions
- `:general` is the default category (no `deftool` breakage for uncategorized tools)
- `tools-as-schema` without `:categories` returns ALL tools (full backward compat)
- Filtering is in `list-tools` so both `list-tools` and `tools-as-schema` share the logic
- Heuristic applies ONLY on first agent-step call (user-input non-nil); recursive calls (user-input nil) get all tools
- planning-tools, evolution-tools default to `:general` (no explicit annotation needed)
- self-tools also left at default `:general` (they're meta-tools rarely needed per-turn)

### Tool Distribution (43 total)
- `:file` → 4 tools
- `:code` → 12 tools
- `:analysis` → 7 tools
- `:general` → 20 tools (including default-category tools)

### Test Results
- 1534 checks, 0 failures (14 new checks from 4 new category tests)
