# Repository Guidelines

## Project Structure & Module Organization
- `src/` holds the Common Lisp implementation, organized by domain: `llm/`, `tools/`, `agent/`, `plan/`, `mcp/`, `cache/`, `repl/`, and `system/`.
- `scripts/` contains helper scripts (e.g., REPL launcher and git-hook installers/checkers).
- `tests/` contains FiveAM test suites (e.g., `tests/suite.lisp`).
- `sibyl.asd` defines the ASDF system; keep it updated when adding files.
- `docs/` hosts design notes (see `docs/design-vision-ja.md`).

## Build, Test, and Development Commands
- Load the system:
  ```bash
  sbcl --eval '(ql:quickload :sibyl :silent t)'
  ```
- Start the REPL (Anthropic example):
  ```bash
  sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (sibyl:start-repl :client (sibyl:make-anthropic-client)))'
  ```
- Start the REPL via helper script (model-selectable):
  ```bash
  scripts/start-repl.sh gpt-5.2-codex
  ```
- Run tests:
  ```lisp
  (ql:quickload :sibyl/tests)
  (asdf:test-system :sibyl)
  ```

## Coding Style & Naming Conventions
- Indentation follows standard Common Lisp conventions (2 spaces, align with macros/forms).
- File naming is kebab-case under `src/` (e.g., `model-selector.lisp`).
- Tool names use kebab-case and are defined via `sibyl.tools:deftool` in `src/tools/`.

## Testing Guidelines
- Framework: FiveAM (see `tests/suite.lisp`).
- Add tests alongside the feature area (e.g., `tests/tools-test.lisp` for tool changes).
- Keep test names descriptive and scoped to behavior; ensure new modules are loaded by the test system.

### Suite Classification (MANDATORY)
- **Always define a per-file test suite and add it to `*safe-suites*` or `*unsafe-suites*` in `tests/suite.lisp`.**
- The `run-tests-parallel` validator will warn about unclassified suites.
- Classification criteria:
  - **SAFE**: pure logic, no file I/O, no global state mutation, no external processes.
  - **UNSAFE**: file I/O, global registries/state, random timing, network, or tool execution.

### Test Speed & Performance
- Avoid unnecessary `sleep` calls — use them only when testing timing-sensitive behavior.
- If a test doesn't need I/O, classify it as SAFE to enable parallel execution.
- **Always mock external APIs** — never make real network calls in tests.
- Test execution times are recorded in `tests/timing-history.json`; significant regressions trigger warnings.

### Mock Patterns
Use these established patterns for mocking:
1. **CLOS override**: Define a mock class (e.g., `mock-llm-client` in `tests/cache-test.lisp`)
2. **Factory functions**: Build mock responses (e.g., `%make-ollama-text-response` in `tests/ollama-test.lisp`)
3. **Let-binding**: Temporarily rebind globals (e.g., `*self-assess-last-test-results*` in `tests/analysis-tools-test.lisp`)

### Parallel Execution Constraints
- SAFE suites run in parallel; UNSAFE suites run sequentially.
- **Do NOT use cross-suite `depends-on`** — each suite must be independent.
- If your test mutates global state, classify it as UNSAFE.

## Commit & Pull Request Guidelines
- Commit messages follow a Conventional Commits pattern: `type(scope): summary` (e.g., `feat(plan): add planning module`).
- PRs should include a short description, the problem being solved, and test status. Add REPL screenshots or transcripts if behavior is user-facing.

## Configuration & Secrets
- Provide API keys via environment variables (`ANTHROPIC_API_KEY`, `CLAUDE_CODE_OAUTH_TOKEN`, `OPENAI_API_KEY`).
- User overrides live at `~/.sibyl/config.lisp`.
