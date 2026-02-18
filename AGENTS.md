# Repository Guidelines

## Project Structure & Module Organization
- `src/` holds the Common Lisp implementation, organized by domain: `llm/`, `tools/`, `agent/`, `plan/`, `mcp/`, and `repl/`.
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
- Always define a per-file test suite and add it to `*safe-suites*` or `*unsafe-suites*` in `tests/suite.lisp`.
  - SAFE: pure logic, no file I/O, no global state mutation, no external processes.
  - UNSAFE: file I/O, global registries/state, random timing, network, or tool execution.

## Commit & Pull Request Guidelines
- Commit messages follow a Conventional Commits pattern: `type(scope): summary` (e.g., `feat(plan): add planning module`).
- PRs should include a short description, the problem being solved, and test status. Add REPL screenshots or transcripts if behavior is user-facing.

## Configuration & Secrets
- Provide API keys via environment variables (`ANTHROPIC_API_KEY`, `CLAUDE_CODE_OAUTH_TOKEN`, `OPENAI_API_KEY`).
- User overrides live at `~/.sibyl/config.lisp`.
