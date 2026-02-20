# Sibyl

**S-expression Intelligence By Yielding Lisp**

A research prototype for a coding agent implemented in Common Lisp.

Explores agent architectures that leverage Lisp macros, the condition system, and CLOS, with LLMs (Claude / GPT) as the backend.

## Features

- **`deftool` macro** — A Lisp DSL for declaratively defining tools
- **Conditions + restarts** — Interactive recovery from tool execution errors via `retry-tool` / `skip-tool` / `use-value`
- **CLOS generic functions** — Provider abstraction through `complete` / `complete-with-tools`; adding a new provider requires only a method definition
- **Thread-safe conversation management** — Lock-guarded conversations using `bordeaux-threads`
- **Interactive REPL** — Chat with the agent, list tools, view history

## Requirements

- [SBCL](http://www.sbcl.org/) (recommended) or another Common Lisp implementation
- [Quicklisp](https://www.quicklisp.org/)
- Anthropic API key, Claude Code OAuth token, or OpenAI API key

## Setup

```bash
# Install SBCL (macOS)
brew install sbcl

# Install Quicklisp
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
```

Link the project as a Quicklisp local project:

```bash
ln -s /path/to/sibyl ~/quicklisp/local-projects/sibyl
```

## Quick Start

```bash
# GPT-5.2-Codex
scripts/start-repl.sh gpt-5.2-codex

# GPT-5-mini
scripts/start-repl.sh gpt-5-mini

# Opus-4.6
scripts/start-repl.sh opus-4.6

# soonet-4.6 (legacy alias; sonnet-4.6 も利用可)
scripts/start-repl.sh soonet-4.6
```

`scripts/start-repl.sh` のデフォルトは `gpt-5.2-codex` です。

## Usage

```lisp
;; Load
(ql:quickload :sibyl)

;; Start the REPL
(sibyl:with-config ()
  (sibyl:start-repl :client (sibyl:make-anthropic-client)))
```

### Authentication

Set an API key or OAuth token via environment variables:

```bash
# Standard Anthropic API key
export ANTHROPIC_API_KEY="sk-ant-api03-..."

# Or Claude Code OAuth token (auto-detected by sk-ant-oat01- prefix)
export CLAUDE_CODE_OAUTH_TOKEN="sk-ant-oat01-..."

# Or OpenAI API key
export OPENAI_API_KEY="sk-..."
```

OAuth tokens are automatically detected and authenticated using `Authorization: Bearer` with the required beta flags. Standard API keys use the `x-api-key` header as usual.

### Defining Custom Tools

```lisp
(sibyl.tools:deftool "count-lines"
    (:description "Count the number of lines in a file"
     :parameters ((:name "path" :type "string" :required t
                   :description "File path")))
  (let* ((path (getf args :path))
         (content (uiop:read-file-string path))
         (lines (count #\Newline content)))
    (format nil "~a: ~a lines" path lines)))
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `/quit`, `/exit` | Quit |
| `/help` | Show help |
| `/tools` | List registered tools |
| `/reset`, `/new` | Reset conversation |
| `/history` | Show conversation history |
| `/mcp` | Show MCP connection status |
| `/plan` | Manage plans (`list`, `new`, `delete`, `status`) |
| `/improve` | Run self-improvement workflow |
| `/review` | Review/approve/reject suggestions |
| `/tokens` | Show token usage stats |
| `/model` | Show/switch model (`list`, `history` supported) |
| `/cost-report` | Show session cost report |
| `/cache-stats` | Show cache hit/miss stats |
| `/sessions` | List saved sessions |
| `/save` | Save current session |
| `/load <session-id>` | Load a saved session |
| `/colors [on|off]` | Toggle terminal colors |
| `/log [on|off|status]` | Toggle/show log output |

## Project Structure

```
sibyl/
├── sibyl.asd                     # ASDF system definition
├── scripts/                      # Utility scripts (REPL launcher, git hooks, checks)
├── src/
│   ├── packages.lisp             # Package definitions
│   ├── conditions.lisp           # Condition hierarchy
│   ├── config.lisp               # Configuration management
│   ├── util.lisp                 # Utilities
│   ├── logging.lisp              # Logging utilities
│   ├── system/                   # System-level helpers
│   ├── llm/
│   │   ├── client.lisp           # LLM client protocol (CLOS)
│   │   ├── providers.lisp        # Anthropic / OpenAI implementations
│   │   ├── model-selector.lisp   # Model selection heuristics
│   │   ├── token-tracker.lisp    # Token/cost tracking
│   │   └── ollama.lisp           # Ollama integration
│   ├── tools/
│   │   ├── protocol.lisp         # deftool macro, registry, execution
│   │   ├── builtin.lisp          # Built-in tools
│   │   ├── lisp-tools.lisp       # Lisp introspection/modification tools
│   │   ├── analysis-tools.lisp   # Analysis/test generation tools
│   │   └── planning-tools.lisp   # Planning helpers
│   ├── agent/
│   │   ├── memory.lisp           # Context window management
│   │   ├── multi-agent.lisp      # Multi-agent orchestration
│   │   └── core.lisp             # Agent loop
│   ├── plan/                      # Plan data model and persistence
│   ├── mcp/                       # MCP client and tool bridge
│   ├── cache/                     # Provider cache adapters/telemetry
│   ├── repl/                      # REPL submodules (display/spinner/session)
│   └── repl.lisp                  # REPL entrypoint
└── tests/
    ├── suite.lisp                # FiveAM suite definitions/classification
    └── *-test.lisp               # Domain-specific test files
```

## Tests

```lisp
(ql:quickload :sibyl/tests)
(asdf:test-system :sibyl)
```

## Git Hooks (pre-push static checks)

To prevent accidental compile-time dependencies on test-only packages in `src/`,
install the repository pre-push hook:

```bash
sbcl --noinform --non-interactive --script scripts/install-git-hooks.lisp
```

The hook runs:

```bash
sbcl --noinform --non-interactive --script scripts/check-no-test-packages-in-src.lisp
```

This check rejects direct `fiveam:` / `it.bese.fiveam:` references under `src/`.
For REPL-side test execution use runtime resolution (`find-package`, `find-symbol`,
`progv`) so test functionality remains available without introducing read-time
package dependencies.

To extend forbidden package prefixes, edit `*forbidden-prefixes*` in
`scripts/check-no-test-packages-in-src.lisp`.

## Dependencies

| Library | Purpose |
|---------|---------|
| [alexandria](https://common-lisp.net/project/alexandria/) | General-purpose utilities |
| [dexador](https://github.com/fukamachi/dexador) | HTTP client |
| [yason](https://github.com/phmarek/yason) | JSON parser |
| [cl-ppcre](https://edicl.github.io/cl-ppcre/) | Regular expressions |
| [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) | Threading |
| [uiop](https://common-lisp.net/project/asdf/uiop.html) | Portable OS/file utilities |
| [local-time](https://common-lisp.net/project/local-time/) | Time handling |
| [ironclad](https://github.com/sharplispers/ironclad) | Cryptographic utilities |
| [fiveam](https://common-lisp.net/project/fiveam/) | Test framework (`:sibyl/tests`) |

## License

MIT
