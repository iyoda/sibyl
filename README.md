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
# Anthropic
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (sibyl:start-repl :client (sibyl:make-anthropic-client)))'

# OpenAI
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (sibyl:start-repl :client (sibyl:make-openai-client)))'
```

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
| `/help` | Show help |
| `/tools` | List registered tools |
| `/reset` | Reset conversation |
| `/history` | Show conversation history |
| `/quit` | Quit |

## Project Structure

```
sibyl/
├── sibyl.asd                 # ASDF system definition
├── src/
│   ├── packages.lisp         # Package definitions
│   ├── conditions.lisp       # Condition hierarchy
│   ├── config.lisp           # Configuration management
│   ├── util.lisp             # Utilities
│   ├── llm/
│   │   ├── message.lisp      # Message & conversation data structures
│   │   ├── client.lisp       # LLM client protocol (CLOS)
│   │   └── providers.lisp    # Anthropic / OpenAI implementations
│   ├── tools/
│   │   ├── protocol.lisp     # deftool macro, registry, execution
│   │   └── builtin.lisp      # Built-in tools
│   ├── agent/
│   │   ├── memory.lisp       # Context window management
│   │   └── core.lisp         # Agent loop
│   └── repl.lisp             # REPL interface
└── tests/
    ├── suite.lisp            # Test suite (FiveAM)
    ├── tools-test.lisp       # Tool tests
    └── message-test.lisp     # Message tests
```

## Tests

```lisp
(ql:quickload :sibyl/tests)
(asdf:test-system :sibyl)
```

## Dependencies

| Library | Purpose |
|---------|---------|
| [alexandria](https://common-lisp.net/project/alexandria/) | General-purpose utilities |
| [dexador](https://github.com/fukamachi/dexador) | HTTP client |
| [yason](https://github.com/phmarek/yason) | JSON parser |
| [cl-ppcre](https://edicl.github.io/cl-ppcre/) | Regular expressions |
| [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) | Threading |
| [local-time](https://common-lisp.net/project/local-time/) | Time handling |
| [fiveam](https://common-lisp.net/project/fiveam/) | Test framework |

## License

MIT
