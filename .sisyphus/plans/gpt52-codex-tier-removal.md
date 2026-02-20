# Add GPT-5.2-Codex & Remove Model Tier System

## TL;DR

> **Quick Summary**: Add GPT-5.2-Codex model support to Sibyl's OpenAI client and remove the entire model tier selection feature (light/medium/heavy). The tier system's complexity analysis, adaptive agent, benchmarks, and A/B testing are removed. A flat model metadata registry and `context-window-for-model` are preserved.
> 
> **Deliverables**:
> - GPT-5.2-Codex pricing entry, model metadata, and default OpenAI model change
> - model-selector.lisp gutted to model metadata registry only (~700 lines removed)
> - Tier config keys replaced with simple `openai.model` / `anthropic.model` keys
> - REPL `/model` simplified to read-only info, `/tier-stats` removed
> - Adaptive agent removed, cost tracking moved to base agent
> - Metrics simplified: no tier fields, no savings, no A/B testing
> - All tier-related tests removed/updated, exports cleaned
> 
> **Estimated Effort**: Large
> **Parallel Execution**: YES - 2 waves
> **Critical Path**: Task 1 → Task 2 → Task 3 → Task 4 → Task 5 → Task 6 → Task 7 → Task 8

---

## Context

### Original Request
User asked to add GPT-5.2-Codex model support, then expanded scope to remove the entire model tier selection feature (light/medium/heavy) that was no longer providing value.

### Interview Summary
**Key Discussions**:
- **Model selection strategy**: Keep model metadata registry (`enhanced-model-config`) but remove tier classification and auto-selection logic
- **model-selector.lisp fate**: Keep file, gut it to only model metadata + `context-window-for-model`
- **REPL commands**: Simplify `/model` to read-only info. Remove `/tier-stats`. Remove tier breakdown from cost reports.
- **Default OpenAI model**: Change from `gpt-5-mini` to `gpt-5.2-codex`
- **Adaptive agent**: Remove entirely — all classes, methods, and REPL integration
- **Cost metrics**: Remove tier fields from `task-cost-record`, remove `tier-rank`, remove A/B testing. Keep cost tracking by model name. Move cost-records to base agent.
- **Config keys**: Replace 6 tier keys with 2 simple keys: `openai.model` and `anthropic.model`
- **Savings system**: Remove entirely — `estimate-baseline-cost-usd`, `decompose-savings`, `__baseline__` model, savings fields from cost records
- **Complexity analysis tool**: Keep `analyze-task-complexity` as standalone utility (scores 1-10), disconnect from tier routing
- **Test strategy**: Tests after implementation

### Research Findings
- **GPT-5.2-Codex**: Model ID `gpt-5.2-codex`, 400K/128K context, $1.75/$14.00 pricing, supports reasoning.effort, optimized for agentic coding
- **Tier system scope**: ~1000 lines across 10 files, 18+ tests, ~55 exported symbols
- **Critical dependency**: `context-window-for-model` reads `*latest-model-tiers*` tier data — needs data migration to flat registry

### Metis Review
**Identified Gaps** (addressed):
- **Cost report lives in adaptive-agent**: Resolved — move cost-records slot to base agent class
- **analyze-task-complexity tool**: Resolved — keep as standalone, remove tier routing
- **Savings/baseline system**: Resolved — remove entirely
- **context-window-for-model data source**: Resolved — migrate to flat `*model-registry*` list
- **use-model-selector parameter in start-repl**: Resolved — remove parameter
- **format-session-report tier display**: Resolved — remove tier distribution, keep cost totals
- **Test count was 18+ not 12**: Resolved — plan accounts for all tests
- **Exported symbols were ~55 not ~30**: Resolved — plan accounts for full cleanup

---

## Work Objectives

### Core Objective
Add GPT-5.2-Codex as the default OpenAI model and completely remove the model tier selection feature, simplifying Sibyl to use direct model specification.

### Concrete Deliverables
- `src/llm/pricing.lisp`: GPT-5.2-Codex pricing entry; `__baseline__` and savings functions removed
- `src/llm/model-selector.lisp`: Gutted to ~50 lines — flat `*model-registry*`, `enhanced-model-config`, `context-window-for-model`
- `src/llm/providers.lisp`: Default model changed to `gpt-5.2-codex`
- `src/llm/metrics.lisp`: Simplified — no tier fields, no savings fields, no A/B testing
- `src/config.lisp`: 6 tier keys replaced with 2 simple model keys
- `src/repl.lisp`: `/model` read-only, `/tier-stats` removed, `/cost-report` adapted, `start-repl` simplified
- `src/packages.lisp`: ~40 tier-related exports removed
- `src/tools/builtin.lisp`: `create-adaptive-agent-team` tool removed, `*global-model-selector*` removed
- `src/agent/multi-agent.lisp`: `make-specialized-agent-adaptive` removed
- `src/agent/core.lisp`: `cost-records` slot added to base agent
- All affected test files updated/cleaned

### Definition of Done
- [x] `(ql:quickload :sibyl :silent t)` compiles cleanly (no warnings about undefined functions/variables)
- [x] `(asdf:test-system :sibyl)` passes all tests
- [x] `(sibyl.llm:lookup-model-pricing "gpt-5.2-codex")` returns correct pricing
- [x] `(sibyl.llm:client-model (sibyl.llm:make-openai-client :api-key "x"))` returns `"gpt-5.2-codex"`
- [x] `(sibyl.llm:context-window-for-model "gpt-5.2-codex")` returns `400000`
- [x] No tier-related symbols exported from `sibyl.llm` package
- [x] `/model` in REPL shows current model info (read-only)
- [x] `/tier-stats` no longer recognized as command

### Must Have
- GPT-5.2-Codex pricing entry with correct values ($1.75/$14.00/$0.0/$0.175)
- GPT-5.2-Codex as default OpenAI model
- GPT-5.2-Codex in model registry with 400000 context window
- `context-window-for-model` working with flat registry (not tier data)
- Cost tracking on base agent (not adaptive-agent)
- Clean compilation with no undefined-function warnings
- All tests passing

### Must NOT Have (Guardrails)
- Do NOT rename `model-selector.lisp` to `model-registry.lisp` (separate follow-up PR)
- Do NOT add `reasoning.effort` support to OpenAI client (separate feature)
- Do NOT refactor pricing.lisp beyond removing tier-savings functions and adding gpt-5.2-codex
- Do NOT touch `create-agent-team` tool (non-adaptive version) in builtin.lisp
- Do NOT change sibyl.asd module entries (keep `(:file "model-selector")`, just gut contents)
- Do NOT redesign the cost tracking system — only migrate cost-records and simplify structs
- Do NOT rewrite REPL help text beyond updating for removed commands
- Do NOT warn about orphaned user config keys (silently ignore old tier keys)

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks in this plan MUST be verifiable WITHOUT any human action.
> Every criterion MUST be verifiable by running a command or using a tool.

### Test Decision
- **Infrastructure exists**: YES (FiveAM test framework)
- **Automated tests**: Tests after implementation
- **Framework**: FiveAM (via `(asdf:test-system :sibyl)`)

### Agent-Executed QA Scenarios (MANDATORY — ALL tasks)

**Verification Tool by Deliverable Type:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| **Lisp compilation** | Bash (sbcl) | `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'` — exit code 0 |
| **Test suite** | Bash (sbcl) | `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(quit)'` |
| **Function behavior** | Bash (sbcl --eval) | Load system, call function, assert return value |
| **Symbol existence** | Bash (sbcl --eval) | `(find-symbol "NAME" :sibyl.llm)` checks export status |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately):
├── Task 1: Add GPT-5.2-Codex (pricing + default change) [no dependencies]
└── Task 2: Migrate model-selector.lisp to flat registry [no dependencies]

Wave 2 (After Wave 1):
├── Task 3: Remove tier infrastructure from config + metrics + pricing [depends: 1, 2]
├── Task 4: Remove adaptive-agent, move cost-records to base agent [depends: 2]
└── Task 5: Simplify REPL commands [depends: 2, 4]

Wave 3 (After Wave 2):
├── Task 6: Clean up multi-agent, builtin tools, packages exports [depends: 3, 4, 5]
└── Task 7: Clean up tests and suite classification [depends: 6]

Wave 4 (After Wave 3):
└── Task 8: Final compilation + test verification [depends: 7]
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 3 | 2 |
| 2 | None | 3, 4, 5 | 1 |
| 3 | 1, 2 | 6 | 4, 5 |
| 4 | 2 | 5, 6 | 3 |
| 5 | 2, 4 | 6 | 3 |
| 6 | 3, 4, 5 | 7 | None |
| 7 | 6 | 8 | None |
| 8 | 7 | None | None (final) |

### Agent Dispatch Summary

| Wave | Tasks | Recommended Agents |
|------|-------|-------------------|
| 1 | 1, 2 | task(category="quick") for Task 1; task(category="unspecified-high") for Task 2 |
| 2 | 3, 4, 5 | task(category="unspecified-high") for each |
| 3 | 6, 7 | task(category="unspecified-low") for each |
| 4 | 8 | task(category="quick") for verification |

---

## TODOs

- [x] 1. Add GPT-5.2-Codex model support

  **What to do**:
  - Add GPT-5.2-Codex pricing entry to `*model-pricing-table*` in `src/llm/pricing.lisp`
  - Change `openai-client` class `:default-initargs` from `"gpt-5-mini"` to `"gpt-5.2-codex"` in `src/llm/providers.lisp:510`
  - Change `make-openai-client` default model parameter from `"gpt-5-mini"` to `"gpt-5.2-codex"` in `src/llm/providers.lisp:514`
  - Verify GPT-5 family detection still works (prefix match `"gpt-5"` in `openai-temperature-pair` at providers.lisp:494-504 — `"gpt-5.2-codex"` starts with `"gpt-5"` so this works automatically)

  **Must NOT do**:
  - Do NOT add reasoning.effort support
  - Do NOT modify the OpenAI streaming logic
  - Do NOT change max_completion_tokens behavior

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Simple additions to existing data structures, well-defined locations
  - **Skills**: []
    - No specialized skills needed for data entry
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction needed
    - `frontend-ui-ux`: No UI work

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 2)
  - **Blocks**: Task 3
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References** (existing code to follow):
  - `src/llm/pricing.lisp:8-27` — Existing pricing entries format. Add new entry following same `(model-name :input N :output N :cache-write N :cache-read N)` pattern after the `gpt-5-mini` entry (line 23-24).
  - `src/llm/providers.lisp:494-504` — `openai-temperature-pair` function. GPT-5 detection uses prefix match `"gpt-5"`. Verify `"gpt-5.2-codex"` passes this check (it does — starts with `"gpt-5"`).
  - `src/llm/providers.lisp:508-514` — `openai-client` class definition and `make-openai-client` function. Change default model in both `:default-initargs` (line 510) and function default (line 514).

  **External References**:
  - GPT-5.2-Codex pricing: $1.75 input, $14.00 output, $0.0 cache-write, $0.175 cache-read (USD per 1M tokens)

  **WHY Each Reference Matters**:
  - pricing.lisp:8-27: Follow exact format of existing entries for consistency
  - providers.lisp:494-504: Verify GPT-5 detection doesn't need changes
  - providers.lisp:508-514: Two locations for default model must both change

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: GPT-5.2-Codex pricing lookup returns correct values
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let ((p (sibyl.llm:lookup-model-pricing "gpt-5.2-codex")))
                       (assert (= (getf p :input) 1.75))
                       (assert (= (getf p :output) 14.0))
                       (assert (= (getf p :cache-write) 0.0))
                       (assert (= (getf p :cache-read) 0.175))
                       (format t "PASS: pricing correct~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0, output contains "PASS: pricing correct"
    Expected Result: All pricing values match GPT-5.2-Codex specs
    Evidence: Terminal output captured

  Scenario: Default OpenAI client uses gpt-5.2-codex
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let ((model (sibyl.llm:client-model (sibyl.llm:make-openai-client :api-key "test"))))
                       (assert (string= model "gpt-5.2-codex"))
                       (format t "PASS: default model is gpt-5.2-codex~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0, output contains "PASS"
    Expected Result: make-openai-client defaults to gpt-5.2-codex
    Evidence: Terminal output captured

  Scenario: GPT-5 temperature handling works for gpt-5.2-codex
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let* ((client (sibyl.llm:make-openai-client :api-key "test" :model "gpt-5.2-codex"))
                            (pair (sibyl.llm::openai-temperature-pair client)))
                       (assert (null pair) nil "Temperature should be nil for GPT-5 with default temp")
                       (format t "PASS: temperature handling correct~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: GPT-5 family detection works for gpt-5.2-codex prefix
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): add gpt-5.2-codex pricing and set as default OpenAI model`
  - Files: `src/llm/pricing.lisp`, `src/llm/providers.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

---

- [x] 2. Migrate model-selector.lisp to flat model registry

  **What to do**:
  - Create a flat `*model-registry*` parameter — a list of `enhanced-model-config` instances (extracted from `*latest-model-tiers*`)
  - Add GPT-5.2-Codex entry to `*model-registry*` with `:context-window 400000`, `:capabilities '(:advanced-reasoning :multimodal :agentic-coding)`
  - Rewrite `context-window-for-model` to iterate `*model-registry*` instead of `*latest-model-tiers*`
  - Keep `enhanced-model-config` class (and its parent `model-config` class) — these store model metadata
  - Keep `complexity-analysis` class, `task-analyzer` class, `*default-complexity-rules*`, `analyze-task-complexity` function, and `make-task-analyzer` — these are kept as standalone utility (not tied to tiers)
  - DELETE everything else from model-selector.lisp:
    - `model-tier` class and all tier-related functions
    - `*default-model-tiers*`, `*latest-model-tiers*`, `*tier-metadata*` parameters
    - `model-selector` class, `latest-model-selector` class
    - `build-model-tiers-from-config`, `configured-model-name`, `make-model-selector`, `make-default-model-selector`
    - `find-tier`, `select-model-for-task`, `select-latest-model-for-task`, `make-latest-model-selector`
    - `adaptive-agent` class and all methods: `make-adaptive-agent`, `adapt-model-for-task`, `agent-run-adaptive`, `create-client-for-model`
    - `*benchmark-task-set*`, `evaluate-classification-accuracy`

  **Must NOT do**:
  - Do NOT rename the file to model-registry.lisp
  - Do NOT change sibyl.asd module entry
  - Do NOT remove `model-config` class (enhanced-model-config inherits from it)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Large deletion + data migration requiring careful preservation of surviving code
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `git-master`: No complex git operations needed

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 1)
  - **Blocks**: Tasks 3, 4, 5
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `src/llm/model-selector.lisp:428-436` — `enhanced-model-config` class definition (KEEP this)
  - `src/llm/model-selector.lisp:439-532` — `*latest-model-tiers*` parameter — extract the `enhanced-model-config` instances from each tier's `:models` list into the new flat `*model-registry*`. There are entries for claude-haiku-4-5-20251015, claude-sonnet-4-6, claude-opus-4-6, gpt-5-mini, gpt-5-nano, gpt-5.2, plus Ollama models.
  - `src/llm/model-selector.lisp:709-723` — `context-window-for-model` function — currently iterates `*latest-model-tiers*` and checks `tier-models`. Rewrite to iterate `*model-registry*` directly.
  - `src/llm/model-selector.lisp:6-16` — `model-tier` class (DELETE this)
  - `src/llm/model-selector.lisp:20-26` — `model-config` class (KEEP this — parent of enhanced-model-config)
  - `src/llm/model-selector.lisp:28-45` — `task-analyzer` and `complexity-analysis` classes (KEEP these)
  - `src/llm/model-selector.lisp:47-53` — `model-selector` class (DELETE this)
  - `src/llm/model-selector.lisp:57-107` — `*default-model-tiers*` (DELETE)
  - `src/llm/model-selector.lisp:111-153` — `*default-complexity-rules*` (KEEP)
  - `src/llm/model-selector.lisp:155-157` — `make-task-analyzer` (KEEP)
  - `src/llm/model-selector.lisp:160-169` — `*tier-metadata*` (DELETE)
  - `src/llm/model-selector.lisp:171-218` — `configured-model-name`, `build-model-tiers-from-config` (DELETE)
  - `src/llm/model-selector.lisp:226-296` — `make-model-selector`, `make-default-model-selector`, `find-tier` (DELETE)
  - `src/llm/model-selector.lisp:298-340` — `select-model-for-task` (DELETE)
  - `src/llm/model-selector.lisp:342-425` — `adaptive-agent` class and methods (DELETE)
  - `src/llm/model-selector.lisp:535-611` — `select-latest-model-for-task`, `latest-model-selector`, `make-latest-model-selector` (DELETE)
  - `src/llm/model-selector.lisp:617-703` — `*benchmark-task-set*`, `evaluate-classification-accuracy` (DELETE)
  - `src/llm/model-selector.lisp:242-291` — `analyze-task-complexity` methods (KEEP — but modify the method on `model-selector` to be standalone, only keep the method on `task-analyzer`)

  **WHY Each Reference Matters**:
  - Lines 439-532: Source data for migration — extract model configs from tier structure into flat list
  - Lines 709-723: Critical function that must survive but needs new data source
  - Lines 6-53: Understanding which classes to keep vs delete

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: context-window-for-model returns correct values from flat registry
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly after migration
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(progn
                       (assert (= (sibyl.llm:context-window-for-model "gpt-5.2-codex") 400000))
                       (assert (= (sibyl.llm:context-window-for-model "claude-sonnet-4-6") 200000))
                       (assert (= (sibyl.llm:context-window-for-model "gpt-5-mini") 128000))
                       (assert (= (sibyl.llm:context-window-for-model "unknown-model") 200000))
                       (format t "PASS: context-window-for-model works~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: Known models return their context windows, unknown falls back to 200000
    Evidence: Terminal output captured

  Scenario: model-tier class no longer exists
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(assert (null (find-class (quote sibyl.llm::model-tier) nil)))' \
             --eval '(format t "PASS: model-tier class removed~%")' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: model-tier class no longer defined
    Evidence: Terminal output captured

  Scenario: analyze-task-complexity still works as standalone
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let* ((analyzer (sibyl.llm:make-task-analyzer))
                            (result (sibyl.llm:analyze-task-complexity analyzer "simple hello world")))
                       (assert (numberp (sibyl.llm:complexity-score result)))
                       (format t "PASS: complexity analysis works standalone, score=~a~%"
                               (sibyl.llm:complexity-score result)))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: Complexity scoring works without tier system
    Evidence: Terminal output captured

  Scenario: *model-registry* contains gpt-5.2-codex entry
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let ((found (find "gpt-5.2-codex" sibyl.llm::*model-registry*
                                        :key (function sibyl.llm:model-name)
                                        :test (function string-equal))))
                       (assert found nil "gpt-5.2-codex not found in registry")
                       (assert (= (sibyl.llm:model-context-window found) 400000))
                       (format t "PASS: gpt-5.2-codex in registry with 400K context~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: GPT-5.2-Codex is in the model registry with correct context window
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `refactor(llm): migrate model-selector to flat model registry, remove tier classes`
  - Files: `src/llm/model-selector.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

---

- [x] 3. Remove tier infrastructure from config, metrics, and pricing

  **What to do**:
  - **config.lisp**: Remove 6 tier config keys (`models.light.anthropic`, `models.light.openai`, `models.medium.anthropic`, `models.medium.openai`, `models.heavy.anthropic`, `models.heavy.openai`). Add 2 new keys: `("openai.model" . "gpt-5.2-codex")` and `("anthropic.model" . "claude-sonnet-4-6")`. Remove `"optimization.auto-model-routing"` key if present.
  - **pricing.lisp**: Remove `("__baseline__" ...)` entry from `*model-pricing-table*`. Remove functions: `estimate-baseline-cost-usd`, `compute-savings-pct`, `decompose-savings`. Keep `estimate-cost-usd` and `lookup-model-pricing` intact.
  - **metrics.lisp**: 
    - Remove from `task-cost-record` struct: `tier-name`, `complexity-score`, `baseline-cost-usd`, `savings-usd`, `savings-pct` fields
    - Simplify `make-task-cost-record-from-delta`: remove `tier-name` and `complexity-score` parameters, remove baseline/savings calculation
    - Remove from `session-cost-report` struct: `total-baseline-cost-usd`, `total-savings-usd`, `total-savings-pct`, `tier-distribution` fields
    - Simplify `compute-session-report`: remove baseline aggregation and tier counting
    - Remove ALL A/B testing infrastructure: `provisioning-stats` struct, `ab-test-report` struct, `tier-rank`, `classify-provisioning`, `compute-provisioning-stats`, `evaluate-provisioning`, `make-adaptive-tier-predictor`, `make-fixed-tier-predictor`, `run-tier-ab-test`, `run-adaptive-vs-baseline-ab-test`, `format-provisioning-stats`, `format-ab-test-report`
    - Simplify `format-session-report`: remove baseline/savings/tier-distribution display, keep task count, actual cost, cache hit rate

  **Must NOT do**:
  - Do NOT remove `estimate-cost-usd` or `lookup-model-pricing` — these are core cost functions
  - Do NOT remove `snapshot-tracker` or `tracker-delta` — these are used for cost measurement
  - Do NOT redesign the cost tracking system beyond removing tier/savings fields

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Coordinated changes across 3 files with struct modifications and function removals
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 4, 5)
  - **Blocks**: Task 6
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `src/config.lisp:91-102` — Current tier config keys (lines 97-102). Replace the 6 `models.*.*` entries with 2 new `openai.model` and `anthropic.model` entries.
  - `src/llm/pricing.lisp:25-27` — `__baseline__` entry and its comment. Remove these 3 lines.
  - `src/llm/pricing.lisp:71-87` — `estimate-baseline-cost-usd` and `compute-savings-pct` functions. DELETE entirely.
  - `src/llm/pricing.lisp:89-145` — `decompose-savings` function. DELETE entirely.
  - `src/llm/metrics.lisp:11-29` — `task-cost-record` struct definition. Remove `tier-name`, `complexity-score`, `baseline-cost-usd`, `savings-usd`, `savings-pct` fields.
  - `src/llm/metrics.lisp:61-96` — `make-task-cost-record-from-delta` function. Simplify: remove `tier-name` and `complexity-score` parameters, remove baseline/savings calculation.
  - `src/llm/metrics.lisp:103-159` — `session-cost-report` struct and `compute-session-report`. Remove baseline/savings/tier fields.
  - `src/llm/metrics.lisp:162-359` — ALL A/B testing code (lines 162-327) and `format-session-report` (lines 333-358). Delete A/B testing entirely. Simplify format-session-report.

  **WHY Each Reference Matters**:
  - config.lisp:97-102: Must replace old keys with new ones, preserving other config entries
  - pricing.lisp: Must surgically remove savings functions while keeping core pricing
  - metrics.lisp: Struct changes affect all code that creates/reads cost records

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: New config keys exist, old tier keys removed
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(sibyl:with-config ()
                       (assert (string= (sibyl.config:config-value "openai.model") "gpt-5.2-codex"))
                       (assert (string= (sibyl.config:config-value "anthropic.model") "claude-sonnet-4-6"))
                       (assert (null (sibyl.config:config-value "models.light.anthropic" nil)))
                       (assert (null (sibyl.config:config-value "models.heavy.openai" nil)))
                       (format t "PASS: config keys correct~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: New simple model keys work, old tier keys return nil
    Evidence: Terminal output captured

  Scenario: __baseline__ model removed from pricing table
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(assert (null (assoc "__baseline__" sibyl.llm:*model-pricing-table* :test (function string-equal))))' \
             --eval '(format t "PASS: __baseline__ removed~%")' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: No __baseline__ entry in pricing table
    Evidence: Terminal output captured

  Scenario: Simplified task-cost-record has no tier fields
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let ((rec (sibyl.llm:make-task-cost-record)))
                       (assert (not (slot-exists-p rec (quote sibyl.llm::tier-name))))
                       (assert (not (slot-exists-p rec (quote sibyl.llm::baseline-cost-usd))))
                       (format t "PASS: tier fields removed from task-cost-record~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: task-cost-record struct no longer has tier-related fields
    Evidence: Terminal output captured

  Scenario: estimate-cost-usd still works for gpt-5.2-codex
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let ((cost (sibyl.llm:estimate-cost-usd "gpt-5.2-codex"
                                   :input-tokens 1000000 :output-tokens 100000)))
                       (assert (> (getf cost :total) 0))
                       (format t "PASS: cost estimation works, total=$~,4f~%" (getf cost :total)))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: Cost estimation works correctly for gpt-5.2-codex
    Evidence: Terminal output captured
  ```

  **Commit**: YES (groups with Task 4)
  - Message: `refactor(llm): remove tier config keys, savings system, and A/B testing from metrics`
  - Files: `src/config.lisp`, `src/llm/pricing.lisp`, `src/llm/metrics.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

---

- [x] 4. Remove adaptive-agent and move cost-records to base agent

  **What to do**:
  - **agent/core.lisp**: Add `cost-records` slot to the base `agent` class:
    ```lisp
    (cost-records :initform nil
                  :accessor agent-cost-records
                  :documentation "List of task-cost-record instances for this session.")
    ```
  - **repl.lisp**: In `handle-cost-report-command` (lines 800-824):
    - Remove the `(typep agent 'sibyl.llm::adaptive-agent)` check
    - Access `(sibyl.agent::agent-cost-records agent)` instead of `(sibyl.llm::agent-cost-records agent)`
    - Remove tier distribution bar chart (since no tiers)
    - Remove the "Cost reporting requires adaptive agent" message
  - **repl.lisp**: In `save-cost-log` (lines 756-798): Remove `baseline_cost_usd`, `savings_usd`, `savings_pct` fields from saved JSON.
  - **repl.lisp**: In `start-repl` (lines 1435-1464):
    - Remove `:use-model-selector` keyword parameter
    - Remove the `ollama-p` / `adaptive-agent` branching logic — always create regular agent
    - Remove the `optimization.auto-model-routing` config check
    - Update docstring to remove adaptive model selection example
  - Ensure `agent-cost-records` is exported from the `sibyl.agent` package (check packages.lisp for agent exports)

  **Must NOT do**:
  - Do NOT change the agent loop logic
  - Do NOT modify how token-tracker works
  - Do NOT add automatic cost recording to agent-run (that was adaptive-agent's job and can be added later if needed)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Cross-cutting change across agent core and REPL with slot migration
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 5)
  - **Blocks**: Tasks 5, 6
  - **Blocked By**: Task 2

  **References**:

  **Pattern References**:
  - `src/agent/core.lisp:10-41` — Base `agent` class definition. Add `cost-records` slot after `token-tracker` slot (line 40).
  - `src/llm/model-selector.lisp:342-360` — Current `adaptive-agent` class (being deleted in Task 2). Shows the `cost-records` slot definition to migrate.
  - `src/repl.lisp:800-824` — `handle-cost-report-command`. Currently gates on `adaptive-agent` type. Remove the type check, use base agent accessor.
  - `src/repl.lisp:756-798` — `save-cost-log`. Remove baseline/savings JSON fields.
  - `src/repl.lisp:1435-1464` — `start-repl`. Remove `:use-model-selector` parameter and adaptive-agent creation branch.
  - `src/repl.lisp:826-869` — `handle-tier-stats-command` — Will be removed in Task 5, but note its `adaptive-agent` type check.
  - `src/packages.lisp` — Check sibyl.agent package exports section for where to add `agent-cost-records` export.

  **WHY Each Reference Matters**:
  - core.lisp:10-41: Exact location to add the new slot
  - model-selector.lisp:342-360: Shows the slot definition to copy (initform nil, accessor name)
  - repl.lisp:800-824: Must change type check and accessor package
  - repl.lisp:1435-1464: Must simplify agent creation

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Base agent has cost-records slot
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(let ((agent (sibyl.agent:make-agent
                                    :client (sibyl.llm:make-openai-client :api-key "test"))))
                       (assert (slot-exists-p agent (quote sibyl.agent::cost-records)))
                       (assert (null (sibyl.agent::agent-cost-records agent)))
                       (format t "PASS: base agent has cost-records slot~%"))' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: Regular agent has cost-records slot initialized to nil
    Evidence: Terminal output captured

  Scenario: start-repl no longer accepts :use-model-selector
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. Verify in source that start-repl lambda-list no longer contains use-model-selector
      2. Grep src/repl.lisp for "use-model-selector" — should return 0 matches
    Expected Result: Parameter removed from start-repl
    Evidence: Grep output showing no matches

  Scenario: handle-cost-report-command works without adaptive-agent
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. Grep src/repl.lisp for "adaptive-agent" — should return 0 matches
      2. Grep src/repl.lisp for "use-model-selector" — should return 0 matches
    Expected Result: No references to adaptive-agent or use-model-selector in repl.lisp
    Evidence: Grep output
  ```

  **Commit**: YES (groups with Task 3)
  - Message: `refactor(agent): move cost-records to base agent, remove adaptive-agent from REPL`
  - Files: `src/agent/core.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

---

- [x] 5. Simplify REPL commands

  **What to do**:
  - **Remove `/tier-stats` command**:
    - Delete `handle-tier-stats-command` function (repl.lisp:826-869)
    - Remove `(cons :tier-stats #'handle-tier-stats-command)` from `*command-handlers*` (line 886)
    - Remove `("/tier-stats" . :tier-stats)` from `*repl-commands*` alist (line 24 area)
  - **Simplify `/model` command** (`handle-model-command`, repl.lisp:583-621):
    - Make it read-only: show current model name, context window, and provider
    - Remove subcommands: `light`, `medium`, `heavy`, `auto`, `benchmark`
    - Remove calls to `find-tier`, `adapt-model-for-task`, `evaluate-classification-accuracy`
    - Keep `status` as the only behavior (show model info)
    - Remove helper functions: `%model-show-status` (if it references tiers), `%model-switch-tier`, `%model-enable-auto`, `%model-run-benchmark`
  - **Remove tier bar chart**: Delete `format-tier-bar-chart` function if it exists
  - **Update `/help` command**: Remove `/tier-stats` and tier-related `/model` subcommands from help text

  **Must NOT do**:
  - Do NOT remove `/cost-report` command (it's being adapted in Task 4)
  - Do NOT rewrite all REPL help text — only update for removed commands
  - Do NOT touch the streaming display logic

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Multiple function removals and command simplification in repl.lisp
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 4) — but depends on Task 4 completing first for `/cost-report` changes
  - **Blocks**: Task 6
  - **Blocked By**: Tasks 2, 4

  **References**:

  **Pattern References**:
  - `src/repl.lisp:583-621` — `handle-model-command` and its subcommand parsing. Simplify to just display model info.
  - `src/repl.lisp:623-652` — `%model-show-status` helper. May keep simplified version or inline.
  - `src/repl.lisp:654-676` — `%model-switch-tier` helper. DELETE.
  - `src/repl.lisp:678-685` — `%model-enable-auto` helper. DELETE.
  - `src/repl.lisp:687-699` — `%model-run-benchmark` helper. DELETE.
  - `src/repl.lisp:826-869` — `handle-tier-stats-command`. DELETE entirely.
  - `src/repl.lisp:873-886` — `*command-handlers*` alist. Remove `:tier-stats` entry.
  - `src/repl.lisp:24` — `*repl-commands*` mapping. Remove `/tier-stats` entry.

  **WHY Each Reference Matters**:
  - Lines 583-699: Main /model command and all its helper functions
  - Lines 826-869: Entire /tier-stats handler to delete
  - Lines 873-886: Command registry that needs entries removed
  - Line 24: Command name mapping

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: /tier-stats no longer registered
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. Grep src/repl.lisp for "tier-stats" — should return 0 matches
      2. Grep src/repl.lisp for "handle-tier-stats" — should return 0 matches
    Expected Result: All tier-stats references removed
    Evidence: Grep output

  Scenario: /model command no longer references tiers
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. Grep src/repl.lisp for "model-switch-tier" — should return 0 matches
      2. Grep src/repl.lisp for "model-enable-auto" — should return 0 matches
      3. Grep src/repl.lisp for "model-run-benchmark" — should return 0 matches
      4. Grep src/repl.lisp for "find-tier" — should return 0 matches
    Expected Result: All tier-related /model subcommand code removed
    Evidence: Grep output

  Scenario: /model command still works as read-only info display
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. Verify handle-model-command function exists in repl.lisp
      2. Verify it displays model name and context window info
      3. Verify it does NOT accept "light", "medium", "heavy", "auto", "benchmark" arguments
    Expected Result: /model is simplified to read-only
    Evidence: Source code inspection
  ```

  **Commit**: YES
  - Message: `refactor(repl): simplify /model to read-only, remove /tier-stats command`
  - Files: `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

---

- [x] 6. Clean up multi-agent, builtin tools, and package exports

  **What to do**:
  - **multi-agent.lisp**: Remove `make-specialized-agent-adaptive` method (around line 308-316). Keep `make-specialized-agent` and `create-agent-team` intact.
  - **builtin.lisp**: 
    - Remove `*global-model-selector*` global variable
    - Remove `ensure-global-model-selector` function
    - Remove `create-adaptive-agent-team` tool definition
    - Keep `analyze-task-complexity` tool (wrapping standalone complexity analyzer) — but update it to use `make-task-analyzer` directly instead of going through model-selector
  - **packages.lisp**: Remove tier-related exports from `sibyl.llm` package:
    - Remove: `model-tier`, `model-selector`, `adaptive-agent`, `make-model-selector`, `make-default-model-selector`, `make-adaptive-agent`, `select-model-for-task`, `create-client-for-model`, `adapt-model-for-task`, `agent-run-adaptive`, `recommended-tier`, `tier-name`, `tier-description`, `tier-models`, `latest-model-selector`, `make-latest-model-selector`, `select-latest-model-for-task`
    - Remove: `estimate-baseline-cost-usd`, `compute-savings-pct`, `decompose-savings`
    - Remove: `task-cost-record-tier-name`, `task-cost-record-complexity-score`, `task-cost-record-baseline-cost-usd`, `task-cost-record-savings-usd`, `task-cost-record-savings-pct`
    - Remove: `session-cost-report-total-baseline-cost-usd`, `session-cost-report-total-savings-usd`, `session-cost-report-total-savings-pct`, `session-cost-report-tier-distribution`
    - Remove ALL provisioning/A/B exports: `provisioning-stats`, `make-provisioning-stats`, `provisioning-stats-total`, `provisioning-stats-correct`, `provisioning-stats-over`, `provisioning-stats-under`, `provisioning-stats-unknown`, `provisioning-stats-accuracy`, `provisioning-stats-over-rate`, `provisioning-stats-under-rate`, `ab-test-report`, `make-ab-test-report`, `ab-test-report-name-a`, `ab-test-report-name-b`, `ab-test-report-stats-a`, `ab-test-report-stats-b`, `ab-test-report-delta-accuracy`, `ab-test-report-delta-over-rate`, `ab-test-report-delta-under-rate`, `ab-test-report-results-a`, `ab-test-report-results-b`, `tier-rank`, `classify-provisioning`, `compute-provisioning-stats`, `evaluate-provisioning`, `make-adaptive-tier-predictor`, `make-fixed-tier-predictor`, `run-tier-ab-test`, `run-adaptive-vs-baseline-ab-test`, `format-provisioning-stats`, `format-ab-test-report`
    - Remove: `agent-cost-records` from sibyl.llm exports (moved to sibyl.agent package)
    - Keep: `model-config`, `enhanced-model-config`, `model-name`, `model-provider`, `model-release-date`, `model-version`, `model-context-window`, `model-capabilities`, `context-window-for-model`
    - Keep: `task-analyzer`, `complexity-analysis`, `make-task-analyzer`, `analyze-task-complexity`, `complexity-reasoning`, `complexity-score`, `complexity-factors`
    - Keep: `task-cost-record`, `make-task-cost-record`, `task-cost-record-task-description`, `task-cost-record-model-name`, `task-cost-record-timestamp`, `task-cost-record-input-tokens`, `task-cost-record-output-tokens`, `task-cost-record-cache-write-tokens`, `task-cost-record-cache-read-tokens`, `task-cost-record-actual-cost-usd`
    - Keep: `session-cost-report` and remaining accessors
    - Keep: `snapshot-tracker`, `tracker-delta`, `make-task-cost-record-from-delta`, `compute-session-report`, `format-session-report`
  - Add `agent-cost-records` export to `sibyl.agent` package in packages.lisp (if not already exported)

  **Must NOT do**:
  - Do NOT remove `create-agent-team` tool or `make-specialized-agent` method
  - Do NOT change sibyl.asd
  - Do NOT remove `analyze-task-complexity` export (keeping as standalone)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: Mostly mechanical deletion of exports and removing specific functions
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (sequential after Wave 2)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 3, 4, 5

  **References**:

  **Pattern References**:
  - `src/agent/multi-agent.lisp:308-316` — `make-specialized-agent-adaptive` method to DELETE
  - `src/tools/builtin.lisp:132-206` — Area containing `*global-model-selector*`, `ensure-global-model-selector`, and adaptive agent tools. Delete selectively.
  - `src/packages.lisp:143-249` — Full block of tier-related exports to clean up. Work through line by line.

  **WHY Each Reference Matters**:
  - multi-agent.lisp: Must remove only the adaptive variant, not the base method
  - builtin.lisp: Must keep analyze-task-complexity tool but update its implementation
  - packages.lisp: Largest batch of changes — must be thorough to avoid "symbol not exported" errors

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: System compiles cleanly with no undefined-function warnings
    Tool: Bash (sbcl)
    Preconditions: All prior tasks completed
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'
      2. Assert: exit code 0, no warnings about undefined functions or unexported symbols
    Expected Result: Clean compilation
    Evidence: Terminal output captured

  Scenario: Removed symbols are not exported
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(dolist (sym (quote ("ADAPTIVE-AGENT" "MODEL-TIER" "MODEL-SELECTOR"
                                          "MAKE-ADAPTIVE-AGENT" "TIER-RANK"
                                          "RUN-TIER-AB-TEST" "DECOMPOSE-SAVINGS")))
                       (multiple-value-bind (s status) (find-symbol sym :sibyl.llm)
                         (declare (ignore s))
                         (when (eq status :external)
                           (error "~a should not be exported" sym))))' \
             --eval '(format t "PASS: tier symbols not exported~%")' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: None of the removed symbols are externally accessible
    Evidence: Terminal output captured

  Scenario: Kept symbols are still exported
    Tool: Bash (sbcl)
    Preconditions: System loads cleanly
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' \
             --eval '(dolist (sym (quote ("ENHANCED-MODEL-CONFIG" "CONTEXT-WINDOW-FOR-MODEL"
                                          "ANALYZE-TASK-COMPLEXITY" "ESTIMATE-COST-USD"
                                          "LOOKUP-MODEL-PRICING")))
                       (multiple-value-bind (s status) (find-symbol sym :sibyl.llm)
                         (declare (ignore s))
                         (unless (eq status :external)
                           (error "~a should be exported" sym))))' \
             --eval '(format t "PASS: kept symbols still exported~%")' \
             --eval '(quit)'
      2. Assert: exit code 0
    Expected Result: Essential symbols remain exported
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `refactor(llm): clean up tier exports, remove adaptive tools from multi-agent and builtin`
  - Files: `src/agent/multi-agent.lisp`, `src/tools/builtin.lisp`, `src/packages.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

---

- [x] 7. Clean up tests and suite classification

  **What to do**:
  - **tests/token-tracking-test.lisp**: Remove the `model-selector-suite` (lines ~200-294). This includes tests: `make-default-model-selector`, `model-selector-light-tier-for-simple-input`, `model-selector-heavy-tier-for-complex-input`, `selector-tiers-contains-light-medium-heavy`, and any other tier-related tests. Keep all other test suites in this file.
  - **tests/phase6-test.lisp**: Remove tier-related tests (lines ~59-109): `tier-cost-factors-reflect-price-ratios`, `estimate-cost-reflects-tier-savings`, `benchmark-light-accuracy-meets-kpi`, `benchmark-overall-accuracy-meets-kpi`. Keep tests that are about cost estimation without tiers (if any). Also remove any Japanese tier tests.
  - **tests/anthropic-streaming-test.lisp**: Remove/update tests that reference `*latest-model-tiers*` (lines ~173-206). These tests check thinking capability by looking up models in tier data. Update them to use `*model-registry*` instead, or rewrite to check capabilities directly.
  - **tests/ollama-test.lisp**: Remove test `latest-model-tiers-include-ollama` (line ~701-705). This checks that ollama model is in the "heavy" tier — no longer applicable.
  - **tests/provisioning-test.lisp**: Remove entire test file contents (or all tier-related tests). This file tests A/B tier provisioning which is being removed.
  - **tests/creation-tools-test.lisp**: Remove `model-selector-basic` test if it references tier system.
  - **tests/sexp-tools-test.lisp**: Remove `model-selector-basic` test if it references tier system.
  - **tests/suite.lisp**: 
    - Remove `model-selector-suite` from `*safe-suites*` 
    - Remove `provisioning-tests` from `*safe-suites*` (or `*unsafe-suites*`)
    - Remove any other references to deleted test suites
  - **Add new tests** (tests after implementation):
    - Test `context-window-for-model` returns correct values for known models and fallback for unknown
    - Test `lookup-model-pricing` returns correct gpt-5.2-codex pricing
    - Test `make-task-cost-record-from-delta` works without tier/savings parameters
    - Test `compute-session-report` works without tier distribution
  - **Classify new test suites**: Add any new suites to `*safe-suites*` in suite.lisp (they're pure logic, no I/O)

  **Must NOT do**:
  - Do NOT modify tests unrelated to the tier system
  - Do NOT change the FiveAM test framework configuration
  - Do NOT make real network calls in tests (mock all external APIs)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: Mostly deletion of test code with some new tests to add
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (after Task 6)
  - **Blocks**: Task 8
  - **Blocked By**: Task 6

  **References**:

  **Pattern References**:
  - `tests/token-tracking-test.lisp:200-294` — `model-selector-suite` to delete
  - `tests/phase6-test.lisp:59-109` — Tier cost and benchmark tests to delete
  - `tests/anthropic-streaming-test.lisp:173-206` — Tests using `*latest-model-tiers*` for capability lookup
  - `tests/ollama-test.lisp:701-705` — Ollama tier membership test to delete
  - `tests/provisioning-test.lisp` — Entire file's tier-related content to delete
  - `tests/suite.lisp:59-66` — Suite classification lists (`*safe-suites*`, `*unsafe-suites*`)
  - `tests/creation-tools-test.lisp` — `model-selector-basic` test
  - `tests/sexp-tools-test.lisp` — `model-selector-basic` test

  **Test References**:
  - `tests/token-tracking-test.lisp` (non-tier tests) — Follow existing test patterns for new tests
  - `tests/suite.lisp` — Follow existing classification pattern for new test suites

  **WHY Each Reference Matters**:
  - Each test file: Must remove only tier-related tests, keep everything else
  - suite.lisp: Must remove deleted suites from classification lists or run-tests-parallel will warn
  - Existing test patterns: New tests should follow same FiveAM conventions

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: All tests pass after cleanup
    Tool: Bash (sbcl)
    Preconditions: All prior tasks completed, system compiles
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
             --eval '(asdf:test-system :sibyl)' \
             --eval '(quit)'
      2. Assert: exit code 0, all tests pass
    Expected Result: Test suite passes with no failures
    Evidence: Terminal output captured

  Scenario: No references to deleted tier symbols in test files
    Tool: Bash (grep)
    Preconditions: Test files modified
    Steps:
      1. Grep tests/ for "*latest-model-tiers*" — should return 0 matches
      2. Grep tests/ for "*default-model-tiers*" — should return 0 matches
      3. Grep tests/ for "adaptive-agent" — should return 0 matches
      4. Grep tests/ for "find-tier" — should return 0 matches
      5. Grep tests/ for "model-selector-suite" in suite.lisp — should return 0 matches
    Expected Result: No references to removed tier infrastructure in tests
    Evidence: Grep output

  Scenario: New tests exist and pass
    Tool: Bash (sbcl)
    Preconditions: New tests written
    Steps:
      1. Verify test for context-window-for-model exists
      2. Verify test for gpt-5.2-codex pricing exists
      3. Verify test for simplified task-cost-record exists
      4. Run all tests — all pass
    Expected Result: New tests cover the simplified functionality
    Evidence: Test output
  ```

  **Commit**: YES
  - Message: `test(llm): remove tier-related tests, add model registry and pricing tests`
  - Files: `tests/token-tracking-test.lisp`, `tests/phase6-test.lisp`, `tests/anthropic-streaming-test.lisp`, `tests/ollama-test.lisp`, `tests/provisioning-test.lisp`, `tests/creation-tools-test.lisp`, `tests/sexp-tools-test.lisp`, `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(quit)'`

---

- [x] 8. Final verification and compilation check

  **What to do**:
  - Run full system compilation: `(ql:quickload :sibyl :silent t)` — verify no warnings
  - Run full test suite: `(asdf:test-system :sibyl)` — verify all tests pass
  - Verify all acceptance criteria from Definition of Done
  - Run a quick smoke test of core functions:
    - `(sibyl.llm:lookup-model-pricing "gpt-5.2-codex")` → correct pricing
    - `(sibyl.llm:client-model (sibyl.llm:make-openai-client :api-key "x"))` → `"gpt-5.2-codex"`
    - `(sibyl.llm:context-window-for-model "gpt-5.2-codex")` → `400000`
    - `(sibyl.llm:context-window-for-model "claude-sonnet-4-6")` → `200000`
    - `(sibyl.llm:estimate-cost-usd "gpt-5.2-codex" :input-tokens 1000000 :output-tokens 100000)` → non-zero cost
  - Verify no orphaned symbols: grep packages.lisp for any symbols that reference deleted functions
  - Verify no compilation warnings about undefined functions

  **Must NOT do**:
  - Do NOT make any code changes in this task — verification only
  - If issues found, report them and let previous tasks be re-run

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Pure verification, no code changes
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 4 (final, sequential)
  - **Blocks**: None (final task)
  - **Blocked By**: Task 7

  **References**:

  **Documentation References**:
  - `AGENTS.md` — Build/test commands: `sbcl --eval '(ql:quickload :sibyl :silent t)'` and `(asdf:test-system :sibyl)`

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Full system compilation passes
    Tool: Bash (sbcl)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'
      2. Assert: exit code 0, no warnings
    Expected Result: Clean compilation
    Evidence: Terminal output

  Scenario: Full test suite passes
    Tool: Bash (sbcl)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
             --eval '(asdf:test-system :sibyl)' --eval '(quit)'
      2. Assert: exit code 0, all tests pass
    Expected Result: All tests green
    Evidence: Terminal output

  Scenario: All Definition of Done criteria verified
    Tool: Bash (sbcl)
    Steps:
      1. Run smoke tests for pricing, default model, context windows
      2. Verify no tier symbols exported
      3. Verify /tier-stats not in command registry
    Expected Result: All criteria pass
    Evidence: Terminal output for each check
  ```

  **Commit**: NO (verification only, no changes)

---

## Commit Strategy

| After Task | Message | Files | Verification |
|------------|---------|-------|--------------|
| 1 | `feat(llm): add gpt-5.2-codex pricing and set as default OpenAI model` | pricing.lisp, providers.lisp | quickload passes |
| 2 | `refactor(llm): migrate model-selector to flat model registry, remove tier classes` | model-selector.lisp | quickload passes |
| 3 | `refactor(llm): remove tier config keys, savings system, and A/B testing from metrics` | config.lisp, pricing.lisp, metrics.lisp | quickload passes |
| 4 | `refactor(agent): move cost-records to base agent, remove adaptive-agent from REPL` | core.lisp, repl.lisp | quickload passes |
| 5 | `refactor(repl): simplify /model to read-only, remove /tier-stats command` | repl.lisp | quickload passes |
| 6 | `refactor(llm): clean up tier exports, remove adaptive tools from multi-agent and builtin` | multi-agent.lisp, builtin.lisp, packages.lisp | quickload passes |
| 7 | `test(llm): remove tier-related tests, add model registry and pricing tests` | tests/*.lisp | test-system passes |

---

## Success Criteria

### Verification Commands
```bash
# Full compilation
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'
# Expected: exit 0, no warnings

# Full test suite
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(quit)'
# Expected: exit 0, all tests pass

# Pricing check
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "~a~%" (sibyl.llm:lookup-model-pricing "gpt-5.2-codex"))' --eval '(quit)'
# Expected: (:INPUT 1.75 :OUTPUT 14.0 :CACHE-WRITE 0.0 :CACHE-READ 0.175)

# Default model check
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "~a~%" (sibyl.llm:client-model (sibyl.llm:make-openai-client :api-key "x")))' --eval '(quit)'
# Expected: gpt-5.2-codex

# Context window check
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "~a~%" (sibyl.llm:context-window-for-model "gpt-5.2-codex"))' --eval '(quit)'
# Expected: 400000
```

### Final Checklist
- [x] All "Must Have" items present
- [x] All "Must NOT Have" guardrails respected
- [x] All tests pass
- [x] No compilation warnings
- [x] No orphaned exports
- [x] No references to `adaptive-agent`, `model-tier`, `*latest-model-tiers*`, `*default-model-tiers*` in source code
- [x] GPT-5.2-Codex is the default OpenAI model
- [x] `context-window-for-model` works from flat registry
