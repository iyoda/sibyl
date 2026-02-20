# Debug Logging Enhancement — Phase 1

## TL;DR

> **Quick Summary**: Sibyl のデバッグログ基盤を拡充する。相関ID/コンテキスト伝播とコンポーネント別ログレベル制御を `src/logging.lisp` に追加し、Agent ループと LLM 呼び出しのクリティカルパスにログポイントを配置する。
> 
> **Deliverables**:
> - 相関ID付きコンテキスト伝播システム（`with-request-context` マクロ）
> - コンポーネント別ログレベル制御（`*component-log-levels*`）
> - Agent ループのログポイント（`agent-step` 入口/出口）
> - LLM 呼び出しのログポイント（リクエスト/レスポンス前後）
> - CL `warn` → `sibyl.logging:log-warn` の統一（4箇所）
> - FiveAM テストスイート
> 
> **Estimated Effort**: Medium
> **Parallel Execution**: YES — 3 waves
> **Critical Path**: Task 1 → Task 2 → Tasks 3-6 (parallel) → Task 7

---

## Context

### Original Request
Sibyl のログ機能を拡充してデバッグを容易にしたい。デバッグログの設計から始める。

### Interview Summary
**Key Discussions**:
- **動機**: 全般的にログが足りない — 特定シナリオではなく全体的な情報不足
- **スコープ**: 基盤改善 + ログポイント追加の両方（ただしログポイントは段階的）
- **基盤優先機能**: 相関ID/コンテキスト伝播 + コンポーネント別ログレベル
- **コンテキスト設計**: リッチコンテキスト（request-id, agent名, step番号, ユーザー入力先頭）
- **実装方式**: Common Lisp の dynamic variable（`*current-request-context*`）
- **Phase 1 ログポイント**: Agent + LLM のクリティカルパスのみ
- **CL `warn` 統一**: 含める（4箇所）
- **テスト**: FiveAM テストあり

**Research Findings**:
- 既存ログ基盤は十分に機能的（6レベル、3フォーマット、色付き、97箇所で使用）
- `with-logging-context` と `log-execution-time` マクロは定義済みだが使用箇所ゼロ
- Config 統合済み（`SIBYL_LOG_LEVEL` → `*log-level*`）
- Agent にフックシステム存在（`:before-step`, `:after-step` 等）
- コンポーネント文字列: "mcp", "llm", "cache", "tools", "repl", "agent"

### Metis Review
**Identified Gaps** (addressed):
- **スレッドコンテキスト伝播**: `execute-tool-calls-parallel` で `bt:make-thread` が dynamic binding を継承しない → Task 1 で明示的に対応
- **CL `warn` 4箇所（3ではない）**: `client.lisp:349` も対象 → Task 5 で対応
- **`should-log-p` API 互換性**: signature 変更せず `log-message` 内でコンポーネントチェック → Task 1 で対応
- **ネストした `with-request-context`**: agent-step の再帰呼び出しで発生 → 内側で step-number を更新しつつ request-id を保持
- **JSON エスケープ**: コンテキスト値の特殊文字 → Task 1 で基本的なエスケープを追加
- **コンポーネント名の大小文字**: lowercase に正規化 → Task 1 で対応

---

## Work Objectives

### Core Objective
Sibyl のログシステムに相関ID/コンテキスト伝播とコンポーネント別ログレベル制御を追加し、Agent + LLM のクリティカルパスにデバッグログポイントを配置して、リクエスト単位のトレーサビリティを実現する。

### Concrete Deliverables
- `src/logging.lisp`: コンテキスト伝播 + コンポーネント別ログレベル機能
- `src/packages.lisp`: 新エクスポートの追加
- `src/config.lisp`: コンポーネント別ログレベルの config 統合
- `src/agent/core.lisp`: Agent ループのログポイント + CL `warn` 統一
- `src/agent/memory.lisp`: CL `warn` 統一
- `src/llm/client.lisp`: LLM 呼び出しログポイント + CL `warn` 統一
- `src/llm/providers.lisp`: LLM プロバイダのログポイント
- `src/tools/protocol.lisp`: スレッドコンテキスト伝播の対応
- `tests/logging-test.lisp`: FiveAM テストスイート
- `tests/suite.lisp`: テストスイート登録
- `sibyl.asd`: テストコンポーネント登録

### Definition of Done
- [x] `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'` がエラーなしで完了
- [x] `(asdf:test-system :sibyl)` で全テスト PASS（logging-test 含む）
- [x] コンテキスト付きログ出力が `:standard`, `:json`, `:structured` 全フォーマットで正常動作
- [x] コンポーネント別ログレベルが機能（例: "llm" だけ :debug にできる）
- [x] 97+ 既存ログ呼び出しが変更なしで動作（後方互換性）

### Must Have
- `*current-request-context*` dynamic variable によるコンテキスト伝播
- `with-request-context` マクロ
- `*component-log-levels*` によるコンポーネント別フィルタリング
- Agent ループのログポイント（step 入口/出口、ツール呼び出し検出、レスポンスタイプ）
- LLM 呼び出しのログポイント（リクエスト前、レスポンス後、トークン情報）
- CL `warn` → `log-warn` 統一（4箇所）
- FiveAM テスト

### Must NOT Have (Guardrails)
- **`log-message` の signature を変更しない** — `(level component message &rest args)` は不変。97+ 既存呼び出しは一切変更しない
- **`should-log-p` の既存 API を壊さない** — `(should-log-p level)` は引き続き有効
- **Phase 1 以外のサブシステムにログポイントを追加しない** — MCP, Tools, REPL, Cache へのログ追加は Phase 2 以降
- **ファイル出力、マクロ化（ゼロコスト）、構造化データフィールドを追加しない** — 将来の拡張
- **REPL コマンドを追加しない** — ログレベル制御は config のみ
- **コンテキストの plist フィールドを拡張しない** — `request-id`, `agent-name`, `step-number`, `user-input-prefix` の4つのみ
- **既存97+ ログ呼び出しを修正しない** — dynamic variable 経由で自動的にコンテキストが付与される
- **`with-logging-context` の export を削除しない** — 未使用でも public API

---

## Verification Strategy

> **ZERO HUMAN INTERVENTION** — ALL verification is agent-executed. No exceptions.

### Test Decision
- **Infrastructure exists**: YES (FiveAM, `tests/suite.lisp`)
- **Automated tests**: YES (Tests-after)
- **Framework**: FiveAM
- **Suite classification**: SAFE（pure logic、string-stream capture、no I/O、no global mutation）

### QA Policy
Every task MUST include agent-executed QA scenarios.
Evidence saved to `.sisyphus/evidence/task-{N}-{scenario-slug}.{ext}`.

- **Library/Module**: Use Bash (SBCL REPL) — Import, call functions, compare output
- **Build verification**: Use Bash — `sbcl --eval '(ql:quickload :sibyl :silent t)'`
- **Test verification**: Use Bash — `sbcl --eval '(asdf:test-system :sibyl)'`

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately — foundation):
├── Task 1: ログ基盤拡張 (logging.lisp + packages.lisp) [deep]
└── Task 2: Config 統合 (config.lisp) [quick]
    Note: Task 2 depends on Task 1's exports

Wave 2 (After Wave 1 — parallel log points + cleanup):
├── Task 3: Agent ループのログポイント (agent/core.lisp) [unspecified-high]
├── Task 4: LLM 呼び出しのログポイント (llm/client.lisp + providers.lisp) [unspecified-high]
├── Task 5: CL warn 統一 (4箇所) [quick]
└── Task 6: スレッドコンテキスト伝播 (tools/protocol.lisp) [unspecified-high]

Wave 3 (After Wave 2 — testing + verification):
└── Task 7: FiveAM テストスイート + 全体検証 [deep]

Wave FINAL (After ALL tasks — independent review, 4 parallel):
├── Task F1: Plan compliance audit (oracle)
├── Task F2: Code quality review (unspecified-high)
├── Task F3: Real manual QA (unspecified-high)
└── Task F4: Scope fidelity check (deep)

Critical Path: Task 1 → Task 2 → Tasks 3-6 (parallel) → Task 7 → F1-F4
Parallel Speedup: ~40% faster than sequential
Max Concurrent: 4 (Wave 2)
```

### Dependency Matrix

| Task | Depends On | Blocks | Wave |
|------|-----------|--------|------|
| 1 | — | 2, 3, 4, 5, 6, 7 | 1 |
| 2 | 1 | 3, 4, 5, 6, 7 | 1 |
| 3 | 1, 2 | 7 | 2 |
| 4 | 1, 2 | 7 | 2 |
| 5 | 1 | 7 | 2 |
| 6 | 1 | 7 | 2 |
| 7 | 3, 4, 5, 6 | F1-F4 | 3 |
| F1-F4 | 7 | — | FINAL |

### Agent Dispatch Summary

- **Wave 1**: 2 tasks — T1 → `deep`, T2 → `quick`
- **Wave 2**: 4 tasks — T3 → `unspecified-high`, T4 → `unspecified-high`, T5 → `quick`, T6 → `unspecified-high`
- **Wave 3**: 1 task — T7 → `deep`
- **FINAL**: 4 tasks — F1 → `oracle`, F2 → `unspecified-high`, F3 → `unspecified-high`, F4 → `deep`

---

## TODOs

- [x] 1. ログ基盤拡張 — コンテキスト伝播 + コンポーネント別ログレベル

  **What to do**:
  - `src/logging.lisp` に以下を追加:
    - `*current-request-context*` dynamic variable（デフォルト `nil`、plist 形式）
      - フィールド: `:request-id`, `:agent-name`, `:step-number`, `:user-input-prefix`
    - `with-request-context ((&key request-id agent-name step-number user-input) &body body)` マクロ
      - `request-id` は `(format nil "req-~a" (incf *request-counter*))` で自動生成（引数省略時）
      - `user-input` は先頭50文字に truncate して `:user-input-prefix` として保存
      - ネスト時: 内側の明示的指定値で上書き、未指定フィールドは外側を継承（`append` で後勝ち）
    - `*request-counter*` — アトミックカウンター（`sb-ext:atomic-incf` 使用）
    - `*component-log-levels*` hash-table（`test 'equal`、デフォルト空）
    - `component-log-level (component)` 関数 — コンポーネントの有効ログレベルを返す（未設定時は `*log-level*` にフォールバック）
    - `(setf component-log-level)` — コンポーネント別ログレベルを設定
    - `reset-component-log-levels ()` — ハッシュテーブルをクリア
  - `log-message` を修正:
    - コンポーネント名を `string-downcase` で正規化してからフィルタリング
    - `should-log-p` の代わりに `component-log-level` でフィルタリング判定（`should-log-p` 自体の signature は変更しない）
    - `*current-request-context*` が non-nil の場合、ログ出力にコンテキスト情報を付与:
      - `:standard` format: `"timestamp [LEVEL] component [req-xxx step:N] message"`
      - `:structured` format: `"[timestamp] LEVEL component [req-xxx agent:Name step:N] message"`
      - `:json` format: context の各フィールドを JSON のトップレベルキーとして追加
    - JSON 出力時にコンテキスト値の特殊文字（`"`, `\`, 改行）をエスケープ
  - `src/packages.lisp` の `sibyl.logging` パッケージに新エクスポートを追加:
    - `*current-request-context*`, `with-request-context`
    - `*component-log-levels*`, `component-log-level`, `reset-component-log-levels`
    - `*request-counter*`

  **Must NOT do**:
  - `log-message` の signature `(level component message &rest args)` を変更しない
  - `should-log-p` の signature `(level)` を変更しない（`log-message` 内部で独自にコンポーネントチェック）
  - `with-logging-context` のエクスポートを削除しない
  - 既存97+ ログ呼び出しを修正しない

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: ログ基盤のコア変更。後方互換性、スレッド安全性、複数フォーマット対応など複雑な設計判断が必要
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: ブラウザ操作不要
    - `git-master`: コミットは後続タスク

  **Parallelization**:
  - **Can Run In Parallel**: NO（基盤タスク、全後続タスクが依存）
  - **Parallel Group**: Wave 1 (sequential with Task 2)
  - **Blocks**: Tasks 2, 3, 4, 5, 6, 7
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `src/logging.lisp:1-121` — 現在のログシステム全体。`log-message` の実装（56-73行）が主な変更対象。`*log-level*`/`*log-stream*`/`*log-format*` の dynamic variable パターンを踏襲
  - `src/logging.lisp:101-109` — `with-logging-context` マクロの実装パターン。`with-request-context` の参考にする
  - `src/logging.lisp:28-36` — `log-level-priority` と `should-log-p` の実装。コンポーネント別フィルタリングはこの上に構築

  **API/Type References**:
  - `src/packages.lisp:62-84` — `sibyl.logging` の現在のエクスポートリスト。新シンボルを追加する場所
  - `src/util.lisp` — `timestamp-now` 関数。ログのタイムスタンプ生成に使用されている

  **External References**:
  - SBCL Manual: `sb-ext:atomic-incf` — スレッドセーフなカウンターインクリメント

  **WHY Each Reference Matters**:
  - `logging.lisp:56-73`: `log-message` の修正箇所。3つのフォーマット分岐をそれぞれ拡張する必要がある
  - `logging.lisp:101-109`: `with-request-context` マクロの設計パターン参照。dynamic variable の let-binding パターン
  - `packages.lisp:62-84`: エクスポートリストへの追加漏れ防止

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: コンテキスト付きログ出力（standard format）
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. sbcl --eval で以下を実行:
         (ql:quickload :sibyl :silent t)
         (let ((out (make-string-output-stream)))
           (let ((sibyl.logging:*log-stream* out)
                 (sibyl.logging:*log-level* :debug)
                 (sibyl.logging:*log-format* :standard))
             (sibyl.logging:with-request-context (:request-id "test-001" :agent-name "TestAgent" :step-number 3)
               (sibyl.logging:log-debug "agent" "Step started")))
           (let ((result (get-output-stream-string out)))
             (assert (search "test-001" result))
             (assert (search "agent" result))
             (assert (search "Step started" result))
             (format t "PASS: ~a~%" result)))
      2. 出力に "test-001", "agent", "Step started" が含まれることを確認
    Expected Result: "PASS:" に続いてコンテキスト付きログ行が表示される
    Failure Indicators: assert エラー、または "test-001" がログ行に含まれない
    Evidence: .sisyphus/evidence/task-1-context-standard.txt

  Scenario: コンテキストなし（後方互換性）
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. sbcl --eval で以下を実行:
         (ql:quickload :sibyl :silent t)
         (let ((out (make-string-output-stream)))
           (let ((sibyl.logging:*log-stream* out)
                 (sibyl.logging:*log-level* :debug)
                 (sibyl.logging:*current-request-context* nil))
             (sibyl.logging:log-debug "test" "No context message"))
           (let ((result (get-output-stream-string out)))
             (assert (search "No context message" result))
             (assert (null (search "req-" result)))
             (format t "PASS: ~a~%" result)))
      2. "No context message" が含まれ、"req-" が含まれないことを確認
    Expected Result: コンテキスト情報なしの従来通りのログ出力
    Failure Indicators: "req-" や "NIL" がログ行に混入
    Evidence: .sisyphus/evidence/task-1-no-context.txt

  Scenario: コンポーネント別ログレベルフィルタリング
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. sbcl --eval で以下を実行:
         (ql:quickload :sibyl :silent t)
         (let ((out (make-string-output-stream)))
           (let ((sibyl.logging:*log-stream* out)
                 (sibyl.logging:*log-level* :info))
             (setf (sibyl.logging:component-log-level "llm") :debug)
             (sibyl.logging:log-debug "llm" "Should appear")
             (sibyl.logging:log-debug "mcp" "Should NOT appear"))
           (let ((result (get-output-stream-string out)))
             (assert (search "Should appear" result))
             (assert (null (search "Should NOT appear" result)))
             (format t "PASS~%"))
           (sibyl.logging:reset-component-log-levels))
      2. "Should appear" のみが出力されることを確認
    Expected Result: "PASS" が表示。llm コンポーネントのみ debug レベルが出力される
    Failure Indicators: "Should NOT appear" も出力される、または "Should appear" が出力されない
    Evidence: .sisyphus/evidence/task-1-component-filter.txt

  Scenario: JSON フォーマットでのコンテキスト出力
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. sbcl --eval で以下を実行:
         (ql:quickload :sibyl :silent t)
         (let ((out (make-string-output-stream)))
           (let ((sibyl.logging:*log-stream* out)
                 (sibyl.logging:*log-level* :debug)
                 (sibyl.logging:*log-format* :json))
             (sibyl.logging:with-request-context (:request-id "json-test" :agent-name "Agent")
               (sibyl.logging:log-debug "llm" "Test msg")))
           (let ((result (get-output-stream-string out)))
             (assert (search "\"request-id\":\"json-test\"" result))
             (format t "PASS: ~a~%" result)))
      2. JSON 出力に "request-id":"json-test" が含まれることを確認
    Expected Result: 有効な JSON 文字列にコンテキストフィールドが含まれる
    Failure Indicators: JSON パースエラー、またはコンテキストフィールドが欠落
    Evidence: .sisyphus/evidence/task-1-json-context.txt
  ```

  **Evidence to Capture:**
  - [ ] task-1-context-standard.txt — standard フォーマットのコンテキスト付きログ
  - [ ] task-1-no-context.txt — コンテキストなし後方互換性
  - [ ] task-1-component-filter.txt — コンポーネント別フィルタリング
  - [ ] task-1-json-context.txt — JSON フォーマットのコンテキスト

  **Commit**: YES (group 1)
  - Message: `feat(logging): add context propagation and component-level filtering`
  - Files: `src/logging.lisp`, `src/packages.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

- [x] 2. Config 統合 — コンポーネント別ログレベルの設定

  **What to do**:
  - `src/config.lisp` の `*env-mappings*` に以下を追加:
    - `"SIBYL_LOG_LEVEL_LLM"` → `"log.level.llm"`
    - `"SIBYL_LOG_LEVEL_MCP"` → `"log.level.mcp"`
    - `"SIBYL_LOG_LEVEL_TOOLS"` → `"log.level.tools"`
    - `"SIBYL_LOG_LEVEL_AGENT"` → `"log.level.agent"`
    - `"SIBYL_LOG_LEVEL_REPL"` → `"log.level.repl"`
    - `"SIBYL_LOG_LEVEL_CACHE"` → `"log.level.cache"`
  - `load-config` 関数の末尾（`*log-level*` 同期の後）に、コンポーネント別ログレベルの同期処理を追加:
    - `"log.level.<component>"` の config 値が存在すれば `(setf (sibyl.logging:component-log-level "<component>") level-kw)` を呼ぶ
    - 同期前に `(sibyl.logging:reset-component-log-levels)` を呼んで前回の設定をクリア
    - コンポーネントリスト: `("llm" "mcp" "tools" "agent" "repl" "cache")` をループ

  **Must NOT do**:
  - 既存の環境変数マッピングを変更しない
  - `load-config` の既存ロジックの順序を変えない

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 既存パターン (`*env-mappings*` + `load-config` sync) の単純な拡張
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO（Task 1 の完了が必要）
  - **Parallel Group**: Wave 1 (after Task 1)
  - **Blocks**: Tasks 3, 4, 5, 6, 7
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/config.lisp:45-54` — `*env-mappings*` の既存パターン。新環境変数はこのリストに追加
  - `src/config.lisp:138-142` — `load-config` 内の `*log-level*` 同期処理。コンポーネント別同期はこの直後に追加

  **WHY Each Reference Matters**:
  - `config.lisp:45-54`: 環境変数マッピングの命名規則とデータ構造を正確に踏襲するため
  - `config.lisp:138-142`: 同期ロジックのパターン（`intern` + `assoc` による keyword 変換）を再利用するため

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: 環境変数からコンポーネント別ログレベル設定
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. sbcl --eval で以下を実行:
         (ql:quickload :sibyl :silent t)
         (sibyl.config:config-set "log.level.llm" "debug")
         (sibyl.config:config-set "log.level.mcp" "trace")
         (sibyl.config:load-config)
         (format t "llm: ~a~%" (sibyl.logging:component-log-level "llm"))
         (format t "mcp: ~a~%" (sibyl.logging:component-log-level "mcp"))
         (format t "tools: ~a~%" (sibyl.logging:component-log-level "tools"))
      2. llm が :debug、mcp が :trace、tools が *log-level* のフォールバック値であることを確認
    Expected Result: llm=DEBUG, mcp=TRACE, tools=INFO (global default)
    Failure Indicators: コンポーネントレベルが設定されない、またはフォールバックが機能しない
    Evidence: .sisyphus/evidence/task-2-config-integration.txt

  Scenario: load-config で前回の設定がクリアされる
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. コンポーネントレベルを手動設定後、load-config を呼び出す
      2. 手動設定がクリアされ、config の値のみが反映されることを確認
    Expected Result: 手動設定がリセットされる
    Failure Indicators: 古い手動設定が残る
    Evidence: .sisyphus/evidence/task-2-config-reset.txt
  ```

  **Commit**: YES (group 1)
  - Message: `feat(logging): add context propagation and component-level filtering`
  - Files: `src/config.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

- [x] 3. Agent ループのログポイント

  **What to do**:
  - `src/agent/core.lisp` の `agent-step` メソッドに以下のログポイントを追加:
    - **入口**: `with-request-context` でコンテキストをバインド
      - `request-id`: 自動生成（`agent-run` 開始時に1回、再帰 `agent-step` では同じ ID を継承）
      - `agent-name`: `(agent-name agent)`
      - `step-number`: `(agent-step-count agent)`
      - `user-input`: ユーザー入力文字列
    - **入口ログ**: `(log-debug "agent" "agent-step: step=~a/~a input-length=~a" step-count max-steps (length input))`
    - **LLM 呼び出し前**: `(log-debug "agent" "Calling LLM: context-messages=~a tools=~a" message-count tool-count)`
    - **レスポンス分岐**: `(log-info "agent" "LLM response: tool-calls=~a text-length=~a" (length tool-calls) (length text))`
    - **再帰前**: `(log-debug "agent" "Recursive step: step=~a" new-step-count)`
    - **ステップ上限**: `(log-warn "agent" "Max steps reached: ~a/~a" step-count max-steps)`
    - **出口**: `(log-debug "agent" "agent-step complete: step=~a response-length=~a" step-count (length response))`
  - `agent-run` メソッドの冒頭で `with-request-context` を1回バインドし、配下の全 `agent-step` 再帰呼び出しで同じ `request-id` を共有する

  **Must NOT do**:
  - 既存のフックシステム (`:before-step`, `:after-step` 等) を変更しない
  - agent-step の制御フローや戻り値を変更しない
  - 既存のログ呼び出しを修正しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: agent-step の構造理解と適切なログポイント配置が必要。制御フローを壊さない注意が必要
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 4, 5, 6)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `src/agent/core.lisp:1-80` — Agent クラス定義、`agent-step-count`, `agent-max-steps`, `agent-name` のアクセサ
  - `src/agent/core.lisp:222+` — `agent-step` メソッドの実装。ここにログポイントを挿入。制御フロー（tool-calls 分岐、再帰呼び出し）を理解してから修正すること
  - `src/agent/core.lisp:170+` — `run-hook` の実装。フックの呼び出し位置を参考に、ログポイントの配置場所を決定

  **API/Type References**:
  - `src/logging.lisp` — `with-request-context` マクロ（Task 1 で追加）の使用方法

  **WHY Each Reference Matters**:
  - `core.lisp:222+`: agent-step の制御フロー全体を理解しないと、ログポイントを間違った場所に置いてしまう。特に再帰呼び出しの前後と tool-calls の分岐が重要
  - `core.lisp:1-80`: `agent-run` でコンテキストをバインドするため、agent クラスのアクセサ名を正確に知る必要がある

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: agent-step にコンテキスト付きログが出力される
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み、*log-level* が :debug
    Steps:
      1. sbcl --eval で以下を実行:
         (ql:quickload :sibyl :silent t)
         (let ((sibyl.logging:*log-level* :debug)
               (sibyl.logging:*log-format* :standard))
           ;; agent-step のログ出力を grep で確認
           ;; agent-run のコード内に with-request-context があることを確認
           (let ((source (uiop:read-file-string "src/agent/core.lisp")))
             (assert (search "with-request-context" source))
             (assert (search "log-debug" source))
             (assert (search "log-info" source))
             (format t "PASS: agent logging code present~%")))
      2. agent/core.lisp に with-request-context と log-debug/log-info 呼び出しが存在することを確認
    Expected Result: "PASS" が表示
    Failure Indicators: with-request-context や log-debug が見つからない
    Evidence: .sisyphus/evidence/task-3-agent-logging.txt

  Scenario: agent-step の制御フローが壊れていない
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. sbcl --eval で以下を実行:
         (ql:quickload :sibyl :silent t)
         ;; agent-step がコンパイルエラーなくロードされることを確認
         (assert (fboundp 'sibyl.agent::agent-step))
         (format t "PASS: agent-step is defined~%")
    Expected Result: "PASS" が表示
    Failure Indicators: コンパイルエラー、未定義関数
    Evidence: .sisyphus/evidence/task-3-agent-integrity.txt
  ```

  **Commit**: YES (group 2)
  - Message: `feat(logging): add debug log points to agent loop and LLM calls`
  - Files: `src/agent/core.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

- [x] 4. LLM 呼び出しのログポイント

  **What to do**:
  - `src/llm/client.lisp` に以下のログポイントを追加:
    - `call-llm` 関数の入口: `(log-debug "llm" "call-llm: messages=~a tools=~a" (length messages) (length tools))`
    - HTTP POST 前（`%post-json`, `%post-stream`）: リクエストボディのサイズをログ
    - HTTP レスポンス後: ステータスコード、レスポンスサイズをログ
    - リトライ発生時: 既存の `log-warn` は維持、追加情報なし
  - `src/llm/providers.lisp` に以下のログポイントを追加:
    - `complete` / `complete-with-tools` メソッドの入口: モデル名、ツール数、ストリーミング有無
    - レスポンスパース後: トークン使用量（input/output/thinking tokens）をログ
      - `(log-info "llm" "Tokens: input=~a output=~a thinking=~a" in out thinking)`
    - Anthropic/OpenAI それぞれのメソッドに追加
  - 既存のログ呼び出しは変更しない（追加のみ）

  **Must NOT do**:
  - 既存のログ呼び出し（10+ in client.lisp, 8+ in providers.lisp）を修正しない
  - LLM レスポンスのフルボディをログしない（サイズのみ）
  - API キーをログに出力しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: LLM クライアントの HTTP 層とプロバイダ層の両方を理解して適切な位置にログを配置する必要がある
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 5, 6)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `src/llm/client.lisp:150-170` — `call-with-retry` のリトライログパターン。既存の `log-warn` の位置と形式を参考にする
  - `src/llm/client.lisp:195-240` — `%post-json` の HTTP POST 実装。リクエスト/レスポンスのログポイント位置
  - `src/llm/providers.lisp:460-500` — Anthropic `complete-with-tools` の実装。トークン抽出位置の参考

  **API/Type References**:
  - `src/llm/token-tracker.lisp` — `tracker-add-usage` の呼び出し位置。トークン情報をログする場所の近くにある

  **WHY Each Reference Matters**:
  - `client.lisp:195-240`: HTTP 層のログは `%post-json` 内に配置する必要があるが、既存ログとの重複を避ける
  - `providers.lisp:460-500`: トークン情報は usage ハッシュからの抽出後にログするため、パース処理を理解する必要がある

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: LLM クライアントにログポイントが存在する
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. client.lisp と providers.lisp のソースを検査:
         (let ((client-src (uiop:read-file-string "src/llm/client.lisp"))
               (prov-src (uiop:read-file-string "src/llm/providers.lisp")))
           (assert (search "call-llm" client-src))
           (assert (search "Tokens:" prov-src))
           (format t "PASS: LLM logging points present~%"))
    Expected Result: "PASS" が表示
    Failure Indicators: ログポイントが見つからない
    Evidence: .sisyphus/evidence/task-4-llm-logging.txt

  Scenario: API キーがログに含まれない
    Tool: Bash (grep)
    Preconditions: Task 4 完了後
    Steps:
      1. src/llm/ 配下の全ファイルで log-debug/log-info 呼び出しに api-key が含まれないことを確認
      2. grep -n "api-key\|api_key\|API.KEY" src/llm/client.lisp src/llm/providers.lisp の log- 呼び出し行を検査
    Expected Result: log 呼び出し行に API キー参照がない
    Failure Indicators: log 行に api-key 参照が見つかる
    Evidence: .sisyphus/evidence/task-4-no-api-key.txt
  ```

  **Commit**: YES (group 2)
  - Message: `feat(logging): add debug log points to agent loop and LLM calls`
  - Files: `src/llm/client.lisp`, `src/llm/providers.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

- [x] 5. CL `warn` → `sibyl.logging:log-warn` 統一

  **What to do**:
  - 以下4箇所の CL 標準 `warn` 呼び出しを `sibyl.logging:log-warn` に変更:
    1. `src/agent/core.lisp:175` — `run-hook` 内のフックエラー: `(warn ...)` → `(log-warn "agent" "Hook ~a error: ~a" hook-name e)`
    2. `src/agent/memory.lisp:145` — LLM compaction fallback: `(warn ...)` → `(log-warn "agent" "Memory compaction failed: ~a" e)`
    3. `src/llm/pricing.lisp:43` — unknown model pricing: `(warn ...)` → `(log-warn "llm" "Unknown model pricing: ~a, using fallback" model)`
    4. `src/llm/client.lisp:349` — NDJSON parse failure: `(warn ...)` → `(log-warn "llm" "NDJSON parse error: ~a" e)`
  - 各箇所で `sibyl.logging` パッケージが `:use` されていることを確認（`packages.lisp` で確認済み: `sibyl.agent` は `sibyl.logging` を use している）

  **Must NOT do**:
  - CL condition system (`handler-bind` / `handler-case` for `warning`) に依存しているコードを壊さない → 確認: これらの `warn` を catch しているコードがないことを `lsp_find_references` で確認
  - 他のファイルの `warn` 呼び出しを変更しない

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 4箇所の単純な置換。各箇所の文脈を確認して適切なコンポーネント名とメッセージを選ぶだけ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 4, 6)
  - **Blocks**: Task 7
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/agent/core.lisp:174-176` — `run-hook` 内の `warn` 呼び出し。周囲のコンテキスト（handler-case）を理解して置換
  - `src/agent/memory.lisp:143-146` — memory compaction 内の `warn`。エラーメッセージのフォーマットを維持
  - `src/llm/pricing.lisp:41-45` — pricing lookup の `warn`。fallback ロジックのコンテキストを理解
  - `src/llm/client.lisp:347-351` — NDJSON パース失敗の `warn`。non-fatal エラーとしてのコンテキスト

  **WHY Each Reference Matters**:
  - 各箇所の周囲のコード（特に handler-case/handler-bind）を理解しないと、CL condition system との連携を壊す可能性がある

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: CL warn が残っていないことの確認
    Tool: Bash (grep)
    Preconditions: Task 5 完了後
    Steps:
      1. grep -n "(warn " src/agent/core.lisp src/agent/memory.lisp src/llm/pricing.lisp src/llm/client.lisp を実行
      2. 移行対象の4行に CL `warn` が残っていないことを確認
    Expected Result: 移行対象行に (warn が見つからない（他の正当な warn は残ってよい）
    Failure Indicators: 移行対象行に (warn が残っている
    Evidence: .sisyphus/evidence/task-5-warn-migration.txt

  Scenario: ログ出力が正しいコンポーネント名を持つ
    Tool: Bash (grep)
    Preconditions: Task 5 完了後
    Steps:
      1. 置換後のコードを確認:
         - core.lisp: log-warn "agent" ...
         - memory.lisp: log-warn "agent" ...
         - pricing.lisp: log-warn "llm" ...
         - client.lisp: log-warn "llm" ...
    Expected Result: 各ファイルで適切なコンポーネント名が使用されている
    Failure Indicators: コンポーネント名が間違っている
    Evidence: .sisyphus/evidence/task-5-component-names.txt
  ```

  **Commit**: YES (group 3)
  - Message: `refactor(logging): migrate CL warn to sibyl.logging`
  - Files: `src/agent/core.lisp`, `src/agent/memory.lisp`, `src/llm/pricing.lisp`, `src/llm/client.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

- [x] 6. スレッドコンテキスト伝播 — 並列ツール実行時の対応

  **What to do**:
  - `src/tools/protocol.lisp` の `execute-tool-calls-parallel` 関数を修正:
    - ツールスレッド生成前に `*current-request-context*` をキャプチャ
    - 各スレッドのラムダ内で `let` バインディングにより再バインド
    - パターン:
      ```lisp
      (let ((ctx sibyl.logging:*current-request-context*))
        (bt:make-thread
          (lambda ()
            (let ((sibyl.logging:*current-request-context* ctx))
              ;; ... tool execution ...
              ))))
      ```
  - SBCL では `bt:make-thread` が dynamic binding を継承しないため、明示的なキャプチャ&リバインドが必須

  **Must NOT do**:
  - `execute-tool-calls-parallel` の既存ロジック（スレッド生成、結果収集、エラー処理）を変更しない
  - 他のスレッド関連コードを修正しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: スレッドと dynamic variable の相互作用を正しく理解する必要がある。間違えるとデータ競合や missing context が発生
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 4, 5)
  - **Blocks**: Task 7
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/tools/protocol.lisp` — `execute-tool-calls-parallel` の実装。`bt:make-thread` の呼び出し位置を特定し、その直前でコンテキストをキャプチャ

  **External References**:
  - SBCL Manual: dynamic variable scoping in threads — `bt:make-thread` はデフォルトで dynamic binding を継承しない

  **WHY Each Reference Matters**:
  - `protocol.lisp` の `execute-tool-calls-parallel`: スレッド生成の正確な位置を知らないと、コンテキストキャプチャを正しい場所に配置できない

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: コンテキストキャプチャコードが存在する
    Tool: Bash (sbcl)
    Preconditions: Task 6 完了後
    Steps:
      1. src/tools/protocol.lisp のソースを検査:
         (let ((src (uiop:read-file-string "src/tools/protocol.lisp")))
           (assert (search "*current-request-context*" src))
           (format t "PASS: context capture code present~%"))
    Expected Result: "PASS" が表示
    Failure Indicators: *current-request-context* がソースに見つからない
    Evidence: .sisyphus/evidence/task-6-thread-context.txt

  Scenario: 並列ツール実行の既存機能が壊れていない
    Tool: Bash (sbcl)
    Preconditions: sibyl がロード済み
    Steps:
      1. execute-tool-calls-parallel が定義されていることを確認:
         (assert (fboundp 'sibyl.tools::execute-tool-calls-parallel))
         (format t "PASS~%")
    Expected Result: "PASS" が表示
    Failure Indicators: コンパイルエラー、未定義関数
    Evidence: .sisyphus/evidence/task-6-parallel-integrity.txt
  ```

  **Commit**: YES (group 2)
  - Message: `feat(logging): add debug log points to agent loop and LLM calls`
  - Files: `src/tools/protocol.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(quit)'`

- [x] 7. FiveAM テストスイート + 全体検証

  **What to do**:
  - `tests/logging-test.lisp` を新規作成。以下のテストケースを含む:
    - **コンテキスト伝播テスト**:
      - `with-request-context` でバインドしたコンテキストが `log-message` の出力に含まれることを検証
      - コンテキストなし（`nil`）の場合に従来通りのフォーマットで出力されることを検証
      - ネストした `with-request-context` で内側の値が優先されることを検証
      - `request-id` の自動生成（`*request-counter*`）が動作することを検証
    - **コンポーネント別ログレベルテスト**:
      - 特定コンポーネントに `:debug` を設定し、グローバルは `:info` の場合、そのコンポーネントの debug ログのみ出力されることを検証
      - 未設定コンポーネントがグローバルレベルにフォールバックすることを検証
      - `reset-component-log-levels` で全クリアされることを検証
      - コンポーネント名の大小文字正規化（"LLM" と "llm" が同じに扱われる）を検証
    - **JSON フォーマットテスト**:
      - コンテキスト付き JSON 出力が valid な JSON 構造を持つことを検証
      - 特殊文字（`"`, `\`, 改行）がエスケープされることを検証
    - **後方互換性テスト**:
      - 既存の `log-message` 呼び出しパターンが変更なしで動作することを検証
      - `should-log-p` が従来の `(should-log-p level)` 呼び出しで動作することを検証
  - テスト内で string-stream を使ってログ出力をキャプチャするパターン:
    ```lisp
    (let ((out (make-string-output-stream)))
      (let ((*log-stream* out) (*log-level* :debug))
        (log-debug "test" "message"))
      (get-output-stream-string out))
    ```
  - `tests/suite.lisp` に `logging-test` スイートを `*safe-suites*` に追加
  - `sibyl.asd` の `:sibyl/tests` コンポーネントに `"tests/logging-test"` を追加
  - 全テスト実行: `(asdf:test-system :sibyl)` で全 PASS を確認

  **Must NOT do**:
  - 外部 API を呼ぶテストを書かない（SAFE suite 分類のため）
  - グローバル状態を永続的に変更するテストを書かない（各テスト後にクリーンアップ）
  - 他のテストファイルを修正しない

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: テストケースの網羅性と正確性が重要。エッジケース（ネスト、正規化、エスケープ）のテストが複雑
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO（全 Wave 2 タスクの完了が必要）
  - **Parallel Group**: Wave 3 (sequential)
  - **Blocks**: F1-F4
  - **Blocked By**: Tasks 3, 4, 5, 6

  **References**:

  **Pattern References**:
  - `tests/suite.lisp` — `*safe-suites*` と `*unsafe-suites*` のリスト定義。新スイートを `*safe-suites*` に追加
  - `tests/cache-test.lisp` — string-stream を使ったテストパターンの参考
  - `sibyl.asd` — テストシステムの `:components` リスト。新テストファイルを追加

  **Test References**:
  - `tests/logging-test.lisp` は新規作成

  **WHY Each Reference Matters**:
  - `suite.lisp`: SAFE/UNSAFE 分類を正しく行わないと、並列テスト実行で問題が発生する
  - `sibyl.asd`: テストファイルを ASDF に登録しないとロードされない

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: 全テスト PASS
    Tool: Bash (sbcl)
    Preconditions: 全 Task 完了後
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
      2. テスト結果を確認: 0 failures, 0 errors
    Expected Result: 全テスト PASS、exit code 0
    Failure Indicators: テスト失敗、コンパイルエラー
    Evidence: .sisyphus/evidence/task-7-all-tests.txt

  Scenario: logging-test スイートが SAFE に分類されている
    Tool: Bash (grep)
    Preconditions: Task 7 完了後
    Steps:
      1. grep "logging" tests/suite.lisp で *safe-suites* に含まれることを確認
    Expected Result: logging 関連のスイートが *safe-suites* リストに含まれる
    Failure Indicators: *unsafe-suites* に含まれている、またはどちらにも含まれていない
    Evidence: .sisyphus/evidence/task-7-suite-classification.txt

  Scenario: システムがクリーンにロードされる
    Tool: Bash (sbcl)
    Preconditions: 全 Task 完了後
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "OK~%")' --quit
    Expected Result: "OK" が表示され exit code 0
    Failure Indicators: ロードエラー、warning
    Evidence: .sisyphus/evidence/task-7-clean-load.txt
  ```

  **Commit**: YES (group 4)
  - Message: `test(logging): add FiveAM test suite for logging enhancements`
  - Files: `tests/logging-test.lisp`, `tests/suite.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(quit)'`

---

## Final Verification Wave

- [x] F1. **Plan Compliance Audit** — `oracle`
  Read the plan end-to-end. For each "Must Have": verify implementation exists (read file, run command). For each "Must NOT Have": search codebase for forbidden patterns — reject with file:line if found. Check evidence files exist in .sisyphus/evidence/. Compare deliverables against plan.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | Tasks [N/N] | VERDICT: APPROVE/REJECT`

- [x] F2. **Code Quality Review** — `unspecified-high`
  Run `sbcl --eval '(ql:quickload :sibyl :silent t)'` + `(asdf:test-system :sibyl)`. Review all changed files for: unused imports, empty catches, debug output left in prod code, incorrect package references. Check AI slop: excessive comments, over-abstraction, generic variable names.
  Output: `Build [PASS/FAIL] | Tests [N pass/N fail] | Files [N clean/N issues] | VERDICT`

- [x] F3. **Real Manual QA** — `unspecified-high`
  Start from clean state. Execute EVERY QA scenario from EVERY task — follow exact steps, capture evidence. Test cross-task integration (context flows from agent through LLM calls, component filtering works with context). Test edge cases: empty context, nested context, component not in hash table. Save to `.sisyphus/evidence/final-qa/`.
  Output: `Scenarios [N/N pass] | Integration [N/N] | Edge Cases [N tested] | VERDICT`

- [x] F4. **Scope Fidelity Check** — `deep`
  For each task: read "What to do", read actual diff (git log/diff). Verify 1:1 — everything in spec was built, nothing beyond spec was built. Check "Must NOT do" compliance (no log points outside Agent+LLM, no file output, no REPL commands, etc.). Detect cross-task contamination. Flag unaccounted changes.
  Output: `Tasks [N/N compliant] | Contamination [CLEAN/N issues] | Unaccounted [CLEAN/N files] | VERDICT`

---

## Commit Strategy

- **Commit 1**: `feat(logging): add context propagation and component-level filtering` — logging.lisp, packages.lisp, config.lisp
- **Commit 2**: `feat(logging): add debug log points to agent loop and LLM calls` — agent/core.lisp, llm/client.lisp, llm/providers.lisp, tools/protocol.lisp
- **Commit 3**: `refactor(logging): migrate CL warn to sibyl.logging` — agent/core.lisp, agent/memory.lisp, llm/client.lisp, llm/pricing.lisp
- **Commit 4**: `test(logging): add FiveAM test suite for logging enhancements` — tests/logging-test.lisp, tests/suite.lisp, sibyl.asd

---

## Success Criteria

### Verification Commands
```bash
# System loads without errors
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "OK~%")' --quit
# Expected: "OK" printed, exit code 0

# All tests pass
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
# Expected: 0 failures, 0 errors
```

### Final Checklist
- [x] All "Must Have" present
- [x] All "Must NOT Have" absent
- [x] All tests pass
- [x] 97+ existing log calls unchanged and working
- [x] Context appears in log output when bound
- [x] Context absent from log output when not bound (backward compatible)
- [x] Component-level filtering works independently of global level
- [x] No CL `warn` calls remain in migrated locations
