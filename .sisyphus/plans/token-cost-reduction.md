# Token Cost Reduction: Comprehensive Optimization

## TL;DR

> **Quick Summary**: Sibylのトークン消費コストを包括的に削減する。プロンプトキャッシング、トークン追跡、ツール最適化、モデルルーティング、会話圧縮、Extended Thinking基盤の6施策を段階的に実装。
>
> **Deliverables**:
> - APIレスポンスからのトークン使用量追跡・可視化（`/tokens` REPLコマンド）
> - Anthropicプロンプトキャッシング（`cache_control`対応）
> - タスクベースのツールスキーマ選択的送信
> - 既存`model-selector`のエージェントループ統合
> - LLMベースの会話履歴圧縮（軽量モデル使用）
> - Extended Thinking `thinking`ブロックのパース基盤
>
> **Estimated Effort**: Large
> **Parallel Execution**: YES - 5 waves
> **Critical Path**: Task 1 → Task 2 → Task 3 → Task 5

---

## Context

### Original Request
トークンの消費コスト削減を包括的に検討・実装したい。

### Interview Summary
**Key Discussions**:
- 全6施策を1つのプランで段階的に実装する
- Batch APIは不要（REPL対話が主用途）
- Extended Thinkingは将来導入予定、基盤のみ今回実装
- TDDで品質担保（FiveAM）

**Research Findings**:
- **プロンプトキャッシング**: キャッシュヒット時入力トークン90%オフ。Sonnet最低1024トークン。`cache_control: {"type": "ephemeral"}` で5分TTL
- **全ツール毎回送信**: 1ツール≒393トークン。8+ツールで3000+トークン/リクエスト浪費
- **システムプロンプト**: ~2000トークンが毎リクエスト送信。`memory-context-window`で動的要約と結合 → キャッシュ無効化の原因
- **APIレスポンスのusageフィールド**: `input_tokens`, `output_tokens`, `cache_creation_input_tokens`, `cache_read_input_tokens`を未読取
- **model-selector**: Light(0.2x)/Medium(1.0x)/Heavy(3.0x)のティア定義済みだがエージェントループ未接続

### Metis Review
**Identified Gaps** (addressed):
- **システムプロンプトの動的要約結合がキャッシュを破壊**: → Task 2でstatic/dynamicブロック分離を実装
- **`complete`/`complete-with-tools`の戻り値変更**: → `(values message usage-plist)`パターンで既存呼び出し元を壊さない
- **ストリーミングとノンストリーミングの両パス**: → 両方でusage抽出を実装
- **OpenAI側は変更しない**: → Anthropic固有機能は`defmethod`特殊化で隔離
- **テスト内でのLLM呼び出し禁止**: → モックレスポンスでテスト、実API不要
- **MCP動的ツール登録**: → ツールキャッシュの無効化メカニズムを含める

---

## Work Objectives

### Core Objective
Sibylのトークン消費コストを50-80%削減する。測定基盤 → キャッシング → 最適化の順で、各施策が前の施策の効果を検証できる構造で実装。

### Concrete Deliverables
- `parse-anthropic-response`が`usage` plistを第2値として返す
- `/tokens` REPLコマンドでセッション累計トークン数を表示
- Anthropicリクエストに`cache_control`が含まれ、キャッシュヒット率を追跡
- `tools-as-schema`がカテゴリフィルタを受け取り、関連ツールのみ返す
- `memory-compact`がLLMベースで200トークン以内の要約を生成
- `start-repl`に`:use-model-selector t`オプション追加
- `thinking`コンテンツブロックのパースとmessage構造体への格納

### Definition of Done
- [x] `(asdf:test-system :sibyl)` — 全テスト通過 (1553 checks, 0 failures)
- [x] `/tokens` コマンドで入力/出力/キャッシュトークン数が表示される
- [x] マルチターン会話でキャッシュヒットが発生し、`cache_read_input_tokens > 0` (cache_control実装済み)
- [x] 5ツール登録状態で、simpleクエリに送信されるツール数 < 全登録数 (43ツール中、filterで4/12/7等に絞り込み可能)

### Must Have
- トークン使用量のリクエスト毎の追跡
- プロンプトキャッシング（system prompt + tool schemas）
- 既存テスト全通過
- 既存の`complete`/`complete-with-tools`呼び出し元の互換性維持

### Must NOT Have (Guardrails)
- `*default-system-prompt*`のテキスト内容変更（テストが検証している）
- ツール結果の圧縮（サイズ追跡のみ、圧縮は今回スコープ外）
- OpenAI provider側のキャッシング変更（Anthropic固有）
- `start-repl`のシグネチャ変更（キーワード引数追加のみ）
- テストスイート内での実API呼び出し
- Batch API対応

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks in this plan MUST be verifiable WITHOUT any human action.

### Test Decision
- **Infrastructure exists**: YES (FiveAM, tests/ ディレクトリ)
- **Automated tests**: TDD (RED-GREEN-REFACTOR)
- **Framework**: FiveAM
- **Test command**: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

### TDD Workflow

Each TODO follows RED-GREEN-REFACTOR:

1. **RED**: Write failing test first
   - Test file: `tests/{area}-test.lisp`
   - Command: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`
   - Expected: FAIL (test exists, implementation doesn't)
2. **GREEN**: Implement minimum code to pass
   - Command: same
   - Expected: PASS
3. **REFACTOR**: Clean up while keeping green
   - Command: same
   - Expected: PASS (still)

### Agent-Executed QA Scenarios (MANDATORY — ALL tasks)

**Verification Tool by Deliverable Type:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| **REPL対話** | interactive_bash (tmux) | REPLを起動し、コマンドを送信し、出力を検証 |
| **API通信** | Bash (SBCL REPL) | Lispフォームを評価し、結果を検証 |
| **ユニットテスト** | Bash (sbcl --eval) | テストスイート実行、結果パース |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately):
└── Task 1: Token Usage Tracking (foundation)

Wave 2 (After Wave 1):
├── Task 2: System Prompt Restructuring
└── Task 7: Extended Thinking Foundation

Wave 3 (After Wave 2):
├── Task 3: Prompt Caching
└── Task 6: Model Auto-Routing

Wave 4 (After Wave 3):
├── Task 4: Selective Tool Sending
└── Task 5: Conversation Compression

Wave 5 (After Wave 4):
└── Task 8: Integration Testing & REPL Command

Critical Path: Task 1 → Task 2 → Task 3 → Task 5
Parallel Speedup: ~35% faster than sequential
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 2, 3, 4, 5, 6, 7 | None (foundation) |
| 2 | 1 | 3, 5 | 7 |
| 3 | 1, 2 | 5 | 6 |
| 4 | 1 | 8 | 5 |
| 5 | 2, 3 | 8 | 4 |
| 6 | 1 | 8 | 3 |
| 7 | 1 | 8 | 2 |
| 8 | 3, 4, 5, 6, 7 | None (final) | None |

### Agent Dispatch Summary

| Wave | Tasks | Recommended Agents |
|------|-------|-------------------|
| 1 | 1 | task(category="unspecified-high", load_skills=[], run_in_background=false) |
| 2 | 2, 7 | dispatch parallel after Wave 1 |
| 3 | 3, 6 | dispatch parallel after Wave 2 |
| 4 | 4, 5 | dispatch parallel after Wave 3 |
| 5 | 8 | final integration task |

---

## TODOs

- [x] 1. Token Usage Tracking — 測定基盤の構築

  **What to do**:
  - `parse-anthropic-response` (providers.lisp:120-144) を修正: レスポンスの `usage` フィールドから `input_tokens`, `output_tokens`, `cache_creation_input_tokens`, `cache_read_input_tokens` を抽出し、`(values message usage-plist)` で返す
  - `parse-anthropic-sse-events` (providers.lisp:146-171) を修正: `message_start` イベントで `usage` を、`message_delta` イベントで最終 `usage` を抽出
  - `complete-anthropic-streaming` (providers.lisp:173-245) を修正: SSEイベントからusageを収集し `(values message usage-plist)` で返す
  - `complete` / `complete-with-tools` のAnthropicメソッド (providers.lisp) を修正: `(values message usage-plist)` を返す
  - `token-tracker` 構造体を新規作成 (client.lisp または新ファイル `src/llm/token-tracker.lisp`): セッション累計の `input-tokens`, `output-tokens`, `cache-read-tokens`, `cache-write-tokens` を保持
  - `agent` クラス (agent/core.lisp) に `token-tracker` スロットを追加
  - `agent-step` (agent/core.lisp:139-183) を修正: `multiple-value-bind` で usage を受け取り、tracker に累積
  - テストをTDDで作成:
    - RED: モックレスポンス（`usage`フィールド付きhash-table）を `parse-anthropic-response` に渡し、第2値にusage plistが返ることを検証
    - RED: ストリーミングSSEイベントのモックで `message_start` / `message_delta` のusage抽出を検証
    - RED: `token-tracker` の累積ロジックを検証
    - GREEN: 各関数を実装してテスト通過
    - REFACTOR: usage抽出ロジックを共通ヘルパーに分離

  **Must NOT do**:
  - OpenAI providerのメソッドは変更しない（将来のタスクとして残す）
  - `complete` / `complete-with-tools` のシグネチャ（引数）は変更しない
  - 実APIへのリクエストをテストに含めない
  - REPLコマンドの追加は Task 8 で行う

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: 複数ファイルにまたがるリファクタリングで、戻り値パターンの変更は慎重な設計が必要
  - **Skills**: []
    - 外部スキル不要。LSP / AST-grep で参照箇所を確認しながら進める

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 1 (solo)
  - **Blocks**: Tasks 2, 3, 4, 5, 6, 7
  - **Blocked By**: None (foundation task)

  **References**:

  **Pattern References**:
  - `src/llm/providers.lisp:120-144` — `parse-anthropic-response`: 現在 `content` ブロックのみ抽出。ここに `usage` フィールド読み取りを追加
  - `src/llm/providers.lisp:146-171` — `parse-anthropic-sse-events`: 現在 `message_start` を無視。ここで usage を含む message_start/message_delta を処理
  - `src/llm/providers.lisp:173-245` — `complete-anthropic-streaming`: SSEコールバック内でusageを収集する必要あり。`text-parts`, `tool-calls` と同列に `usage-data` を追加
  - `src/llm/client.lisp:42-55` — `complete` / `complete-with-tools` のgeneric function定義。docstringを更新して第2値を文書化
  - `src/agent/core.lisp:139-183` — `agent-step`: `complete-with-tools` の呼び出し箇所(line 153-156)。`multiple-value-bind` でusageを受け取る

  **API/Type References**:
  - `src/llm/message.lisp` — message構造体定義。usage-plistはmessageに含めず別値として返す
  - `src/llm/client.lisp:22-40` — `llm-client` クラス。token-trackerスロットはここではなく `agent` クラスに追加

  **Test References**:
  - `tests/client-test.lisp` — 既存のクライアントテスト。モックレスポンスのパターンを参考
  - `tests/agent-test.lisp` — 既存のエージェントテスト。agent-stepのテストパターン参考

  **External References**:
  - Anthropic API usage response format: レスポンスJSONの `usage` オブジェクトに `input_tokens` (int), `output_tokens` (int), `cache_creation_input_tokens` (int, optional), `cache_read_input_tokens` (int, optional) が含まれる
  - Streaming: `message_start` イベントの `message.usage` にinput_tokens、`message_delta` イベントの `usage` にoutput_tokensが含まれる

  **Acceptance Criteria**:

  TDD Tests (FiveAM):
  - [ ] テストファイル作成: `tests/token-tracking-test.lisp`
  - [ ] テスト: モックレスポンスから `parse-anthropic-response` が `(values message (:input-tokens 150 :output-tokens 50))` を返す
  - [ ] テスト: `message_start` SSEイベントからusageが抽出される
  - [ ] テスト: `token-tracker` に3回累積後、合計が正しい
  - [ ] テスト: `agent-step` 後に `agent-token-tracker` の値が更新される（モッククライアント使用）
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS（既存テスト含む）

  Agent-Executed QA Scenarios:

  ```
  Scenario: parse-anthropic-response extracts usage from mock response
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)'
      2. Evaluate:
         (let ((resp (make-hash-table :test (quote equal))))
           (setf (gethash "content" resp)
                 (vector (alexandria:alist-hash-table
                          (list (cons "type" "text") (cons "text" "hello")))))
           (setf (gethash "usage" resp)
                 (alexandria:alist-hash-table
                  (list (cons "input_tokens" 150) (cons "output_tokens" 50))))
           (multiple-value-bind (msg usage)
               (sibyl.llm::parse-anthropic-response resp)
             (format t "input=~a output=~a content=~a"
                     (getf usage :input-tokens)
                     (getf usage :output-tokens)
                     (sibyl.llm:message-content msg))))
      3. Assert: output contains "input=150 output=50 content=hello"
    Expected Result: Usage plist correctly extracted as second return value
    Evidence: Terminal output captured

  Scenario: Full test suite passes
    Tool: Bash
    Preconditions: All changes committed
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
      2. Assert: exit code 0
      3. Assert: output contains "Did X checks" with 0 failures
    Expected Result: All existing + new tests pass
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): add token usage tracking from API responses`
  - Files: `src/llm/providers.lisp`, `src/llm/client.lisp`, `src/agent/core.lisp`, `tests/token-tracking-test.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 2. System Prompt Restructuring — 静的/動的ブロック分離

  **What to do**:
  - `memory-context-window` (memory.lisp:43-59) を修正: システムプロンプトを「静的ブロック」（`*default-system-prompt*` そのまま）と「動的ブロック」（会話要約）に分離して返す
  - `messages-to-anthropic-format` (providers.lisp) を修正: system promptがリスト（content blocks）の場合を処理。静的ブロックには後で `cache_control` を付与できるようにする
  - 現在 system prompt は文字列として返されている。これを Anthropic のcontent block形式（`[{"type": "text", "text": "..."}, ...]`）に対応させる
  - `system-message` 関数が文字列だけでなくコンテンツブロックリストも受け付けるようにする
  - テストをTDDで作成:
    - RED: `memory-context-window` がsummaryなしの場合、system promptが単一ブロックで返される
    - RED: summaryありの場合、system promptが2ブロック（static + dynamic）で返される
    - RED: `messages-to-anthropic-format` がcontent block形式のsystemを正しくフォーマットする
    - GREEN: 実装
    - REFACTOR

  **Must NOT do**:
  - `*default-system-prompt*` のテキスト内容は一切変更しない
  - 既存の `agent-test.lisp` のシステムプロンプトテスト（6テスト）を壊さない
  - `cache_control` の付与はこのタスクでは行わない（Task 3で実施）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: メッセージフォーマットの内部表現変更は影響範囲が広い
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Task 7)
  - **Blocks**: Tasks 3, 5
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/agent/memory.lisp:43-59` — `memory-context-window`: 現在 `format nil "~a~%~%## Previous conversation summary:~%~a"` でsystem promptと要約を結合。これを分離する
  - `src/llm/providers.lisp:87-110` — `messages-to-anthropic-format`: system messageの処理。現在は文字列前提
  - `src/llm/message.lisp` — `system-message` 関数定義。コンテンツブロック対応に拡張

  **Test References**:
  - `tests/agent-test.lisp:46-121` — 6つのシステムプロンプトテスト。`*default-system-prompt*` の特定文字列を検証。これらが壊れないことを確認

  **External References**:
  - Anthropic API system format: `"system": [{"type": "text", "text": "static part"}, {"type": "text", "text": "dynamic summary"}]` — リスト形式で複数ブロックを送信可能

  **Acceptance Criteria**:

  TDD Tests:
  - [ ] テスト: summaryなしで `memory-context-window` → system prompt が単一テキストブロック
  - [ ] テスト: summaryありで `memory-context-window` → 2つの分離されたブロック
  - [ ] テスト: `messages-to-anthropic-format` がcontent block形式systemを正しくalist化
  - [ ] テスト: 既存の `agent-test.lisp` のシステムプロンプトテスト全通過
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS

  Agent-Executed QA Scenarios:

  ```
  Scenario: System prompt splits into static and dynamic blocks
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded
    Steps:
      1. Create a memory with summary: (let ((mem (sibyl.agent::make-memory)))
           (setf (sibyl.agent::memory-summary mem) "Test summary")
           (let ((ctx (sibyl.agent::memory-context-window mem :system-prompt "Static prompt")))
             (format t "~a" (car ctx))))
      2. Assert: First message contains system content with two separate blocks
      3. Assert: Static part equals "Static prompt" exactly (unchanged)
      4. Assert: Dynamic part contains "Test summary"
    Expected Result: System prompt properly split into cacheable static + dynamic summary
    Evidence: Terminal output captured

  Scenario: Existing system prompt tests still pass
    Tool: Bash
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(5am:run! (quote sibyl.tests::agent-suite))' --quit
      2. Assert: All agent-suite tests pass with 0 failures
    Expected Result: No regression in system prompt content tests
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `refactor(agent): split system prompt into static/dynamic content blocks`
  - Files: `src/agent/memory.lisp`, `src/llm/providers.lisp`, `src/llm/message.lisp`, `tests/token-tracking-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 3. Prompt Caching — Anthropic cache_control 対応

  **What to do**:
  - `tools-to-anthropic-format` (providers.lisp:112-118) を修正: 最後のツール定義に `cache_control: {"type": "ephemeral"}` を付与
  - `messages-to-anthropic-format` を修正: 静的システムプロンプトブロックに `cache_control: {"type": "ephemeral"}` を付与（Task 2で分離済み）
  - `token-tracker` を拡張: `cache-read-tokens` と `cache-write-tokens` のフィールドを追加（Task 1のusage plistから `cache_creation_input_tokens`, `cache_read_input_tokens` を読み取り）
  - キャッシュヒット率の計算: `cache-read-tokens / (cache-read-tokens + input-tokens)` をトラッカーに追加
  - 設定キー追加 (config.lisp): `"optimization.cache-enabled"` (default: t)
  - テストをTDDで作成:
    - RED: `tools-to-anthropic-format` の最後のツールに `cache_control` が含まれる
    - RED: system prompt の静的ブロックに `cache_control` が含まれる
    - RED: キャッシュ無効設定時に `cache_control` が付与されない
    - RED: キャッシュヒット率の計算が正しい
    - GREEN: 実装
    - REFACTOR

  **Must NOT do**:
  - OpenAI providerに `cache_control` を追加しない（Anthropic固有機能）
  - 動的ブロック（会話要約）に `cache_control` を付与しない（内容が変わるためキャッシュ無効化を起こす）
  - ツールスキーマの内容自体は変更しない（`cache_control` ラッパーのみ）
  - キャッシュTTLの設定UIは作らない（デフォルト5分のephemeralのみ）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: APIリクエスト構築の正確性が重要。キャッシュ無効化の連鎖を理解する必要あり
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Task 6)
  - **Blocks**: Task 5
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `src/llm/providers.lisp:112-118` — `tools-to-anthropic-format`: ここでツールスキーマをAnthropic形式に変換。最後の要素に `cache_control` を追加
  - `src/llm/providers.lisp:173-188` — `complete-anthropic-streaming` のbody構築部分: `final-body` にcache対応のsystem blockを渡す
  - `src/config.lisp` — 設定キーの定義パターン。`"optimization.cache-enabled"` を追加

  **API/Type References**:
  - Anthropic cache_control format: `"cache_control": {"type": "ephemeral"}` をcontent blockまたはtool定義に付与
  - Cache hierarchy: `tools` → `system` → `messages` の順でprefix matchingされる。ツールを変更するとsystem以降のキャッシュも無効化

  **Test References**:
  - Task 1で作成した `tests/token-tracking-test.lisp` — トラッカーのキャッシュフィールド拡張テストを追加

  **Acceptance Criteria**:

  TDD Tests:
  - [ ] テスト: `tools-to-anthropic-format` 出力の最後のツールに `("cache_control" . (("type" . "ephemeral")))` が存在
  - [ ] テスト: system prompt content block に `cache_control` 付与
  - [ ] テスト: `"optimization.cache-enabled"` が nil の場合、`cache_control` なし
  - [ ] テスト: cache-hit-rate 計算: read=900, input=100 → rate=0.9
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS

  Agent-Executed QA Scenarios:

  ```
  Scenario: Anthropic request body contains cache_control
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded, cache-enabled config
    Steps:
      1. Load system, build a request body using the Anthropic format functions
      2. Convert to JSON string
      3. Assert: JSON contains "cache_control"
      4. Assert: JSON contains "ephemeral"
      5. Assert: cache_control appears on system block
      6. Assert: cache_control appears on last tool definition
    Expected Result: Request body properly annotated for caching
    Evidence: JSON output captured

  Scenario: Cache-disabled config omits cache_control
    Tool: Bash (sbcl --eval)
    Preconditions: (setf (sibyl:config "optimization.cache-enabled") nil)
    Steps:
      1. Build request body with caching disabled
      2. Convert to JSON
      3. Assert: JSON does NOT contain "cache_control"
    Expected Result: No caching annotations when disabled
    Evidence: JSON output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): implement Anthropic prompt caching with cache_control`
  - Files: `src/llm/providers.lisp`, `src/config.lisp`, `tests/token-tracking-test.lisp`
  - Pre-commit: test suite

---

- [x] 4. Selective Tool Sending — タスクベースのツールフィルタリング

  **What to do**:
  - `deftool` マクロ (protocol.lisp) に `:category` パラメータ追加: `:general`, `:code`, `:file`, `:system`, `:analysis` などのカテゴリを定義
  - 既存の組み込みツールに適切なカテゴリを付与 (builtin.lisp, lisp-tools.lisp, analysis-tools.lisp, etc.)
  - MCP登録ツールにデフォルトカテゴリ `:external` を付与 (mcp/tools.lisp)
  - `tools-as-schema` を拡張: `(tools-as-schema &key categories)` で指定カテゴリのツールのみ返す。省略時は全ツール（後方互換）
  - `agent-step` を修正: ユーザー入力やコンテキストからカテゴリを推論するシンプルなヒューリスティック。例: ファイルパスを含む入力 → `:file` カテゴリ追加、コード関連キーワード → `:code` 追加
  - 常に含まれる「コア」カテゴリ `:general` を定義（基本応答に必要なツール）
  - ツールキャッシュ無効化: MCP接続/切断時にキャッシュをクリアするフック
  - テストをTDDで作成:
    - RED: `deftool` に `:category :file` を指定して作成 → `tool-category` が `:file`
    - RED: `(tools-as-schema :categories '(:file))` → `:file` カテゴリのツールのみ返す
    - RED: カテゴリ省略時は全ツール返す
    - GREEN: 実装
    - REFACTOR

  **Must NOT do**:
  - LLMベースのツール選択（Phase 2として将来実装）
  - 既存の `deftool` の必須パラメータ変更（`:category` はオプショナル、デフォルト `:general`）
  - ツールの削除や統合（カテゴリ付与のみ）
  - ヒューリスティックの過度な複雑化（シンプルなキーワードマッチングで十分）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: deftoolマクロの拡張は慎重な後方互換性の維持が必要
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 4 (with Task 5)
  - **Blocks**: Task 8
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/tools/protocol.lisp:1-50` — `deftool` マクロ定義。`:category` キーワードを追加
  - `src/tools/protocol.lisp:116-118` — `tools-as-schema`: カテゴリフィルタ引数を追加
  - `src/tools/builtin.lisp` — 組み込みツール定義。各ツールにカテゴリ付与
  - `src/tools/lisp-tools.lisp` — Lispツール。`:code` カテゴリ
  - `src/tools/analysis-tools.lisp` — 分析ツール。`:analysis` カテゴリ
  - `src/mcp/tools.lisp:77-94` — MCP ツール登録。デフォルト `:external` カテゴリ

  **Test References**:
  - `tests/tools-test.lisp` — 既存のツールテスト。deftoolの拡張後も通過すること

  **Acceptance Criteria**:

  TDD Tests:
  - [ ] テスト: `:category :file` 付きでdeftool → `tool-category` スロットが `:file`
  - [ ] テスト: カテゴリ未指定のdeftool → `tool-category` が `:general`
  - [ ] テスト: `(tools-as-schema :categories '(:file))` → `:file` カテゴリのみ返す
  - [ ] テスト: `(tools-as-schema)` → 全ツール返す（後方互換）
  - [ ] テスト: 既存の `tools-test.lisp` 全通過
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS

  Agent-Executed QA Scenarios:

  ```
  Scenario: Category-filtered tools-as-schema returns subset
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded, tools registered
    Steps:
      1. Load system
      2. Count total tools: (length (sibyl.tools:tools-as-schema))
      3. Count filtered tools: (length (sibyl.tools:tools-as-schema :categories '(:file)))
      4. Assert: filtered count < total count
      5. Assert: each filtered tool has category :file
    Expected Result: Filtering reduces tool count
    Evidence: Terminal output captured

  Scenario: Backward compatibility - no categories returns all
    Tool: Bash (sbcl --eval)
    Steps:
      1. (= (length (sibyl.tools:tools-as-schema))
            (length (sibyl.tools:list-tools)))
      2. Assert: returns T
    Expected Result: Default behavior unchanged
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(tools): add category-based selective tool schema sending`
  - Files: `src/tools/protocol.lisp`, `src/tools/builtin.lisp`, `src/tools/lisp-tools.lisp`, `src/tools/analysis-tools.lisp`, `src/mcp/tools.lisp`, `src/agent/core.lisp`, `tests/tools-test.lisp`
  - Pre-commit: test suite

---

- [x] 5. Conversation Compression — LLMベース要約

  **What to do**:
  - `memory-compact` (memory.lisp:64-101) を書き換え: 現在のテキストベースの要約をLLMベース要約に置換
  - 要約用のプロンプトテンプレート定義: 会話メッセージ群を受け取り、200トークン以内の構造化要約を返す
  - 要約にはlight-tierモデル（Haiku相当）を使用: コスト効率のため。`model-selector` のlight tierを活用
  - 要約が静的システムプロンプトと分離されている（Task 2で実装済み）ことを前提に、キャッシュを壊さない構造を維持
  - `memory` クラスに `:compaction-strategy` スロット追加: `:simple` (現行) と `:llm` の切替。デフォルト `:llm`、テスト時は `:simple`
  - `max-messages` のデフォルトを100から50に変更（より早い要約トリガー）
  - 要約トークン上限: 200トークン（`max_tokens` パラメータで制御）
  - テストをTDDで作成:
    - RED: `:compaction-strategy :simple` 時に既存の動作を維持
    - RED: `:compaction-strategy :llm` でモッククライアント使用時、`complete` が呼ばれ要約テキストが `memory-summary` に設定される
    - RED: 要約後も `memory-context-window` が正しい構造を返す
    - GREEN: 実装
    - REFACTOR

  **Must NOT do**:
  - 要約の品質最適化（反復的な改善、長さ調整等は将来タスク）
  - テストスイート内での実LLM呼び出し（`:compaction-strategy :simple` でテスト、またはモッククライアント使用）
  - `max-messages` の動的調整（固定値のみ）
  - 要約のキャッシュ（要約自体は短いので効果が低い）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: LLM呼び出しの組み込みとキャッシュ無効化の回避が複雑
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 4 (with Task 4)
  - **Blocks**: Task 8
  - **Blocked By**: Tasks 2, 3

  **References**:

  **Pattern References**:
  - `src/agent/memory.lisp:64-101` — 現在の `memory-compact`: テキストベース要約のプレースホルダー。コメント "In a full implementation, this would use the LLM to summarize" がある
  - `src/agent/memory.lisp:43-59` — `memory-context-window`: Task 2で静的/動的分離済み。要約はdynamic blockに入る
  - `src/agent/core.lisp:139-183` — `agent-step`: `agent-client` でLLMクライアントにアクセス可能。要約用にlight-tierクライアントが必要

  **API/Type References**:
  - `src/llm/model-selector.lisp:57-80` — light tierの定義。Haiku相当のモデル設定

  **Test References**:
  - `tests/agent-test.lisp` — エージェント動作テスト。コンパクション後の動作検証パターン参考

  **Acceptance Criteria**:

  TDD Tests:
  - [ ] テスト: `:compaction-strategy :simple` → 既存のテキスト要約動作維持
  - [ ] テスト: `:compaction-strategy :llm` + モッククライアント → `complete` 呼び出しで要約生成
  - [ ] テスト: 要約後に `memory-context-window` が正しいブロック構造を返す
  - [ ] テスト: `max-messages` が50の場合、51メッセージ目でコンパクション発生
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS

  Agent-Executed QA Scenarios:

  ```
  Scenario: LLM compaction generates structured summary with mock client
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded, mock client defined
    Steps:
      1. Create memory with :compaction-strategy :llm and max-messages 5
      2. Create mock client that returns "Summary: test discussion about X"
      3. Push 6 messages to trigger compaction
      4. Assert: memory-summary is not nil
      5. Assert: memory-summary contains text from mock client response
      6. Assert: conversation-length <= 3 (half of max)
    Expected Result: LLM-based compaction produces summary and trims history
    Evidence: Terminal output captured

  Scenario: Simple compaction strategy maintains backward compatibility
    Tool: Bash (sbcl --eval)
    Steps:
      1. Create memory with :compaction-strategy :simple and max-messages 5
      2. Push 6 messages
      3. Assert: memory-summary contains "Compacted" (existing format)
    Expected Result: Simple strategy unchanged
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(agent): implement LLM-based conversation compaction`
  - Files: `src/agent/memory.lisp`, `src/agent/core.lisp`, `tests/token-tracking-test.lisp`
  - Pre-commit: test suite

---

- [x] 6. Model Auto-Routing — model-selectorのエージェントループ統合

  **What to do**:
  - `start-repl` (repl.lisp) に `:use-model-selector` キーワード引数追加（デフォルト `nil`）
  - `:use-model-selector t` 時、`make-adaptive-agent` を使用してエージェント作成（`agent` の代わりに `adaptive-agent`）
  - `adaptive-agent` の `agent-step` メソッドを確認・修正: ステップ前にタスク複雑度を分析し、適切なtierのモデルを選択
  - `agent-run-adaptive` (model-selector.lisp) がすでに定義されている場合、それを活用
  - token-tracker にモデル別のトークン使用量を記録: tier名ごとの input/output tokens
  - 設定キー追加: `"optimization.auto-model-routing"` (default: nil)
  - テストをTDDで作成:
    - RED: `make-adaptive-agent` が `adaptive-agent` インスタンスを返す
    - RED: 単純入力（"hello"）に対して light tier が選択される
    - RED: 複雑入力（"analyze this code..."）に対して medium/heavy tier が選択される
    - GREEN: 実装
    - REFACTOR

  **Must NOT do**:
  - `model-selector` のロジック自体の書き換え（既存のティア定義とルールを使用）
  - `start-repl` のデフォルト動作変更（`:use-model-selector` はデフォルト `nil`）
  - 実行時のモデル切り替えによるキャッシュ問題の対処（今回スコープ外、ログに警告を出す程度）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: 既存の未接続コードの統合は影響範囲の確認が重要
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Task 3)
  - **Blocks**: Task 8
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/llm/model-selector.lisp:1-60` — `model-tier`, `model-config`, `task-analyzer`, `model-selector` クラス定義
  - `src/llm/model-selector.lisp:57-105` — `*default-model-tiers*`: Light(0.2x), Medium(1.0x), Heavy(3.0x) の定義
  - `src/llm/model-selector.lisp:110-134` — 複雑度分析ルール
  - `src/llm/model-selector.lisp:300+` — `make-adaptive-agent` の定義（DEFMETHODパターン）
  - `src/repl.lisp:988-1002` — `start-repl`: 現在 `agent` を作成。ここに分岐追加

  **Test References**:
  - `tests/agent-test.lisp` — エージェント作成テスト参考

  **Acceptance Criteria**:

  TDD Tests:
  - [ ] テスト: `make-adaptive-agent` → `adaptive-agent` 型のインスタンス
  - [ ] テスト: 単純入力のタスク分析 → light tier推奨
  - [ ] テスト: 複雑入力のタスク分析 → medium/heavy tier推奨
  - [ ] テスト: `:use-model-selector nil` → 通常の `agent` 作成（後方互換）
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS

  Agent-Executed QA Scenarios:

  ```
  Scenario: Task complexity analysis selects appropriate tier
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded
    Steps:
      1. Create model-selector with default tiers
      2. Analyze "hello" → Assert: recommended-tier is "light"
      3. Analyze "analyze this complex code and refactor the authentication module" → Assert: recommended-tier is "medium" or "heavy"
    Expected Result: Complexity-based tier selection works
    Evidence: Terminal output captured

  Scenario: start-repl with use-model-selector creates adaptive-agent
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded
    Steps:
      1. Call agent creation logic with :use-model-selector t
      2. Assert: returned agent is of type adaptive-agent
      3. Assert: agent has model-selector slot populated
    Expected Result: Adaptive agent properly created
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): integrate model-selector into agent loop for auto-routing`
  - Files: `src/llm/model-selector.lisp`, `src/repl.lisp`, `src/config.lisp`, `tests/agent-test.lisp`
  - Pre-commit: test suite

---

- [x] 7. Extended Thinking Foundation — thinking ブロックパース基盤

  **What to do**:
  - `message` 構造体 (message.lisp) に `:thinking` スロット追加: thinking content block のテキストを格納
  - `parse-anthropic-response` (providers.lisp:120-144) を修正: `type` が `"thinking"` のcontent blockを検出し、message の `:thinking` スロットに格納
  - `parse-anthropic-sse-events` (providers.lisp:146-171) を修正: `content_block_start` で `type: "thinking"` を処理。`thinking_delta` イベントでthinkingテキストを収集
  - `complete-anthropic-streaming` (providers.lisp:173-245) を修正: thinkingブロックの収集と message 構造体への格納
  - token-tracker に `thinking-tokens` フィールド追加: usage response の thinking token 情報を追跡
  - テストをTDDで作成:
    - RED: `parse-anthropic-response` がthinkingブロック付きモックレスポンスから `message-thinking` を返す
    - RED: thinkingなしのレスポンスでは `message-thinking` がnil
    - RED: token-tracker が thinking tokens を累積
    - GREEN: 実装
    - REFACTOR

  **Must NOT do**:
  - Extended Thinking の有効化パラメータ（`thinking: {"type": "enabled", ...}`）のリクエスト送信実装（基盤のみ）
  - thinking content の表示（REPLでの表示は将来タスク）
  - thinking token の `budget_tokens` 制御

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: パース追加は比較的単純な拡張
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Task 2)
  - **Blocks**: Task 8
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/llm/message.lisp` — `message` 構造体定義。`:thinking` スロットを追加
  - `src/llm/providers.lisp:120-144` — `parse-anthropic-response`: `content_blocks` ループ内に `"thinking"` タイプの分岐追加
  - `src/llm/providers.lisp:146-171` — `parse-anthropic-sse-events`: `content_block_start` で `"thinking"` を処理

  **External References**:
  - Anthropic Extended Thinking response format: content blocks に `{"type": "thinking", "thinking": "reasoning text..."}` が含まれる。`signature` フィールドも付随するが今回は無視可
  - Streaming: `content_block_start` で `type: "thinking"`、`content_block_delta` で `type: "thinking_delta"` + `thinking` フィールド

  **Test References**:
  - `tests/message-test.lisp` — 既存のmessageテスト。構造体拡張後も通過すること

  **Acceptance Criteria**:

  TDD Tests:
  - [ ] テスト: thinkingブロック付きモックレスポンス → `message-thinking` に "reasoning text" が格納
  - [ ] テスト: thinkingなしモックレスポンス → `message-thinking` が nil
  - [ ] テスト: 既存の `message-test.lisp` 全通過（構造体拡張で壊れないこと）
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS

  Agent-Executed QA Scenarios:

  ```
  Scenario: parse-anthropic-response handles thinking content blocks
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded
    Steps:
      1. Create mock response with thinking block:
         content = [{"type": "thinking", "thinking": "Let me reason..."}, {"type": "text", "text": "Answer"}]
      2. Parse with parse-anthropic-response
      3. Assert: (message-thinking result) equals "Let me reason..."
      4. Assert: (message-content result) equals "Answer"
    Expected Result: Both thinking and text content correctly extracted
    Evidence: Terminal output captured

  Scenario: Existing messages without thinking work unchanged
    Tool: Bash (sbcl --eval)
    Steps:
      1. Parse standard response (no thinking blocks)
      2. Assert: (message-thinking result) is nil
      3. Assert: (message-content result) has expected text
    Expected Result: No regression for non-thinking responses
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): add extended thinking content block parsing foundation`
  - Files: `src/llm/message.lisp`, `src/llm/providers.lisp`, `tests/message-test.lisp`
  - Pre-commit: test suite

---

- [x] 8. Integration Testing & REPL Command — 統合テストとユーザーインターフェース

  **What to do**:
  - `/tokens` REPLコマンド追加 (repl.lisp): セッション累計のトークン使用量を表示
    - 表示内容: 入力トークン合計、出力トークン合計、キャッシュヒットトークン、キャッシュ書き込みトークン、キャッシュヒット率、推定コスト（モデル別）
  - `/tokens` コマンドの出力フォーマット:
    ```
    Token Usage (this session):
      Input:  12,450 tokens
      Output:  3,200 tokens
      Cache Read:  10,800 tokens (86.7% hit rate)
      Cache Write:  1,650 tokens
      Est. Cost: $0.12
    ```
  - 統合テスト作成: モッククライアントを使用して、全最適化が連携して動作することを検証
  - `sibyl.asd` の更新: 新規テストファイルの登録
  - テストをTDDで作成:
    - RED: `/tokens` コマンドがフォーマットされたトークン情報を返す
    - RED: 統合テスト: agent-step後にtrackerが更新され、cache_control付きリクエストが生成される
    - GREEN: 実装
    - REFACTOR

  **Must NOT do**:
  - チャート表示、履歴グラフ等の高度なUI
  - リクエスト毎のトークン表示（セッション累計のみ）
  - 外部ファイルへのトークンログ出力

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: REPL コマンド追加と統合テストは比較的単純
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 5 (solo, final)
  - **Blocks**: None (final task)
  - **Blocked By**: Tasks 3, 4, 5, 6, 7

  **References**:

  **Pattern References**:
  - `src/repl.lisp` — 既存REPLコマンド（`/help`, `/tools`, `/reset`, `/history`）の定義パターン。同じ形式で `/tokens` を追加
  - `src/agent/core.lisp` — `agent-token-tracker` スロット（Task 1で追加）へのアクセス

  **Test References**:
  - `tests/repl-test.lisp` — 既存REPLテスト。コマンド追加テストのパターン参考

  **Acceptance Criteria**:

  TDD Tests:
  - [ ] テスト: `/tokens` コマンドがトークン使用量の文字列を返す
  - [ ] テスト: フォーマットに "Input:", "Output:", "Cache Read:" が含まれる
  - [ ] テスト: 統合テスト — モッククライアントでagent-run → tracker更新 → /tokens出力が正確
  - [ ] `asdf:test-system :sibyl` → 全テスト PASS（最終確認）

  Agent-Executed QA Scenarios:

  ```
  Scenario: /tokens command displays formatted token usage
    Tool: interactive_bash (tmux)
    Preconditions: Sibyl REPL started with mock client
    Steps:
      1. Start REPL in tmux session
      2. Send a test message to generate token usage
      3. Type: /tokens
      4. Wait for output (timeout: 5s)
      5. Assert: output contains "Token Usage"
      6. Assert: output contains "Input:"
      7. Assert: output contains numbers (token counts)
    Expected Result: Formatted token usage displayed
    Evidence: Terminal output captured

  Scenario: Full integration - all optimizations work together
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded with all optimizations
    Steps:
      1. Create agent with all optimizations enabled
      2. Build request body
      3. Assert: cache_control present on system prompt
      4. Assert: cache_control present on tools
      5. Assert: tool count < total registered (selective sending)
      6. Assert: token tracker initialized and ready
    Expected Result: All optimizations coexist without conflicts
    Evidence: Terminal output captured

  Scenario: Final full test suite
    Tool: Bash
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
      2. Assert: exit code 0
      3. Assert: 0 failures
    Expected Result: All tests pass including new optimization tests
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add /tokens command and integration tests for cost optimization`
  - Files: `src/repl.lisp`, `tests/token-tracking-test.lisp`, `sibyl.asd`
  - Pre-commit: test suite

---

## Commit Strategy

| After Task | Message | Key Files | Verification |
|------------|---------|-----------|--------------|
| 1 | `feat(llm): add token usage tracking from API responses` | providers.lisp, client.lisp, core.lisp | test suite |
| 2 | `refactor(agent): split system prompt into static/dynamic content blocks` | memory.lisp, providers.lisp, message.lisp | test suite |
| 3 | `feat(llm): implement Anthropic prompt caching with cache_control` | providers.lisp, config.lisp | test suite |
| 4 | `feat(tools): add category-based selective tool schema sending` | protocol.lisp, builtin.lisp, core.lisp | test suite |
| 5 | `feat(agent): implement LLM-based conversation compaction` | memory.lisp, core.lisp | test suite |
| 6 | `feat(llm): integrate model-selector into agent loop for auto-routing` | model-selector.lisp, repl.lisp | test suite |
| 7 | `feat(llm): add extended thinking content block parsing foundation` | message.lisp, providers.lisp | test suite |
| 8 | `feat(repl): add /tokens command and integration tests for cost optimization` | repl.lisp, sibyl.asd | test suite |

---

## Success Criteria

### Verification Commands
```bash
# Full test suite
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
# Expected: 0 failures

# Token tracking verification (in REPL)
# /tokens → shows Input/Output/Cache stats
```

### Final Checklist
- [x] All "Must Have" present: token tracking, prompt caching, backward compatibility
- [x] All "Must NOT Have" absent: no system prompt text changes, no tool result compression, no OpenAI caching changes
- [x] All tests pass: `(asdf:test-system :sibyl)` → 1553 checks, 0 failures
- [x] `/tokens` command works in REPL (format-token-usage + handle-tokens-command implemented)
- [x] Prompt caching annotations in Anthropic requests (cache_control on system + last tool)
- [x] Selective tool sending reduces tool count (43 total → filtered by category)
- [x] Model routing available via `:use-model-selector t` in start-repl
- [x] Extended thinking blocks parsed correctly (message-thinking slot + parse-anthropic-response)
