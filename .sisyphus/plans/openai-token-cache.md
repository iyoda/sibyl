# OpenAI Token Cache: Usage 抽出・Pricing・キャッシュ最適化

## TL;DR

> **Quick Summary**: OpenAI Client の `parse-openai-response` が usage データを返していない根本バグを修正し、GPT-5 mini の pricing を追加。サーバーサイド Prompt Cache のトークン追跡・コスト計算・テレメトリを完全統合し、`/tokens` `/cost-report` を正しく機能させる。
>
> **Deliverables**:
> - `parse-openai-response` が `(values message usage-plist)` を返すよう修正
> - OpenAI ストリーミングの usage 抽出（`stream_options` 追加含む）
> - GPT-5 mini pricing テーブルエントリ（cache-read 割引率込み）
> - サーバーサイドキャッシュトークンのテレメトリ統合
> - `prompt_cache_key` / `prompt_cache_retention` パラメータサポート（条件付き）
> - TDD テスト一式（FiveAM）
>
> **Estimated Effort**: Medium
> **Parallel Execution**: YES - 2 waves
> **Critical Path**: Task 1 → Task 3 → Task 4 → Task 5 → Task 7

---

## Context

### Original Request
OpenAI Client のトークンキャッシュを有効にしてコスト削減したい。

### Interview Summary
**Key Discussions**:
- **スコープ**: フル対応（基盤修正 + pricing + キャッシュ最適化 + テレメトリ統合）
- **対象モデル**: GPT-5 mini のみ pricing テーブルに追加
- **テスト戦略**: TDD（RED → GREEN → REFACTOR）

**Research Findings**:
- **根本バグ発見**: `parse-openai-response` が usage を返していない。Anthropic 側は正常動作。OpenAI 利用時のトークン追跡・コスト計算が完全に非機能。
- **OpenAI Prompt Caching は自動**: コード変更なしで有効（Anthropic の `cache_control` ブロックとは異なる）
- **GPT-5 mini**: $0.25/1M input, $1.00/1M output, cache-read $0.025/1M（90%割引）、cache-write コスト無し
- **レスポンス形式**: `usage.prompt_tokens_details.cached_tokens` でキャッシュトークン報告
- **ストリーミング**: `stream_options: {"include_usage": true}` を明示的に要求する必要あり（Metis 指摘）

### Metis Review
**Identified Gaps** (addressed):
- **CRITICAL**: ストリーミングで usage を受け取るには `stream_options: {"include_usage": true}` をリクエストボディに追加する必要がある → Task 3 に組み込み済み
- **HIGH RISK**: `prompt_cache_key` / `prompt_cache_retention` が現在の Chat Completions API に存在するか未確認 → Task 6 で条件付き実装（API 検証を先行）
- **MEDIUM**: cache `:around` メソッドが現在 OpenAI に対して NIL usage を保存している → Task 1 の修正で自動解消。既存キャッシュエントリは TTL で期限切れになる
- **LOW**: `estimate-cost-usd` が未知モデルで Sonnet 価格にフォールバック → Task 2 で解消

---

## Work Objectives

### Core Objective
OpenAI Client のトークン usage 抽出を修正し、サーバーサイド Prompt Cache のコスト削減効果を正確に計測・可視化する。

### Concrete Deliverables
- `src/llm/providers.lisp`: `parse-openai-response` 修正、ストリーミング usage 抽出
- `src/llm/pricing.lisp`: GPT-5 mini pricing エントリ追加
- `tests/openai-test.lisp`: 新規テストファイル（TDD）
- テレメトリ統合（既存の `openai-extract-server-cache-tokens` 接続）

### Definition of Done
- [ ] `(multiple-value-bind (msg usage) (parse-openai-response resp))` で usage-plist が取得できる
- [ ] `(getf usage :cache-read-tokens)` で `cached_tokens` の値が正しく返る
- [ ] `(lookup-model-pricing "gpt-5-mini")` がフォールバック警告なしで正しい pricing を返す
- [ ] ストリーミング時も usage-plist が返る（`stream_options` 込み）
- [ ] `(asdf:test-system :sibyl)` が全テスト PASS
- [ ] 新規テストスイートが `*safe-suites*` または `*unsafe-suites*` に分類済み

### Must Have
- `parse-openai-response` が `(values message usage-plist)` を返す
- usage-plist に `:input-tokens`, `:output-tokens`, `:cache-read-tokens`, `:cache-write-tokens` を含む
- GPT-5 mini の pricing エントリ（`:cache-write 0.0`, `:cache-read` 確認済みレート）
- ストリーミングリクエストに `stream_options: {"include_usage": true}` を含む
- TDD テスト一式

### Must NOT Have (Guardrails)
- `parse-anthropic-response` への変更
- `complete-anthropic-streaming` への変更
- Anthropic 固有コードへの変更
- `cache/integration.lisp` の `:around` メソッドへの変更（既に正しく `(values message usage)` を期待）
- `token-tracker` 構造体の変更（既に全必要フィールドを持つ）
- `cache/openai.lisp` の `openai-normalize-request`, `openai-wrap-response`, `openai-no-cache-p` への変更
- GPT-5 mini 以外のモデルを pricing テーブルに追加
- `as any` / `@ts-ignore` 相当のハック
- 実 API を呼ぶテスト

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks are verified by executing REPL commands or running the test suite.
> No manual API calls, no visual confirmation, no human action required.

### Test Decision
- **Infrastructure exists**: YES (FiveAM, 20 test files)
- **Automated tests**: TDD (RED → GREEN → REFACTOR)
- **Framework**: FiveAM (`(asdf:test-system :sibyl)`)

### Test Commands
- Unit: `(asdf:test-system :sibyl)`
- Parallel: `(sibyl.tests:run-tests-parallel)`
- Single suite: `(fiveam:run! 'sibyl.tests::openai-usage-tests)`

### TDD Workflow (per task)
1. **RED**: テスト作成 → `(fiveam:run! 'suite)` → FAIL（実装なし）
2. **GREEN**: 最小実装 → `(fiveam:run! 'suite)` → PASS
3. **REFACTOR**: コード整理 → `(fiveam:run! 'suite)` → PASS（変わらず）

### Agent-Executed QA Scenarios (MANDATORY — ALL tasks)

**Verification Tool by Deliverable Type:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| Common Lisp 実装 | Bash (sbcl REPL) | `sbcl --eval '(ql:quickload :sibyl/tests)' --eval '(asdf:test-system :sibyl)' --quit` |
| テスト結果 | Bash (sbcl REPL) | テスト実行、PASS/FAIL カウント確認 |
| Suite 分類 | Bash (sbcl REPL) | `(sibyl.tests:run-tests-parallel)` で unclassified 警告なし確認 |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately):
├── Task 1: parse-openai-response usage 抽出 (基盤 - 最優先)
└── Task 2: GPT-5 mini pricing テーブル追加 (独立)

Wave 2 (After Wave 1):
├── Task 3: ストリーミング usage 抽出 + stream_options (depends: 1)
├── Task 4: テレメトリ統合 (depends: 1)
└── Task 5: キャッシュ統合検証 (depends: 1, 2)

Wave 3 (After Wave 2):
├── Task 6: prompt_cache_key/retention (条件付き, depends: 3)
└── Task 7: 最終統合テスト + suite 分類 (depends: all)

Critical Path: Task 1 → Task 3 → Task 5 → Task 7
Parallel Speedup: ~35% faster than sequential
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 3, 4, 5 | 2 |
| 2 | None | 5 | 1 |
| 3 | 1 | 6, 7 | 4, 5 |
| 4 | 1 | 7 | 3, 5 |
| 5 | 1, 2 | 7 | 3, 4 |
| 6 | 3 | 7 | — |
| 7 | All | None | None (final) |

### Agent Dispatch Summary

| Wave | Tasks | Recommended Agents |
|------|-------|-------------------|
| 1 | 1, 2 | task(category="unspecified-low") parallel |
| 2 | 3, 4, 5 | task(category="unspecified-low") parallel after Wave 1 |
| 3 | 6, 7 | task(category="unspecified-low") sequential |

---

## TODOs

- [ ] 1. parse-openai-response: usage-plist 抽出の追加 (TDD)

  **What to do**:
  - **RED**: `tests/openai-test.lisp` に `openai-usage-tests` スイートを作成
    - テスト: `parse-openai-response` が `(values message usage-plist)` を返す
    - テスト: usage-plist に `:input-tokens`, `:output-tokens` が含まれる
    - テスト: `prompt_tokens_details.cached_tokens` が `:cache-read-tokens` にマッピングされる
    - テスト: `:cache-write-tokens` が常に 0（OpenAI は cache-write コストなし）
    - テスト: usage フィールドが存在しない場合 NIL を返す
    - テスト: `prompt_tokens_details` が存在しない場合 `cached_tokens` は 0
  - **GREEN**: `src/llm/providers.lisp` の `parse-openai-response` (L498-526) を修正
    - `response` から `"usage"` hash-table を抽出
    - `"prompt_tokens"` → `:input-tokens`
    - `"completion_tokens"` → `:output-tokens`
    - `"prompt_tokens_details"."cached_tokens"` → `:cache-read-tokens`
    - `:cache-write-tokens` → 常に 0
    - `(values (assistant-message ...) usage-plist)` を返す
  - **REFACTOR**: 必要に応じて共通 usage 抽出ヘルパーに整理

  **Must NOT do**:
  - `parse-anthropic-response` を変更しない
  - cache `:around` メソッドを変更しない（既に multiple-value-bind で正しく動作する）
  - `token-tracker` 構造体を変更しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: 明確なパターン（Anthropic 実装）があり、限定的な変更で済む
  - **Skills**: []
    - 追加スキル不要 — Common Lisp の標準パターンのみ

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 2)
  - **Blocks**: Tasks 3, 4, 5
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References** (既存コードの参照):
  - `src/llm/providers.lisp:189-228` — `parse-anthropic-response` の完全な実装。usage hash-table からの抽出パターン、`(values message usage-plist)` の返し方。このパターンを OpenAI 版に適用する
  - `src/llm/providers.lisp:498-526` — 現在の `parse-openai-response`。修正対象。`(assistant-message ...)` のみ返している部分を `(values ...)` に変更
  - `src/cache/openai.lisp:60-74` — `openai-extract-server-cache-tokens`。`prompt_tokens_details.cached_tokens` の抽出ロジックが既にある。このロジックを参考に usage-plist を構築する

  **API/Type References** (データ構造):
  - `src/llm/token-tracker.lisp:5-11` — `token-tracker` 構造体。usage-plist のキー名（`:input-tokens`, `:output-tokens`, `:cache-read-tokens`, `:cache-write-tokens`）はこの構造体のフィールド名に合わせる
  - `src/llm/message.lisp` — `assistant-message` 関数。`parse-openai-response` が第一戻り値として返す message の構築方法

  **Test References** (テストパターン):
  - `tests/token-tracking-test.lisp:12-47` — `parse-anthropic-response` のテスト。mock response hash-table の構築方法、`multiple-value-bind` でのアサーション。このパターンを OpenAI 向けに複製する
  - `tests/token-tracking-test.lisp:49-65` — usage が NIL の場合のテスト。同じパターンで OpenAI のエッジケースをテスト

  **External References**:
  - OpenAI Chat Completions API response: `usage.prompt_tokens` (入力), `usage.completion_tokens` (出力), `usage.prompt_tokens_details.cached_tokens` (キャッシュ)

  **Acceptance Criteria**:

  **TDD (tests):**
  - [ ] テストファイル作成: `tests/openai-test.lisp`
  - [ ] スイート `openai-usage-tests` を `sibyl-tests` の子として定義
  - [ ] テストカバー: 正常ケース（usage 全フィールドあり）
  - [ ] テストカバー: cached_tokens あり
  - [ ] テストカバー: usage フィールドなし → NIL
  - [ ] テストカバー: prompt_tokens_details なし → cache-read-tokens = 0
  - [ ] `(fiveam:run! 'sibyl.tests::openai-usage-tests)` → PASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: parse-openai-response returns usage with cached tokens
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded via (ql:quickload :sibyl)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' で起動
      2. mock response hash-table を構築:
         - choices: [{"message": {"content": "test"}}]
         - usage: {"prompt_tokens": 100, "completion_tokens": 50,
                   "prompt_tokens_details": {"cached_tokens": 80}}
      3. (multiple-value-bind (msg usage) (sibyl.llm::parse-openai-response resp) ...)
      4. Assert: (getf usage :input-tokens) = 100
      5. Assert: (getf usage :output-tokens) = 50
      6. Assert: (getf usage :cache-read-tokens) = 80
      7. Assert: (getf usage :cache-write-tokens) = 0
    Expected Result: 全 assertion が pass
    Failure Indicators: multiple-value-bind で usage が NIL

  Scenario: parse-openai-response handles missing usage gracefully
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded
    Steps:
      1. usage フィールドなしの mock response を構築
      2. (multiple-value-bind (msg usage) (sibyl.llm::parse-openai-response resp) ...)
      3. Assert: msg は有効な message struct
      4. Assert: usage は NIL
    Expected Result: エラーなし、usage は NIL
    Failure Indicators: エラー発生、または不正な usage 値
  ```

  **Commit**: YES
  - Message: `feat(llm): extract usage from OpenAI response with cache token tracking`
  - Files: `src/llm/providers.lisp`, `tests/openai-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(unless (fiveam:run! (quote sibyl.tests::openai-usage-tests)) (uiop:quit 1))' --quit`

---

- [ ] 2. GPT-5 mini pricing テーブルエントリ追加 (TDD)

  **What to do**:
  - **RED**: `tests/openai-test.lisp` に `openai-pricing-tests` スイートを追加
    - テスト: `lookup-model-pricing "gpt-5-mini"` が NIL でない
    - テスト: `:cache-write` が 0.0
    - テスト: `:cache-read` が正の値
    - テスト: `:input` と `:output` が正の値
    - テスト: フォールバック警告が出ない
  - **GREEN**: `src/llm/pricing.lisp` の `*model-pricing-table*` に GPT-5 mini エントリ追加
    - `"gpt-5-mini"` → `:input 0.25 :output 1.00 :cache-write 0.0 :cache-read 0.025`
  - **REFACTOR**: 必要に応じてコメント追加

  **Must NOT do**:
  - 既存の Claude モデル pricing を変更しない
  - GPT-5 mini 以外のモデルを追加しない
  - pricing テーブルのスキーマ（`:input :output :cache-write :cache-read`）を変更しない

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 単一ファイルへの数行追加、テストも簡潔
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 1)
  - **Blocks**: Task 5
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `src/llm/pricing.lisp:8-24` — 既存の `*model-pricing-table*`。Claude モデルのエントリ形式（`:input N :output N :cache-write N :cache-read N`）を正確に複製する
  - `src/llm/pricing.lisp:29-43` — `lookup-model-pricing` 関数。プレフィックスマッチのフォールバック動作。`"gpt-5-mini"` が完全一致でヒットすることを確認

  **Test References**:
  - `tests/token-tracking-test.lisp` — pricing 関連テストがあればパターン参照。なければ `cache-key-tests` の単純なアサーションパターンを参照: `tests/cache-test.lisp:31-58`

  **External References**:
  - OpenAI pricing (2025): GPT-5 mini = $0.25/1M input, $1.00/1M output, $0.025/1M cache-read, $0/1M cache-write

  **Acceptance Criteria**:

  **TDD:**
  - [ ] `openai-pricing-tests` スイート作成
  - [ ] `(fiveam:run! 'sibyl.tests::openai-pricing-tests)` → PASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: GPT-5 mini pricing lookup returns correct rates
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded
    Steps:
      1. (ql:quickload :sibyl :silent t)
      2. (let ((p (sibyl.llm::lookup-model-pricing "gpt-5-mini")))
           (assert (not (null p)))
           (assert (= 0.25 (getf p :input)))
           (assert (= 1.00 (getf p :output)))
           (assert (= 0.0 (getf p :cache-write)))
           (assert (= 0.025 (getf p :cache-read))))
    Expected Result: 全 assertion pass、警告なし
    Failure Indicators: "No pricing found" 警告が出力される

  Scenario: cost estimation with cached tokens is cheaper
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded, GPT-5 mini in pricing table
    Steps:
      1. (let* ((no-cache (sibyl.llm:estimate-cost-usd "gpt-5-mini"
                   :input-tokens 10000 :output-tokens 1000))
               (with-cache (sibyl.llm:estimate-cost-usd "gpt-5-mini"
                   :input-tokens 2000 :output-tokens 1000
                   :cache-read-tokens 8000)))
           (assert (< (getf with-cache :total) (getf no-cache :total))))
    Expected Result: キャッシュありのほうが安い
    Failure Indicators: キャッシュありのコストが同じまたは高い
  ```

  **Commit**: YES (groups with Task 1)
  - Message: `feat(llm): add GPT-5 mini to pricing table with cache-read rates`
  - Files: `src/llm/pricing.lisp`, `tests/openai-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(unless (fiveam:run! (quote sibyl.tests::openai-pricing-tests)) (uiop:quit 1))' --quit`

---

- [ ] 3. OpenAI ストリーミング usage 抽出 + stream_options (TDD)

  **What to do**:
  - **RED**: `tests/openai-test.lisp` に `openai-streaming-usage-tests` スイートを追加
    - テスト: リクエストボディに `"stream_options"` が含まれる
    - テスト: `stream_options` に `"include_usage": true` が含まれる
    - テスト: ストリーミング完了後に usage-plist が返される（mock SSE イベントで検証）
  - **GREEN**:
    1. `src/llm/providers.lisp` の `complete-openai-streaming` (L528-635) で:
       - リクエストボディに `("stream_options" . (("include_usage" . t)))` を追加
       - 最終 SSE チャンク（`data: [DONE]` の直前）から usage を抽出
       - `(values message usage-plist)` を返すよう変更
    2. SSE パース中に `usage` フィールドを持つチャンクを検出・保存
  - **REFACTOR**: usage 抽出ロジックを共通ヘルパーに整理可能か検討

  **Must NOT do**:
  - `complete-anthropic-streaming` を変更しない
  - 非ストリーミングの `complete` / `complete-with-tools` メソッドを変更しない（Task 1 で対応済み）
  - `http-post-stream` / `parse-sse-stream` の汎用部分を変更しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: SSE パースのステートマシン変更が必要で、既存ストリーミングロジックの理解が必須
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 4, 5)
  - **Blocks**: Tasks 6, 7
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/llm/providers.lisp:528-635` — 現在の `complete-openai-streaming`。修正対象。SSE イベントの処理ループ、tool-call-state の管理パターン
  - `src/llm/providers.lisp:290-364` — `complete-anthropic-streaming`。Anthropic 側のストリーミング usage 抽出パターン。`message_start` イベントから input_tokens を取得し、`message_delta` から output_tokens を取得する方法
  - `src/llm/providers.lisp:541-544` — 現在のストリーミングリクエストボディ構築部分。ここに `stream_options` を追加する

  **API/Type References**:
  - OpenAI SSE format: `data: {"usage": {"prompt_tokens": N, "completion_tokens": N, "prompt_tokens_details": {"cached_tokens": N}}}` が最終チャンクで送信される（`stream_options.include_usage=true` の場合のみ）

  **Test References**:
  - `tests/token-tracking-test.lisp` — mock レスポンスの構築パターン
  - `tests/cache-test.lisp:463-495` — mock-llm-client クラスパターン。ストリーミングのテストには SSE レスポンスの mock が必要

  **Acceptance Criteria**:

  **TDD:**
  - [ ] `openai-streaming-usage-tests` スイート作成
  - [ ] リクエストボディに stream_options が含まれることをテスト
  - [ ] `(fiveam:run! 'sibyl.tests::openai-streaming-usage-tests)` → PASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Streaming request includes stream_options
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded
    Steps:
      1. complete-openai-streaming が構築するリクエストボディを検証
      2. ボディの alist に ("stream_options" . (("include_usage" . t))) が含まれるか確認
      3. mock HTTP クライアントで実際のリクエストボディをキャプチャして検証
    Expected Result: stream_options が正しく含まれる
    Failure Indicators: stream_options が欠落

  Scenario: Streaming returns usage after completion
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded, mock SSE stream prepared
    Steps:
      1. mock SSE イベント列を構築（text delta + 最終 usage チャンク）
      2. complete-openai-streaming を呼び出し
      3. (multiple-value-bind (msg usage) ...)
      4. Assert: msg のコンテンツが正しい
      5. Assert: usage が NIL でない
      6. Assert: :input-tokens, :output-tokens が正の値
    Expected Result: message と usage の両方が返る
    Failure Indicators: usage が NIL
  ```

  **Commit**: YES
  - Message: `feat(llm): add stream_options and extract usage from OpenAI streaming`
  - Files: `src/llm/providers.lisp`, `tests/openai-test.lisp`
  - Pre-commit: テストスイート実行

---

- [ ] 4. サーバーサイドキャッシュトークンのテレメトリ統合 (TDD)

  **What to do**:
  - **RED**: `tests/openai-test.lisp` に `openai-cache-telemetry-tests` スイートを追加
    - テスト: OpenAI レスポンスの cached_tokens がテレメトリに記録される
    - テスト: `get-cache-telemetry` で server-cache-tokens が正の値を返す
  - **GREEN**:
    1. `parse-openai-response` が返す usage-plist に `:cache-read-tokens` が含まれている（Task 1 で対応済み）
    2. キャッシュテレメトリパスで `record-server-cache-tokens` が呼ばれるよう統合
    3. agent core の usage 処理フローで OpenAI の cache-read-tokens が正しく `tracker-add-usage` に渡されることを確認
  - **REFACTOR**: `openai-extract-server-cache-tokens` の必要性を再評価（Task 1 で usage-plist に直接含めるため、別途呼び出しが不要になる可能性あり）

  **Must NOT do**:
  - `token-tracker` 構造体を変更しない
  - cache telemetry のインターフェースを変更しない
  - Anthropic のテレメトリパスを変更しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: 既存のテレメトリインフラに接続するだけ。パターンは明確
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 5)
  - **Blocks**: Task 7
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/cache/telemetry.lisp` — `record-server-cache-tokens`, `get-cache-telemetry`。サーバーサイドキャッシュトークンの記録・取得インターフェース
  - `src/agent/core.lisp` — agent ループ内の usage 処理。`tracker-add-usage` の呼び出し箇所。ここで OpenAI の usage-plist が正しく渡されるか確認
  - `src/llm/token-tracker.lisp:13-25` — `tracker-add-usage`。`:cache-read-tokens` を累積するロジック

  **Test References**:
  - `tests/cache-test.lisp` — `cache-telemetry-tests` スイート。テレメトリのテストパターン

  **Acceptance Criteria**:

  **TDD:**
  - [ ] `openai-cache-telemetry-tests` スイート作成
  - [ ] `(fiveam:run! 'sibyl.tests::openai-cache-telemetry-tests)` → PASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: OpenAI cached tokens recorded in tracker
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded
    Steps:
      1. token-tracker を作成
      2. OpenAI 形式の usage-plist を作成 (:cache-read-tokens 500)
      3. (tracker-add-usage tracker usage-plist)
      4. Assert: (token-tracker-cache-read-tokens tracker) = 500
      5. Assert: (tracker-cache-hit-rate tracker) > 0.0
    Expected Result: cache-read-tokens が正しく累積
    Failure Indicators: cache-read-tokens が 0 のまま
  ```

  **Commit**: YES (groups with Task 3)
  - Message: `feat(cache): integrate OpenAI server-side cache telemetry`
  - Files: 変更ファイルに応じて
  - Pre-commit: テストスイート実行

---

- [ ] 5. キャッシュ統合検証（クライアントサイド LRU + usage 保存）(TDD)

  **What to do**:
  - **RED**: `tests/openai-test.lisp` に `openai-cache-integration-tests` スイートを追加
    - テスト: OpenAI complete が cache `:around` メソッド経由で usage を正しく保存する
    - テスト: キャッシュヒット時に保存された usage-plist が返される
    - テスト: `estimate-cost-usd "gpt-5-mini"` がキャッシュトークン込みで正しいコストを返す
  - **GREEN**: Task 1 + Task 2 の修正で cache `:around` メソッドが自動的に正しく動作することを検証。追加コード変更は最小限（必要な場合のみ）
  - **REFACTOR**: 統合テストの整理

  **Must NOT do**:
  - `cache/integration.lisp` の `:around` メソッドを変更しない（既に正しい）
  - LRU キャッシュ本体を変更しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: 既存の cache `:around` メソッドが正しく動作することを検証するだけ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 4)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `src/cache/integration.lisp:101-151` — cache `:around` メソッド。`(multiple-value-bind (message usage) (call-next-method) ...)` パターン。Task 1 の修正により、OpenAI からも正しい usage が返るようになる
  - `tests/cache-test.lisp:463-495` — `mock-llm-client` クラスと `with-clean-integration-state` マクロ。統合テストの mock パターン

  **Test References**:
  - `tests/cache-test.lisp` — cache 統合テスト全般。キャッシュヒット/ミスのテストパターン

  **Acceptance Criteria**:

  **TDD:**
  - [ ] `openai-cache-integration-tests` スイート作成
  - [ ] `(fiveam:run! 'sibyl.tests::openai-cache-integration-tests)` → PASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: OpenAI response cached with usage data
    Tool: Bash (sbcl REPL)
    Preconditions: sibyl loaded, mock-llm-client available
    Steps:
      1. mock-openai-client を作成（complete が (values msg usage-plist) を返す）
      2. cache を有効にして complete を呼ぶ
      3. Assert: cache MISS がログされる
      4. 同じリクエストで再度 complete を呼ぶ
      5. Assert: cache HIT がログされる
      6. Assert: 返された usage-plist が元の値と一致
    Expected Result: 2回目の呼び出しでキャッシュヒット、usage も保存済み
    Failure Indicators: usage が NIL でキャッシュされる
  ```

  **Commit**: YES
  - Message: `test(cache): verify OpenAI cache integration with usage tracking`
  - Files: `tests/openai-test.lisp`
  - Pre-commit: テストスイート実行

---

- [ ] 6. prompt_cache_key / prompt_cache_retention サポート（条件付き）

  **What to do**:
  - **⚠️ 前提条件**: まず OpenAI Chat Completions API ドキュメントで `prompt_cache_key` と `prompt_cache_retention` パラメータの存在を確認する。存在しない場合、このタスクはキャンセル。
  - **確認方法**: `curl https://api.openai.com/v1/chat/completions -H "Authorization: Bearer $OPENAI_API_KEY" -d '{"model":"gpt-5-mini","messages":[...],"prompt_cache_key":"test"}' ` で unrecognized parameter エラーが出るか確認
  - **存在する場合**:
    - **RED**: テスト作成
      - テスト: `openai-client` に `cache-key` と `cache-retention` スロットが存在
      - テスト: リクエストボディにパラメータが含まれる（設定時のみ）
      - テスト: 未設定時はリクエストに含まれない
    - **GREEN**:
      1. `openai-client` クラスにオプショナルスロット追加: `cache-key` (nil), `cache-retention` (nil)
      2. `make-openai-client` ファクトリに対応キーワード追加
      3. `complete` / `complete-with-tools` メソッドでリクエストボディに条件追加
    - **REFACTOR**: config システムとの統合（`optimization.openai.cache-key` 等）

  **Must NOT do**:
  - API に存在しないパラメータを追加しない
  - デフォルトで有効にしない（オプショナル、明示的設定のみ）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: クラスへのスロット追加とリクエストボディの条件分岐のみ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (sequential)
  - **Blocks**: Task 7
  - **Blocked By**: Task 3

  **References**:

  **Pattern References**:
  - `src/llm/providers.lisp:437-443` — `openai-client` クラス定義。新スロット追加位置
  - `src/llm/providers.lisp:444-457` — `make-openai-client` ファクトリ関数。新キーワード追加位置
  - `src/llm/providers.lisp:637-655` — `complete` メソッドのボディ構築。条件付きパラメータ追加の場所

  **External References**:
  - OpenAI API: `prompt_cache_key` (ルーティングヒント), `prompt_cache_retention` ("in_memory" | "24h")
  - ⚠️ これらが Chat Completions API で有効かは要確認（Responses API 専用の可能性あり）

  **Acceptance Criteria**:

  **前提確認:**
  - [ ] OpenAI API で `prompt_cache_key` パラメータが認識されるか確認
  - [ ] 認識されない場合 → このタスク全体をキャンセル

  **TDD (パラメータが存在する場合):**
  - [ ] テストスイート作成
  - [ ] テスト PASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: API parameter existence verification
    Tool: Bash (curl)
    Preconditions: OPENAI_API_KEY set
    Steps:
      1. curl -s -w "%{http_code}" -X POST https://api.openai.com/v1/chat/completions \
           -H "Authorization: Bearer $OPENAI_API_KEY" \
           -H "Content-Type: application/json" \
           -d '{"model":"gpt-5-mini","messages":[{"role":"user","content":"hi"}],"max_completion_tokens":5,"prompt_cache_key":"test-key"}'
      2. Assert: HTTP status is 200 (パラメータが認識される) OR 400 with "unrecognized" (認識されない)
      3. If 400 with unrecognized → タスクキャンセル
    Expected Result: パラメータの有効性が判明
    Evidence: Response body captured
  ```

  **Commit**: YES (if implemented)
  - Message: `feat(llm): add optional prompt_cache_key/retention for OpenAI`
  - Files: `src/llm/providers.lisp`, `tests/openai-test.lisp`
  - Pre-commit: テストスイート実行

---

- [ ] 7. 最終統合テスト + suite 分類 + 全テスト PASS

  **What to do**:
  - 全新規テストスイートを `tests/suite.lisp` の `*safe-suites*` または `*unsafe-suites*` に分類
    - `openai-usage-tests` → `*safe-suites*`（純粋ロジック、I/O なし）
    - `openai-pricing-tests` → `*safe-suites*`（純粋ロジック）
    - `openai-streaming-usage-tests` → 内容に応じて分類
    - `openai-cache-telemetry-tests` → `*unsafe-suites*`（グローバル状態操作）
    - `openai-cache-integration-tests` → `*unsafe-suites*`（キャッシュ状態操作）
  - `tests/openai-test.lisp` を `sibyl.asd` の `:sibyl/tests` コンポーネントに追加
  - 全テストスイート実行で PASS 確認
  - `run-tests-parallel` で unclassified 警告なし確認

  **Must NOT do**:
  - 既存テストスイートの分類を変更しない
  - テストを削除しない

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: suite.lisp と sibyl.asd への数行追加のみ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (final)
  - **Blocks**: None (final task)
  - **Blocked By**: All previous tasks

  **References**:

  **Pattern References**:
  - `tests/suite.lisp` — `*safe-suites*` と `*unsafe-suites*` の定義。新スイートをどちらに追加するか
  - `sibyl.asd` — `:sibyl/tests` システム定義。新テストファイルのコンポーネント追加位置

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Full test suite passes
    Tool: Bash (sbcl REPL)
    Preconditions: All previous tasks completed
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(let ((results (asdf:test-system :sibyl))) (format t "~%Results: ~a~%" results))' --quit
      2. Assert: exit code 0
      3. Assert: zero failures in output
    Expected Result: All tests pass
    Failure Indicators: Non-zero exit code, failure count > 0

  Scenario: Parallel runner has no unclassified warnings
    Tool: Bash (sbcl REPL)
    Preconditions: All suites classified in suite.lisp
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --quit
      2. Assert: output に "unclassified" 警告が含まれない
    Expected Result: Clean parallel run
    Failure Indicators: "unclassified suites" warning in output

  Scenario: New test file registered in ASDF
    Tool: Bash (grep)
    Preconditions: sibyl.asd exists
    Steps:
      1. grep "openai-test" sibyl.asd
      2. Assert: output に "openai-test" が含まれる
    Expected Result: File registered in :sibyl/tests system
    Failure Indicators: No match found
  ```

  **Commit**: YES
  - Message: `chore(tests): classify OpenAI test suites and register in ASDF`
  - Files: `tests/suite.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

## Commit Strategy

| After Task | Message | Files | Verification |
|------------|---------|-------|--------------|
| 1 | `feat(llm): extract usage from OpenAI response with cache token tracking` | `src/llm/providers.lisp`, `tests/openai-test.lisp` | `(fiveam:run! 'openai-usage-tests)` |
| 2 | `feat(llm): add GPT-5 mini to pricing table with cache-read rates` | `src/llm/pricing.lisp`, `tests/openai-test.lisp` | `(fiveam:run! 'openai-pricing-tests)` |
| 3 | `feat(llm): add stream_options and extract usage from OpenAI streaming` | `src/llm/providers.lisp`, `tests/openai-test.lisp` | `(fiveam:run! 'openai-streaming-usage-tests)` |
| 4 | `feat(cache): integrate OpenAI server-side cache telemetry` | 該当ファイル, `tests/openai-test.lisp` | `(fiveam:run! 'openai-cache-telemetry-tests)` |
| 5 | `test(cache): verify OpenAI cache integration with usage tracking` | `tests/openai-test.lisp` | `(fiveam:run! 'openai-cache-integration-tests)` |
| 6 | `feat(llm): add optional prompt_cache_key/retention for OpenAI` (条件付き) | `src/llm/providers.lisp`, `tests/openai-test.lisp` | テスト実行 |
| 7 | `chore(tests): classify OpenAI test suites and register in ASDF` | `tests/suite.lisp`, `sibyl.asd` | `(asdf:test-system :sibyl)` |

---

## Success Criteria

### Verification Commands
```lisp
;; 1. 全テスト PASS
(ql:quickload :sibyl/tests)
(asdf:test-system :sibyl)
;; Expected: 0 failures

;; 2. OpenAI usage 抽出
(let* ((resp ...) ;; mock response with usage
       )
  (multiple-value-bind (msg usage)
      (sibyl.llm::parse-openai-response resp)
    (assert (not (null usage)))
    (assert (plusp (getf usage :input-tokens)))))
;; Expected: No assertion error

;; 3. GPT-5 mini pricing
(sibyl.llm::lookup-model-pricing "gpt-5-mini")
;; Expected: (:INPUT 0.25 :OUTPUT 1.0 :CACHE-WRITE 0.0 :CACHE-READ 0.025)

;; 4. Parallel runner clean
(sibyl.tests:run-tests-parallel)
;; Expected: No "unclassified suites" warning
```

### Final Checklist
- [ ] All "Must Have" present
- [ ] All "Must NOT Have" absent
- [ ] All tests pass (`(asdf:test-system :sibyl)`)
- [ ] New suites classified in suite.lisp
- [ ] New test file registered in sibyl.asd
- [ ] No Anthropic code modified
- [ ] No pricing table schema changes
