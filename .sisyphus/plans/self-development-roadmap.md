# Sibyl Self-Development Roadmap: メタサーキュラー・ブートストラップ

## TL;DR

> **Quick Summary**: Common Lisp製コーディングエージェントSibylが、自分自身のソースコードを理解・修正・テスト・改善できるようにする。Lispのホモイコニシティ（S式 = コード = データ）を最大活用し、同一SBCLイメージ内でのインメモリ即反映とCondition Systemによる安全な自己修正を実現する。
> 
> **Deliverables**:
> - Phase 0: Lisp-Aware基盤ツール群（S式リーダー、シンボル探索、eval、マクロ展開）
> - Phase 1: 自己理解能力（自身のアーキテクチャマッピング、依存グラフ）
> - Phase 2: 安全な自己修正（Condition System rollback、ファイル同期、ASDF保護）
> - Phase 3: TDDによる自己テスト（FiveAM統合、RED→GREEN→REFACTOR）
> - Phase 4: 人間監視下の自己改善（タスク指示 → 設計 → 実装 → テスト → 報告）
> - Phase 5: 半自律開発（改善提案 → 実装 → マイルストーン承認）
> - Phase 6: 自己進化エージェント（自身の限界認識 → 計画 → 改善）
> 
> **Estimated Effort**: XL（段階的リリース、各フェーズが独立して価値を持つ）
> **Parallel Execution**: YES - フェーズ内で独立タスクの並列実行可
> **Critical Path**: Phase 0 → Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5 → Phase 6

---

## Context

### Original Request
Sibyl自身の開発をSibylで行えるようにするまでのロードマップを考えたい。

### Interview Summary
**Key Discussions**:
- **自律性レベル**: 段階的に進める。人間監視下から始め、信頼性に応じて自律度を上げる
- **Lisp活用度**: S式操作を最大活用。LispリーダーでソースをデータとしてREAD、defun単位の精密編集
- **最初のマイルストーン**: 既存ツール改善（小さくリスクの低い変更で実証）
- **安全性モデル**: Condition System活用（retry/skip/use-valueリスタートで回復）
- **ホットリロード**: インメモリ即反映（defun再定義 + ファイル同期）
- **実行環境**: SBCL REPL内から（真のメタサーキュラー自己修正）
- **テスト戦略**: TDD（テストファースト、FiveAM使用）

**Research Findings**:
- **クロージャのキャプチャ意味論**: `#'foo`でキャプチャされた関数オブジェクトは更新されない。`(foo ...)`のシンボル参照は即座に更新される。Sibylの`deftool`マクロはシンボル参照を使うため、ほとんどのハンドラは再定義を見る
- **`sb-introspect:who-calls`はコンパイル済みコードのみ対象**: eval後にcompileが必要
- **`function-lambda-expression`がNILを返す場合がある**: ロールバック用にはfdefinitionで関数オブジェクト自体を保存すべき
- **Eclector + CST**: ソース位置情報を保持したS式リーディングが可能
- **FiveAM programmatic API**: `fiveam:run`（データ返却）+ `results-status`でプログラム的にテスト結果を判定
- **ASDF reload protection**: `:around`メソッドで意図しないリロードを防止
- **歴史的先例**: EURISKO (1983), 3-LISP (1982) — Lispでの自己修正は実現可能だが、完全自律進化は40年経っても困難

### Metis Review
**Identified Gaps** (addressed):
- クロージャキャプチャの安全性ルール → Phase 2の`safe-redefine`ツールに組み込み
- コンパイル必須ルール → Phase 0の`eval-form`ツールでcompile自動適用
- `function-lambda-expression` NIL問題 → fdefinitionベースのロールバック設計
- ASDF reload保護 → Phase 2でASDF `:around`メソッド実装
- Phase 6の現実性 → スコープを限定し、「改善提案」レベルに留める
- FiveAMプログラマティックAPI → Phase 3で`fiveam:run` + `results-status`使用

---

## Work Objectives

### Core Objective
SibylがSBCL REPL内で自身のCommon Lispソースコードをメタサーキュラーに開発できる能力を、段階的に構築する。

### Concrete Deliverables
各フェーズが独立したマイルストーンとして完結する。

| Phase | Deliverable | 何ができるようになるか |
|-------|-------------|----------------------|
| 0 | Lisp-Aware基盤ツール | ソースをS式として読み、シンボルを探索し、式を評価できる |
| 1 | 自己理解能力 | 自身のアーキテクチャ、依存関係、コード構造を把握できる |
| 2 | 安全な自己修正 | defunを再定義し、失敗時はCondition Systemで回復できる |
| 3 | TDD自己テスト | 自分の変更に対してテストを先に書き、パスを確認できる |
| 4 | 人間監視下の自己改善 | 人間が指示したタスクを自律的に実装・テスト・報告できる |
| 5 | 半自律開発 | 自ら改善案を提案し、承認後に実装できる |
| 6 | 自己進化 | 自身の限界を認識し、改善計画を立てて実行できる |

### Definition of Done
- [ ] Sibylが自分のgrepツールのパラメータを追加する変更を、テストファーストで実装できる（Phase 4完了）
- [ ] 変更後にFiveAMテストが全パスする
- [ ] 変更がファイルに永続化される
- [ ] 失敗した変更からCondition Systemで回復できる

### Must Have
- S式レベルのコード操作（テキスト操作ではない）
- Condition Systemによる安全な自己修正とロールバック
- TDD: テストを先に書いてから実装
- インメモリ即反映 + ファイル同期
- 各フェーズが独立して動作し価値を持つ
- 全自己生成コードはcompile必須（`sb-introspect`互換性のため）

### Must NOT Have (Guardrails)
- **ファイルの全文上書きによる編集禁止**: 既存の`write-file`でソース全体を書き直すのではなく、defun/defmethod単位の精密編集
- **未コンパイルコードの実行禁止**: evalしたコードは必ずcompileする
- **テスト無しの自己修正禁止**: Phase 3以降、テスト無しの変更は拒否
- **ASDF無断リロード禁止**: インメモリ修正をASDF reloadで上書きしない
- **Phase 6での無制限自己進化禁止**: 提案→承認のフローを維持。自律的にアーキテクチャ変更しない
- **`#'function`キャプチャパターンの無視禁止**: 再定義時にクロージャの影響を確認
- **system-promptやLLMクライアントの自己修正禁止**: （Phase 4まで。Phase 5以降で段階的に解放）

---

## Verification Strategy (MANDATORY)

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks in this plan MUST be verifiable WITHOUT any human action.
> ALL verification is executed by the agent using tools (Bash/REPL, interactive_bash, etc.).

### Test Decision
- **Infrastructure exists**: YES (FiveAM)
- **Automated tests**: TDD (test-first)
- **Framework**: FiveAM (existing)

### TDD Workflow for Each Task

**Task Structure:**
1. **RED**: FiveAMでテストを先に書く
   - Test file: `tests/` 配下
   - Test command: `(asdf:test-system :sibyl)` or specific test via `(fiveam:run 'test-name)`
   - Expected: FAIL (test exists, implementation doesn't)
2. **GREEN**: テストをパスする最小実装
   - Command: `(fiveam:run 'test-name)` → `(fiveam:results-status *)` → T
   - Expected: PASS
3. **REFACTOR**: グリーンを保ちながら改善
   - Command: `(asdf:test-system :sibyl)` → 全テストPASS

### Agent-Executed QA Scenarios (MANDATORY)

**Verification Tool by Deliverable Type:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| Lisp関数/マクロ | Bash (sbcl --eval) | ql:quickload → 関数呼び出し → 出力検証 |
| ツール定義 | Bash (sbcl --eval) | ツール登録確認 → execute-tool → 結果検証 |
| 自己修正 | interactive_bash (sbcl REPL) | REPL内で修正実行 → 動作確認 → ファイル同期確認 |
| テスト | Bash (sbcl --eval) | fiveam:run → results-status → T確認 |
| Condition回復 | interactive_bash | 意図的エラー → リスタート呼び出し → 回復確認 |

---

## Execution Strategy

### Phase Dependencies

```
Phase 0 (Foundation)
├── Phase 1 (Self-Understanding) [depends: 0]
│   └── Phase 2 (Self-Modification) [depends: 0, 1]
│       └── Phase 3 (Self-Testing) [depends: 2]
│           └── Phase 4 (Supervised Self-Improvement) [depends: 3]
│               └── Phase 5 (Semi-Autonomous) [depends: 4]
│                   └── Phase 6 (Self-Evolving) [depends: 5]
```

### Phase内の並列化

各Phase内のタスクは依存関係に基づいて並列実行可能。
具体的な並列構成は各Phase内のWave定義で指定。

---

## TODOs

### ═══════════════════════════════════════════
### PHASE 0: Lisp-Aware Foundation Tools
### ═══════════════════════════════════════════
### 
### Sibylに「Lispを理解する目」を与える。
### 現在のテキストベースのツールをS式レベルに引き上げる。
### ═══════════════════════════════════════════

- [x] 0-1. `read-sexp` ツール — S式リーダー

  **What to do**:
  - Lispソースファイルを`CL:READ`でS式のリストとして読み込むツールを`deftool`で定義
  - トップレベルフォーム（defun, defmethod, defclass, defvar, defparameter, deftool等）を認識
  - 各フォームの種類、名前、開始/終了位置（行番号）を返す
  - ファイル全体のフォームリスト、または特定の名前のフォームを返すモード
  - READはコメントを捨てるため、行番号マッピングにはファイルの行位置を別途テキストレベルで取得
  - パラメータ: `path` (必須), `name` (オプション: 特定定義の検索), `type` (オプション: defun/defclass等フィルタ)

  **TDD**:
  - RED: `tests/sexp-tools-test.lisp`にテスト追加
    - Sibyl自身の`src/tools/builtin.lisp`をread-sexpで読み、6つのdeftoolフォームが認識されること
    - `name: "read-file"`で特定のdeftoolのS式が取得できること
    - 存在しないファイルでtool-errorがシグナルされること
  - GREEN: `src/tools/lisp-tools.lisp`に実装
  - REFACTOR: エッジケース（空ファイル、構文エラー）対応

  **Must NOT do**:
  - Eclectorの導入（Phase 0ではCL:READで十分。必要なら後のPhaseで検討）
  - コメント保持（READの性質上不可。ファイル編集はテキストレベルで行う設計）

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: Common Lispのリーダー、パッケージシステム、ファイル位置管理の知識が必要
  - **Skills**: [`playwright`]（検証時のみ必要な場合）
  - **Skills Evaluated but Omitted**:
    - `frontend-ui-ux`: UIなし
    - `git-master`: git操作なし

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 0-A (with Tasks 0-2, 0-3)
  - **Blocks**: [1-1, 1-2, 2-1]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/tools/protocol.lisp:51-79` — `deftool`マクロの使い方。新ツール定義はこのパターンに従う
  - `src/tools/builtin.lisp:10-17` — `read-file`ツールの実装パターン。ファイルパス処理の参考
  - `src/tools/builtin.lisp:1-4` — パッケージ宣言 `(in-package #:sibyl.tools)`。新ファイルも同じ

  **API/Type References**:
  - `src/tools/protocol.lisp:12-17` — `tool`構造体の定義（name, description, parameters, handler）
  - `src/tools/protocol.lisp:85-105` — `tool-to-schema`でパラメータがLLMに渡される形式

  **Test References**:
  - `tests/tools-test.lisp` — 既存のツールテストパターン。FiveAMの使い方の参考
  - `tests/suite.lisp` — テストスイート定義。新テストファイルの登録方法

  **External References**:
  - CLHS `READ`: https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm
  - CLHS `READ-FROM-STRING`: ファイル位置管理の代替手段

  **Acceptance Criteria**:
  - [ ] テストファイル `tests/sexp-tools-test.lisp` が存在し、FiveAMテストが定義されている
  - [ ] `(fiveam:run 'read-sexp-tests)` → `(fiveam:results-status *)` → T
  - [ ] ツール`read-sexp`が`*tool-registry*`に登録されている
  - [ ] `sibyl.asd`に新ファイル(`lisp-tools`, `sexp-tools-test`)が追加されている

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: read-sexp reads Sibyl's own builtin.lisp
    Tool: Bash (sbcl --eval)
    Preconditions: Sibyl loaded via (ql:quickload :sibyl)
    Steps:
      1. (ql:quickload :sibyl)
      2. (sibyl.tools:execute-tool "read-sexp" '(("path" . "src/tools/builtin.lisp")))
      3. Assert: result contains 6 tool definitions (read-file, write-file, list-directory, shell, grep, file-info)
      4. (sibyl.tools:execute-tool "read-sexp" '(("path" . "src/tools/builtin.lisp") ("name" . "read-file")))
      5. Assert: result contains the S-expression for the read-file deftool
    Expected Result: S-expression data returned as structured string
    Evidence: SBCL stdout captured

  Scenario: read-sexp handles missing file gracefully
    Tool: Bash (sbcl --eval)
    Preconditions: Sibyl loaded
    Steps:
      1. (handler-case (sibyl.tools:execute-tool "read-sexp" '(("path" . "/nonexistent.lisp")))
           (sibyl.conditions:tool-error (e) (format nil "CAUGHT: ~a" e)))
      2. Assert: output contains "CAUGHT:"
    Expected Result: tool-error condition signaled and caught
    Evidence: SBCL stdout captured
  ```

  **Commit**: YES
  - Message: `feat(tools): add read-sexp tool for S-expression aware source reading`
  - Files: `src/tools/lisp-tools.lisp`, `tests/sexp-tools-test.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 0-2. `describe-symbol` ツール — シンボルイントロスペクション

  **What to do**:
  - 指定されたシンボルの情報を返すツールを定義
  - 情報: バウンドされているか、fboundpか、マクロか、ジェネリック関数か、クラスか
  - 関数ならアーギュメントリスト（`sb-introspect:function-lambda-list`）
  - ジェネリック関数ならメソッドリスト
  - クラスならスロット定義
  - パラメータ: `symbol` (必須, "sibyl.tools:deftool"形式), `package` (オプション)
  - `sb-introspect`パッケージの活用

  **TDD**:
  - RED: `describe-symbol`で`sibyl.agent:agent-step`を問い合わせ → ジェネリック関数、2メソッドと判定されるテスト
  - GREEN: `sb-introspect`を使った実装
  - REFACTOR: 出力フォーマットの整理

  **Must NOT do**:
  - シンボルの定義ソースの取得（それは`read-sexp`の役割）
  - 外部パッケージのシンボルの修正（読み取り専用）

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: SBCL内部API（sb-introspect）の知識が必要
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 0-A (with Tasks 0-1, 0-3)
  - **Blocks**: [1-1, 1-2]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/tools/builtin.lisp:52-69` — `shell`ツールのパターン。外部情報を文字列で返す形式の参考
  - `src/tools/protocol.lisp:51-79` — `deftool`マクロの使い方

  **API/Type References**:
  - `src/agent/core.lisp:10-38` — `agent`クラスのCLOS定義。describe-symbolで解析対象になる
  - `src/agent/core.lisp:88-91` — `agent-step`ジェネリック関数。メソッドディスパッチの参考

  **External References**:
  - SBCL `sb-introspect`: http://www.sbcl.org/manual/#sb_002dintrospect
  - `sb-introspect:function-lambda-list` — 引数リスト取得
  - `sb-introspect:who-calls` — 呼び出し関係（コンパイル済みコードのみ）
  - `sb-introspect:find-definition-sources-by-name` — 定義元のソース位置

  **Acceptance Criteria**:
  - [ ] `(fiveam:run 'describe-symbol-tests)` → `(fiveam:results-status *)` → T
  - [ ] `describe-symbol "sibyl.agent:agent-step"` → ジェネリック関数と認識
  - [ ] `describe-symbol "sibyl.tools:*tool-registry*"` → 特殊変数と認識

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: describe-symbol identifies generic function
    Tool: Bash (sbcl --eval)
    Preconditions: Sibyl loaded
    Steps:
      1. (ql:quickload :sibyl)
      2. (sibyl.tools:execute-tool "describe-symbol" '(("symbol" . "sibyl.agent:agent-step")))
      3. Assert: output contains "generic-function"
      4. Assert: output contains method information
    Expected Result: Symbol type and metadata returned
    Evidence: SBCL stdout captured

  Scenario: describe-symbol handles non-existent symbol
    Tool: Bash (sbcl --eval)
    Steps:
      1. (sibyl.tools:execute-tool "describe-symbol" '(("symbol" . "sibyl.agent:nonexistent-xyz")))
      2. Assert: output indicates symbol not found or unbound
    Expected Result: Graceful handling of missing symbol
    Evidence: SBCL stdout captured
  ```

  **Commit**: YES (groups with 0-1)
  - Message: `feat(tools): add describe-symbol tool for Lisp symbol introspection`
  - Files: `src/tools/lisp-tools.lisp`, `tests/sexp-tools-test.lisp`
  - Pre-commit: `(asdf:test-system :sibyl)`

---

- [x] 0-3. `eval-form` ツール — 安全な式評価

  **What to do**:
  - Lisp式を現在のイメージ内で評価するツールを定義
  - **重要**: 評価後に自動`compile`を適用（`sb-introspect`互換性のため）
  - `*package*`をsibyl関連パッケージにバインドして評価
  - 評価結果を文字列として返す（multiple-values対応）
  - タイムアウト機構（無限ループ防止）: `sb-ext:with-timeout`
  - 出力キャプチャ: `*standard-output*`と`*error-output*`をstring-streamにバインド
  - パラメータ: `form` (必須, 文字列), `package` (オプション, デフォルト "SIBYL"), `timeout` (オプション, デフォルト 30秒)

  **TDD**:
  - RED: `(eval-form "(+ 1 2)")` → "3"のテスト
  - RED: `(eval-form "(defun test-fn () 42)")` → 関数定義後にfboundp確認テスト
  - RED: タイムアウトテスト（`(eval-form "(loop)" :timeout 1)`）
  - GREEN: `sb-ext:with-timeout` + eval + compile実装
  - REFACTOR: エラーメッセージの改善

  **Must NOT do**:
  - 任意のシステムコマンド実行（shellツールがある）
  - パッケージの削除やdefpackageの評価
  - `(quit)`や`(sb-ext:exit)`の評価

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: SBCL内部（sb-ext:with-timeout）とセキュリティ考慮が必要
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 0-A (with Tasks 0-1, 0-2)
  - **Blocks**: [2-1, 2-2, 3-1]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/tools/builtin.lisp:52-69` — `shell`ツールのパターン。出力キャプチャの参考
  - `src/tools/protocol.lisp:142-175` — `execute-tool`のCondition System使用パターン

  **External References**:
  - SBCL `sb-ext:with-timeout`: タイムアウト機構
  - CLHS `EVAL`: https://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm
  - CLHS `COMPILE`: https://www.lispworks.com/documentation/HyperSpec/Body/f_cmp.htm

  **Acceptance Criteria**:
  - [ ] `(fiveam:run 'eval-form-tests)` → `(fiveam:results-status *)` → T
  - [ ] 定義された関数が自動的にcompileされる（`compiled-function-p`で確認）
  - [ ] タイムアウトが正しく機能する

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: eval-form evaluates simple expression
    Tool: Bash (sbcl --eval)
    Steps:
      1. (ql:quickload :sibyl)
      2. (sibyl.tools:execute-tool "eval-form" '(("form" . "(+ 1 2 3)")))
      3. Assert: result contains "6"
    Expected Result: Evaluated result as string
    Evidence: SBCL stdout captured

  Scenario: eval-form compiles defined functions
    Tool: Bash (sbcl --eval)
    Steps:
      1. (sibyl.tools:execute-tool "eval-form"
           '(("form" . "(defun sibyl-test-fn-12345 () 42)")))
      2. Assert: (compiled-function-p #'sibyl-test-fn-12345) → T
      3. (fmakunbound 'sibyl-test-fn-12345)  ; cleanup
    Expected Result: Function both defined and compiled
    Evidence: SBCL stdout captured

  Scenario: eval-form times out on infinite loop
    Tool: Bash (sbcl --eval)
    Steps:
      1. (sibyl.tools:execute-tool "eval-form"
           '(("form" . "(loop)") ("timeout" . 2)))
      2. Assert: result contains "timeout" or error indication
    Expected Result: Timeout triggered, no hang
    Evidence: SBCL stdout captured (within 5s)
  ```

  **Commit**: YES (groups with 0-1, 0-2)
  - Message: `feat(tools): add eval-form tool with auto-compile and timeout safety`
  - Files: `src/tools/lisp-tools.lisp`, `tests/sexp-tools-test.lisp`
  - Pre-commit: `(asdf:test-system :sibyl)`

---

- [x] 0-4. `macroexpand-form` ツール — マクロ展開

  **What to do**:
  - Lisp式のマクロ展開結果を返すツール
  - `macroexpand-1`（1段階）と`macroexpand`（完全展開）の両方をサポート
  - 特にSibyl自身の`deftool`マクロの展開結果を理解するのに重要
  - パラメータ: `form` (必須), `full` (オプション, デフォルトt = 完全展開)
  - 結果はpretty-printed S-expressionとして返す

  **TDD**:
  - RED: `(macroexpand-form "(deftool ...)")`でdeftoolの展開結果が返るテスト
  - GREEN: `macroexpand`/`macroexpand-1` + `pprint`実装
  - REFACTOR: 出力の可読性改善

  **Must NOT do**:
  - 展開結果の評価（それはeval-formの役割）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: macroexpand呼び出しのラッパーなので比較的シンプル
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 0-B (with Task 0-5)
  - **Blocks**: [1-2]
  - **Blocked By**: None（ただしeval-formと同じファイルに書くなら0-3の後）

  **References**:

  **Pattern References**:
  - `src/tools/protocol.lisp:51-79` — `deftool`マクロ定義。展開対象として最重要
  - `src/tools/builtin.lisp:10-17` — シンプルなツール定義パターン

  **External References**:
  - CLHS `MACROEXPAND`: https://www.lispworks.com/documentation/HyperSpec/Body/f_mexp_.htm

  **Acceptance Criteria**:
  - [ ] `(fiveam:run 'macroexpand-form-tests)` → `(fiveam:results-status *)` → T
  - [ ] deftoolマクロの展開結果にmake-tool, register-toolが含まれること

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: macroexpand-form expands deftool macro
    Tool: Bash (sbcl --eval)
    Steps:
      1. (ql:quickload :sibyl)
      2. (sibyl.tools:execute-tool "macroexpand-form"
           '(("form" . "(sibyl.tools:deftool \"test\" (:description \"test\" :parameters ()) (+ 1 1))")))
      3. Assert: result contains "MAKE-TOOL" and "REGISTER-TOOL"
    Expected Result: Expanded form showing tool construction
    Evidence: SBCL stdout captured
  ```

  **Commit**: YES (groups with 0-5)
  - Message: `feat(tools): add macroexpand-form tool for macro understanding`
  - Files: `src/tools/lisp-tools.lisp`, `tests/sexp-tools-test.lisp`
  - Pre-commit: `(asdf:test-system :sibyl)`

---

- [x] 0-5. `package-symbols` ツール — パッケージ探索

  **What to do**:
  - パッケージ内のシンボル一覧を返すツール
  - 外部シンボル（exported）のみ、または全シンボルのモード
  - 各シンボルの種類（function, variable, class, macro）を付加
  - Sibylの全パッケージ: `sibyl`, `sibyl.llm`, `sibyl.tools`, `sibyl.agent`, `sibyl.repl`, `sibyl.util`, `sibyl.conditions`
  - パラメータ: `package` (必須), `external-only` (オプション, デフォルトt)

  **TDD**:
  - RED: `(package-symbols "SIBYL.TOOLS")` → `deftool`, `execute-tool`等のexported symbolsが含まれるテスト
  - GREEN: `do-external-symbols`/`do-symbols`で実装
  - REFACTOR: ソート、フィルタリング

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: パッケージAPI呼び出しのラッパー
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 0-B (with Task 0-4)
  - **Blocks**: [1-1]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/packages.lisp` — Sibylのパッケージ定義。exportされるシンボル一覧
  - `src/tools/builtin.lisp:34-46` — `list-directory`パターン。リスト返却の参考

  **Acceptance Criteria**:
  - [ ] `(fiveam:run 'package-symbols-tests)` → `(fiveam:results-status *)` → T
  - [ ] `package-symbols "SIBYL.TOOLS"` → `deftool`が含まれること

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: package-symbols lists sibyl.tools exports
    Tool: Bash (sbcl --eval)
    Steps:
      1. (ql:quickload :sibyl)
      2. (sibyl.tools:execute-tool "package-symbols" '(("package" . "SIBYL.TOOLS")))
      3. Assert: result contains "DEFTOOL", "EXECUTE-TOOL", "FIND-TOOL"
    Expected Result: Exported symbols with type annotations
    Evidence: SBCL stdout captured
  ```

  **Commit**: YES (groups with 0-4)
  - Message: `feat(tools): add package-symbols tool for package exploration`
  - Files: `src/tools/lisp-tools.lisp`, `tests/sexp-tools-test.lisp`
  - Pre-commit: `(asdf:test-system :sibyl)`

---

- [x] 0-6. ASDFシステム定義の更新とテストスイート統合

  **What to do**:
  - `sibyl.asd`にPhase 0の新ファイルを追加
    - `src/tools/lisp-tools.lisp`をtoolsモジュールに追加
  - `sibyl/tests`にテストファイルを追加
    - `tests/sexp-tools-test.lisp`
  - `src/packages.lisp`にリスプツールの新しいexportを追加
  - 全テストスイートが通ることを確認

  **Must NOT do**:
  - 既存のファイル構造を変更（追加のみ）

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after Wave 0-A and 0-B complete)
  - **Blocks**: [Phase 1 全体]
  - **Blocked By**: [0-1, 0-2, 0-3, 0-4, 0-5]

  **References**:

  **Pattern References**:
  - `sibyl.asd:1-52` — 現在のシステム定義。新ファイルの追加位置
  - `src/packages.lisp` — パッケージ定義。新しいexportの追加

  **Acceptance Criteria**:
  - [ ] `(ql:quickload :sibyl)` がエラーなしで完了
  - [ ] `(asdf:test-system :sibyl)` → 全テストPASS
  - [ ] 新ツール5つ全てが`*tool-registry*`に登録されている

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Full system loads and all tests pass
    Tool: Bash (sbcl)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl)' --eval '(asdf:test-system :sibyl)' --eval '(sb-ext:exit)'
      2. Assert: exit code 0
      3. Assert: stdout contains "Did X checks" with 0 failures
      4. sbcl --eval '(ql:quickload :sibyl)' --eval '(format t "~a" (length (sibyl.tools:list-tools)))' --eval '(sb-ext:exit)'
      5. Assert: tool count = original count + 5 new tools
    Expected Result: System loads cleanly, all tests pass, all tools registered
    Evidence: SBCL stdout captured
  ```

  **Commit**: YES
  - Message: `chore(system): integrate Phase 0 Lisp-aware tools into ASDF system`
  - Files: `sibyl.asd`, `src/packages.lisp`
  - Pre-commit: `(asdf:test-system :sibyl)`

---

### ═══════════════════════════════════════════
### PHASE 1: Self-Understanding (自己理解)
### ═══════════════════════════════════════════
###
### Sibylが自分自身のアーキテクチャを把握する能力。
### 「自分が何者であるか」を理解してから修正に移る。
### ═══════════════════════════════════════════

- [x] 1-1. `codebase-map` ツール — 自己アーキテクチャマッピング

  **What to do**:
  - Sibylの全ソースファイルを`read-sexp`で読み、アーキテクチャマップを生成
  - 出力: モジュール→ファイル→定義（defun/defmethod/defclass/deftool/defvar）の階層構造
  - 各定義にパッケージ、引数リスト、docstringを付加
  - `sibyl.asd`のcomponents定義を解析してモジュール構造を取得
  - パラメータ: `detail-level` (オプション: "summary" / "full", デフォルト "summary")

  **TDD**:
  - RED: `codebase-map`でSibyl自身を解析 → `agent-step`が`src/agent/core.lisp`にあることが認識されるテスト
  - GREEN: `read-sexp` + ASDFシステム情報を組み合わせた実装
  - REFACTOR: 出力フォーマットの最適化

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1-A (with Task 1-2)
  - **Blocks**: [1-3, 2-1]
  - **Blocked By**: [0-6]

  **References**:

  **Pattern References**:
  - `sibyl.asd:18-37` — ASDFシステム定義。モジュール構造の情報源
  - Task 0-1の`read-sexp`ツール — ファイルごとのフォーム取得に使用

  **Acceptance Criteria**:
  - [ ] `codebase-map` → Sibylの全モジュール（llm, tools, agent, repl）が認識される
  - [ ] 各モジュール内の主要な定義が列挙される
  - [ ] `(fiveam:run 'codebase-map-tests)` → PASS

  **Commit**: YES
  - Message: `feat(tools): add codebase-map for self-architecture awareness`
  - Files: `src/tools/lisp-tools.lisp`, `tests/sexp-tools-test.lisp`

---

- [x] 1-2. `who-calls` ツール — 呼び出し関係解析

  **What to do**:
  - `sb-introspect:who-calls`をラップした呼び出し関係ツール
  - 指定された関数を呼び出している全ての関数を列挙
  - 逆方向も: 指定された関数が呼び出している関数（`sb-introspect:who-calls`の逆引き）
  - **注意**: コンパイル済みコードのみ対象。evalだけで定義された関数は検出不可
  - パラメータ: `function` (必須, "sibyl.tools:execute-tool"形式), `direction` (オプション: "callers" / "callees", デフォルト "callers")

  **TDD**:
  - RED: `who-calls "sibyl.tools:execute-tool" :direction "callers"` → `agent-step`のコンテキスト内の呼び出しが見つかるテスト
  - GREEN: `sb-introspect:who-calls`ラッパー実装
  - REFACTOR: 出力の整理、外部ライブラリの呼び出しのフィルタリング

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1-A (with Task 1-1)
  - **Blocks**: [1-3]
  - **Blocked By**: [0-6]

  **References**:

  **Pattern References**:
  - `src/agent/core.lisp:93-137` — `agent-step`メソッド。`execute-tool-call`を呼び出す。テスト対象の参考

  **External References**:
  - `sb-introspect:who-calls`: http://www.sbcl.org/manual/#sb_002dintrospect
  - 注意: コンパイル済みコードのみ対象

  **Acceptance Criteria**:
  - [ ] `who-calls "sibyl.tools:execute-tool-call"` → `agent-step`が結果に含まれる
  - [ ] `(fiveam:run 'who-calls-tests)` → PASS

  **Commit**: YES (groups with 1-1)
  - Message: `feat(tools): add who-calls tool for call-graph analysis`

---

- [x] 1-3. `system-prompt`への自己認識の組み込み

  **What to do**:
  - `*default-system-prompt*`（`src/agent/core.lisp:44-52`）を拡張
  - Sibylが自身のアーキテクチャを認識していることをプロンプトに反映
  - `codebase-map`の結果をsystem-promptに動的に組み込むオプション
  - 「あなたは自身のソースコードにアクセスし、理解し、修正できるLispエージェントです」
  - 利用可能なLisp-awareツール群のリストをプロンプトに含める

  **TDD**:
  - RED: エージェント初期化時にsystem-promptにツール情報が含まれるテスト
  - GREEN: `make-agent`でcodebase-map結果をsystem-promptに追加
  - REFACTOR: プロンプトの最適化（トークン節約）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after Wave 1-A)
  - **Blocks**: [Phase 2]
  - **Blocked By**: [1-1, 1-2]

  **References**:

  **Pattern References**:
  - `src/agent/core.lisp:44-52` — 現在の`*default-system-prompt*`。拡張対象
  - `src/agent/core.lisp:58-69` — `make-agent`。system-prompt引数の処理

  **Acceptance Criteria**:
  - [ ] `(agent-system-prompt (make-agent ...))` にLisp-awareツールの情報が含まれる
  - [ ] プロンプトに「自己修正可能」である旨の記述がある
  - [ ] `(fiveam:run 'system-prompt-tests)` → PASS

  **Commit**: YES
  - Message: `feat(agent): embed self-awareness into system prompt`
  - Files: `src/agent/core.lisp`

---

### ═══════════════════════════════════════════
### PHASE 2: Safe Self-Modification (安全な自己修正)
### ═══════════════════════════════════════════
###
### Sibylが自分自身のコードを安全に修正できる能力。
### Condition Systemによるロールバックが核心。
### ═══════════════════════════════════════════

- [x] 2-1. `safe-redefine` ツール — 安全な関数再定義

  **What to do**:
  - 関数/メソッドを再定義するツール。ロールバック機構付き
  - **ロールバック設計**:
    1. 再定義前に`fdefinition`で現在の関数オブジェクトを保存
    2. 新しい定義を`eval` + `compile`
    3. 失敗時はCondition Systemで`restore-definition`リスタートを提供
    4. リスタートが呼ばれたら保存した関数オブジェクトで`(setf fdefinition)`
  - **クロージャ安全性チェック**:
    - 再定義対象の関数が`#'function`形式でキャプチャされていないか`who-calls`で確認
    - 警告がある場合はCondition Systemで通知（`sibyl-warning`）
  - パラメータ: `name` (必須, "sibyl.tools:find-tool"形式), `new-definition` (必須, S-expression文字列), `force` (オプション, クロージャ警告を無視)

  **TDD**:
  - RED: テスト用関数を定義 → `safe-redefine`で再定義 → 新定義が動作することを確認
  - RED: 不正な定義で再定義 → `restore-definition`リスタートで元に戻ることを確認
  - GREEN: fdefinition保存 + eval + compile + Condition Systemラッパー実装
  - REFACTOR: エラーメッセージの改善

  **Must NOT do**:
  - ファイルへの書き込み（それは`sync-to-file`の役割）
  - 他のパッケージの関数の再定義（Sibylのパッケージ群のみ）

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: Condition Systemの高度な使用、CLOSメソッド再定義、コンパイル制御が必要
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (Wave 2-A)
  - **Blocks**: [2-2, 2-3]
  - **Blocked By**: [1-3]

  **References**:

  **Pattern References**:
  - `src/tools/protocol.lisp:142-175` — `execute-tool`のCondition System使用。restart-caseパターンの参考
  - `src/conditions.lisp` — 既存のCondition階層。新しいconditionの追加場所

  **External References**:
  - CLHS `FDEFINITION`: 関数オブジェクトの取得/設定
  - CLHS `COMPILE`: 関数のコンパイル
  - CLHS `RESTART-CASE`: リスタートの定義
  - Metisからの知見: `function-lambda-expression`はNILを返す場合があるため、ソースではなく関数オブジェクトを保存すべき

  **Acceptance Criteria**:
  - [ ] テスト用関数の再定義が成功し、新定義が動作する
  - [ ] 不正な再定義時に`restore-definition`リスタートが利用可能
  - [ ] リスタート後に元の関数が復元される
  - [ ] 再定義後の関数が`compiled-function-p`でTを返す
  - [ ] `(fiveam:run 'safe-redefine-tests)` → PASS

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: safe-redefine successfully redefines a function
    Tool: interactive_bash (sbcl REPL)
    Steps:
      1. (ql:quickload :sibyl)
      2. (defun test-target () "original")
      3. (sibyl.tools:execute-tool "safe-redefine"
           '(("name" . "test-target")
             ("new-definition" . "(defun test-target () \"modified\")")))
      4. Assert: (test-target) => "modified"
      5. Assert: (compiled-function-p #'test-target) => T
    Expected Result: Function redefined and compiled
    Evidence: REPL output captured

  Scenario: safe-redefine rolls back on error
    Tool: interactive_bash (sbcl REPL)
    Steps:
      1. (defun test-target-2 () "original")
      2. (handler-bind ((error (lambda (c)
                                 (invoke-restart 'sibyl.tools::restore-definition))))
           (sibyl.tools:execute-tool "safe-redefine"
             '(("name" . "test-target-2")
               ("new-definition" . "(defun test-target-2 () (/ 1 0))"))))
         ;; Note: the definition itself might not error, but we test the mechanism
      3. Verify restore-definition restart exists in the restart list
    Expected Result: Rollback mechanism available via condition system
    Evidence: REPL output captured
  ```

  **Commit**: YES
  - Message: `feat(tools): add safe-redefine with condition-system rollback`
  - Files: `src/tools/lisp-tools.lisp`, `src/conditions.lisp`, `tests/sexp-tools-test.lisp`

---

- [x] 2-2. `sync-to-file` ツール — インメモリ→ファイル同期

  **What to do**:
  - インメモリで再定義された関数/メソッドの新定義をファイルに書き戻すツール
  - **戦略**: テキストレベルの精密編集（READはコメントを捨てるため）
    1. `read-sexp`で対象定義の行範囲を取得
    2. ファイルの該当行範囲を新しいS式テキストで置換
    3. 置換前のファイル内容をバックアップ（メモリ内に保持）
    4. 失敗時はCondition Systemでバックアップから復元
  - パラメータ: `name` (必須, シンボル名), `file` (必須, ファイルパス), `new-source` (必須, S-expression文字列)
  - **重要**: コメントや空行を可能な限り保持する

  **TDD**:
  - RED: テスト用ファイルを作成 → `sync-to-file`で定義を置換 → ファイル内容の確認
  - GREEN: 行範囲ベースの置換実装
  - REFACTOR: コメント保持の改善

  **Must NOT do**:
  - ファイル全体の上書き（該当定義のみ置換）
  - write-fileツールの使用（より精密な編集が必要）

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after 2-1)
  - **Blocks**: [2-3]
  - **Blocked By**: [2-1]

  **References**:

  **Pattern References**:
  - Task 0-1の`read-sexp` — 定義の行範囲取得に使用
  - `src/tools/builtin.lisp:19-32` — `write-file`の実装。ファイル書き込みの参考（ただしこれは全文上書き）

  **Acceptance Criteria**:
  - [ ] テスト用ファイルの特定定義のみが置換される
  - [ ] 置換前後でファイルの他の部分が変更されない
  - [ ] コメントが保持される（少なくとも該当定義外のコメント）
  - [ ] `(fiveam:run 'sync-to-file-tests)` → PASS

  **Commit**: YES
  - Message: `feat(tools): add sync-to-file for precise definition-level file editing`

---

- [ ] 2-3. `asdf-reload-protection` — ASDF リロード保護

  **What to do**:
  - `asdf:perform`に`:around`メソッドを追加し、インメモリ修正済みファイルの意図しないリロードを防止
  - 修正済みファイルのリストを管理する特殊変数`*modified-files*`
  - `safe-redefine` + `sync-to-file`が成功したら`*modified-files*`から除外
  - 保護状態を確認・解除するユーティリティ

  **TDD**:
  - RED: ファイルを`*modified-files*`に追加 → `ql:quickload`で保護されることをテスト
  - GREEN: `:around`メソッド + `*modified-files*`管理実装
  - REFACTOR: 保護のスコープ調整

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after 2-2)
  - **Blocks**: [Phase 3]
  - **Blocked By**: [2-2]

  **References**:

  **External References**:
  - ASDF Manual: `perform` generic function
  - Metisからの知見: ASDF `:around`メソッドによるリロード保護パターン

  **Acceptance Criteria**:
  - [ ] `*modified-files*`に含まれるファイルが`ql:quickload`でリロードされない
  - [ ] 保護の追加・解除が正しく動作する
  - [ ] `(fiveam:run 'asdf-protection-tests)` → PASS

  **Commit**: YES
  - Message: `feat(system): add ASDF reload protection for in-memory modifications`

---

### ═══════════════════════════════════════════
### PHASE 3: Self-Testing via TDD (自己テスト)
### ═══════════════════════════════════════════
###
### Sibylが自分の変更に対してテストファーストで検証する能力。
### 「テストを書いてから実装する」サイクルをLLMが自律的に回す。
### ═══════════════════════════════════════════

- [ ] 3-1. `run-tests` ツール — FiveAM テスト実行

  **What to do**:
  - FiveAMテストをプログラム的に実行し、結果を構造化データとして返すツール
  - `fiveam:run`（データ返却）を使用、`fiveam:run!`（出力表示）ではない
  - `fiveam:results-status`でパス/フェイル判定
  - 個別テスト、テストスイート、全テストの3モード
  - 結果: パス数、フェイル数、各フェイルの詳細（テスト名、期待値、実際値）
  - パラメータ: `suite` (オプション, デフォルトで全テスト), `test` (オプション, 特定テスト名)

  **TDD**:
  - RED: `run-tests`でsibyl-testsスイートを実行 → 結果にpass/fail数が含まれるテスト
  - GREEN: `fiveam:run` + `fiveam:results-status` + 結果パース実装
  - REFACTOR: 結果フォーマットの改善

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3-A (with Task 3-2)
  - **Blocks**: [3-3]
  - **Blocked By**: [2-3]

  **References**:

  **Pattern References**:
  - `tests/suite.lisp` — FiveAMスイート定義。テスト実行の既存パターン
  - `sibyl.asd:49-51` — テスト実行の設定

  **External References**:
  - FiveAM: `fiveam:run` vs `fiveam:run!`の違い
  - `fiveam:results-status`: 結果判定関数
  - Metisからの知見: `(let ((*standard-output* (make-broadcast-stream))) ...)`で出力を抑制

  **Acceptance Criteria**:
  - [ ] `run-tests` → pass/fail数を含む構造化結果
  - [ ] 意図的に失敗するテストを追加 → fail数が正しくカウントされる
  - [ ] `(fiveam:run 'run-tests-tests)` → PASS

  **Commit**: YES
  - Message: `feat(tools): add run-tests tool for programmatic FiveAM execution`

---

- [ ] 3-2. `write-test` ツール — テストコード生成・登録

  **What to do**:
  - FiveAMテストケースを生成し、ファイルに書き込み、現在のイメージに登録するツール
  - テスト名、スイート、テスト本体をパラメータとして受け取る
  - 生成されたテストコードを`eval` + `compile`でイメージに登録
  - テストファイルに`sync-to-file`的に追記
  - パラメータ: `name` (必須), `suite` (オプション, デフォルト "sibyl-tests"), `body` (必須, S-expression文字列)

  **TDD**:
  - RED: `write-test`でテストを生成 → そのテストが`run-tests`で実行可能であることを確認
  - GREEN: FiveAM `def-test`形式のコード生成 + eval + ファイル追記
  - REFACTOR: テスト名の重複チェック

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: FiveAMのAPI理解、コード生成、ファイル追記の組み合わせ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3-A (with Task 3-1)
  - **Blocks**: [3-3]
  - **Blocked By**: [2-3]

  **References**:

  **Pattern References**:
  - `tests/tools-test.lisp` — 既存のテストコード。テスト定義パターン
  - Task 2-2の`sync-to-file` — ファイル追記に使用

  **Acceptance Criteria**:
  - [ ] 生成されたテストがFiveAMで認識・実行される
  - [ ] テストファイルに正しいフォーマットで追記される
  - [ ] `(fiveam:run 'write-test-tests)` → PASS

  **Commit**: YES (groups with 3-1)
  - Message: `feat(tools): add write-test tool for programmatic test generation`

---

- [ ] 3-3. TDDオーケストレーション — RED→GREEN→REFACTORサイクル

  **What to do**:
  - TDDサイクルをLLMが自律的に回すためのワークフローをsystem-promptに組み込む
  - エージェントの行動指針:
    1. まず`write-test`でテストを書く
    2. `run-tests`で失敗を確認（RED）
    3. `safe-redefine`で最小実装（GREEN）
    4. `run-tests`でパスを確認
    5. `safe-redefine`でリファクタ（REFACTOR）
    6. `run-tests`で再確認
    7. `sync-to-file`でファイルに永続化
  - Condition System: テストが失敗したらリスタート→修正→再テストのループ
  - system-promptにTDDワークフローの手順を記述

  **TDD**:
  - RED: TDDワークフローのテスト（モック使用）— エージェントがテスト→実装→テストの順で動くことを確認
  - GREEN: system-prompt拡張 + ワークフロー検証実装
  - REFACTOR: プロンプト最適化

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: エージェントのメタ行動を設計する高度なタスク
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after Wave 3-A)
  - **Blocks**: [Phase 4]
  - **Blocked By**: [3-1, 3-2]

  **References**:

  **Pattern References**:
  - `src/agent/core.lisp:44-52` — system-prompt。TDDワークフローの追加場所
  - `src/agent/core.lisp:93-137` — `agent-step`。ツール呼び出しの再帰ループ
  - Tasks 2-1, 3-1, 3-2のツール — TDDサイクルで使用

  **Acceptance Criteria**:
  - [ ] system-promptにTDDワークフローの手順が含まれる
  - [ ] エージェントが「テストを先に書く」ことを理解している（LLM応答から確認）
  - [ ] `(fiveam:run 'tdd-orchestration-tests)` → PASS

  **Commit**: YES
  - Message: `feat(agent): integrate TDD workflow into agent behavior`

---

### ═══════════════════════════════════════════
### PHASE 4: Human-Supervised Self-Improvement
### ═══════════════════════════════════════════
###
### 人間がタスクを指示し、SibylがTDDで自律実装する。
### 最初のマイルストーン: 既存ツールの改善。
### ═══════════════════════════════════════════

- [ ] 4-1. 自己改善REPLコマンド `/improve`

  **What to do**:
  - REPLに`/improve`コマンドを追加
  - 使い方: `/improve grepツールに--exclude-dirオプションを追加`
  - フロー:
    1. 対象ツールのコードを`read-sexp`で読み取り
    2. `who-calls`で影響範囲を確認
    3. テストを`write-test`で作成（RED）
    4. `safe-redefine`で実装（GREEN）
    5. `run-tests`で全テストパス確認
    6. `sync-to-file`でファイルに永続化
    7. 結果を人間に報告
  - 人間の承認なしにファイルには書き込まない（`--auto-commit`フラグで解除可）

  **TDD**:
  - RED: `/improve`コマンドがREPLコマンドとして認識されるテスト
  - GREEN: REPL拡張 + エージェントへの指示パイプライン実装
  - REFACTOR: エラーハンドリング改善

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (Wave 4-A)
  - **Blocks**: [4-2]
  - **Blocked By**: [3-3]

  **References**:

  **Pattern References**:
  - `src/repl.lisp:10-17` — `*repl-commands*`。新コマンドの追加場所
  - `src/repl.lisp:23-61` — `handle-repl-command`。コマンドハンドラの実装パターン
  - `src/repl.lisp:97-123` — メインループ。コマンドディスパッチの流れ

  **Acceptance Criteria**:
  - [ ] `/improve`コマンドがREPLで認識される
  - [ ] 自然言語のタスク記述からエージェントが自律的にTDDサイクルを回す
  - [ ] 全テストがパスした状態で報告される
  - [ ] `--auto-commit`なしではファイル書き込み前に確認プロンプトが出る

  **Commit**: YES
  - Message: `feat(repl): add /improve command for human-supervised self-improvement`

---

- [ ] 4-2. ブートストラップ実証 — Sibylが自分のgrepツールを改善する

  **What to do**:
  - **これがPhase 4の最重要マイルストーン**
  - Sibylに以下のタスクを与える:
    「grepツールに`--exclude-dir`オプションを追加せよ。テストファーストで実装すること」
  - Sibylが自律的に:
    1. 現在のgrepツールのコードを`read-sexp`で読む
    2. `who-calls`で影響範囲を確認
    3. FiveAMテストを`write-test`で先に書く（`--exclude-dir`のテストケース）
    4. `run-tests`でRED確認
    5. `safe-redefine`でgrepツールを再定義
    6. `run-tests`でGREEN確認
    7. `sync-to-file`でファイルに書き戻し
    8. 人間に結果を報告
  - **人間は結果をレビューし、必要なら指示を修正して再実行**

  **Must NOT do**:
  - 自動マージ（人間のレビュー後）
  - grepツール以外の修正

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: 自律的なゴール達成。E2Eの自己修正フロー
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (after 4-1)
  - **Blocks**: [Phase 5]
  - **Blocked By**: [4-1]

  **References**:

  **Pattern References**:
  - `src/tools/builtin.lisp:75-92` — 現在の`grep`ツール。修正対象
  - 全Phase 0-3のツール — 自己修正に使用

  **Acceptance Criteria**:
  - [ ] Sibylが自律的にgrepツールのテストを作成した
  - [ ] Sibylが自律的にgrepツールを再定義した
  - [ ] 新しいgrepツールに`--exclude-dir`オプションが追加された
  - [ ] 全テスト（既存 + 新規）がパスする
  - [ ] 変更がファイルに永続化された
  - [ ] 人間がレビュー可能な差分が表示される

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Sibyl improves its own grep tool via /improve
    Tool: interactive_bash (sbcl REPL)
    Preconditions: Full Phase 0-3 tools loaded
    Steps:
      1. Start Sibyl REPL
      2. Input: /improve grepツールに--exclude-dirパラメータを追加。.gitディレクトリを除外できるようにする
      3. Observe: Agent reads grep tool via read-sexp
      4. Observe: Agent writes test via write-test
      5. Observe: Agent runs test → RED (fails)
      6. Observe: Agent redefines grep via safe-redefine
      7. Observe: Agent runs test → GREEN (passes)
      8. Observe: Agent syncs to file
      9. Assert: (sibyl.tools:execute-tool "grep"
           '(("pattern" . "defun") ("path" . ".") ("exclude-dir" . ".git")))
         → results do not contain .git/ paths
    Expected Result: Grep tool successfully enhanced by Sibyl itself
    Evidence: Full REPL session captured
  ```

  **Commit**: YES
  - Message: `milestone(bootstrap): Sibyl successfully improves its own grep tool via TDD`

---

### ═══════════════════════════════════════════
### PHASE 5: Semi-Autonomous Development
### ═══════════════════════════════════════════
###
### Sibylが自ら改善案を提案し、承認後に実装する。
### 人間の監視はマイルストーンレベルに緩和。
### ═══════════════════════════════════════════

- [ ] 5-1. `suggest-improvements` ツール — 改善提案機能

  **What to do**:
  - Sibylが自身のコードベースを分析し、改善提案を生成するツール
  - 分析観点:
    - コード重複の検出
    - 未実装のTODOコメント
    - テストカバレッジのギャップ（テストのない関数）
    - ドキュメント/docstringの欠如
    - エラーハンドリングの不足
  - `codebase-map` + `read-sexp` + `who-calls`の結果をLLMに渡して分析
  - 提案の優先度付け: High / Medium / Low
  - パラメータ: `scope` (オプション: "all" / "module-name" / "file-path")

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 5-A (with Task 5-2)
  - **Blocks**: [5-3]
  - **Blocked By**: [4-2]

  **References**:
  - Tasks 1-1, 1-2 (self-understanding tools)
  - All Phase 0 tools

  **Acceptance Criteria**:
  - [ ] Sibylが自身のコードに対して実行可能な改善提案を生成する
  - [ ] 各提案に優先度と理由が付与される
  - [ ] 提案がランダムではなく、コード分析に基づいている

  **Commit**: YES
  - Message: `feat(agent): add suggest-improvements for self-analysis`

---

- [ ] 5-2. `/review` REPLコマンド — 提案レビュー・承認フロー

  **What to do**:
  - 改善提案をリスト表示し、承認/拒否/修正できるREPLコマンド
  - `/review` → 提案一覧表示
  - `/review approve 1` → 提案1を承認し、`/improve`フローで自動実装
  - `/review reject 2` → 提案2を拒否
  - `/review modify 3 "条件を追加して"` → 提案3を修正して承認
  - 承認バッチ処理: `/review approve-all` （複数提案の一括承認）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 5-A (with Task 5-1)
  - **Blocks**: [5-3]
  - **Blocked By**: [4-2]

  **Acceptance Criteria**:
  - [ ] `/review`コマンドが提案を表示する
  - [ ] `approve`で自動実装フローが起動する
  - [ ] 全テストがパスした状態で完了する

  **Commit**: YES (groups with 5-1)
  - Message: `feat(repl): add /review command for improvement proposal workflow`

---

- [ ] 5-3. 自律実装サイクルの実証

  **What to do**:
  - Sibylが:
    1. `suggest-improvements`で改善案を提案
    2. 人間が`/review approve`で承認
    3. 自動TDDサイクルで実装
    4. 結果報告
  - このサイクルを3回以上成功させる
  - 各サイクルで異なる種類の改善（ツール強化、テスト追加、ドキュメント改善）

  **Recommended Agent Profile**:
  - **Category**: `deep`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Blocked By**: [5-1, 5-2]

  **Acceptance Criteria**:
  - [ ] 3つ以上の改善が正常に実装された
  - [ ] 各改善がテスト付きである
  - [ ] コードベースが改善前より良い状態になっている

  **Commit**: YES
  - Message: `milestone(semi-autonomous): Sibyl proposes and implements improvements`

---

### ═══════════════════════════════════════════
### PHASE 6: Self-Evolving Agent (自己進化)
### ═══════════════════════════════════════════
###
### Sibylが自身の限界を認識し、改善計画を立てて実行する。
### ⚠️ スコープ限定: 提案→承認フロー維持。完全自律は対象外。
### ═══════════════════════════════════════════

- [ ] 6-1. `self-assess` ツール — 自己限界認識

  **What to do**:
  - Sibylが自身の現在の能力と限界を評価するツール
  - 評価観点:
    - 現在のツールセットでできること/できないこと
    - 過去のタスクの成功/失敗パターン（記録が必要）
    - コードベースの複雑さメトリクス（行数、関数数、循環的複雑度の簡易版）
    - テストカバレッジの概算
  - タスク実行ログの保存機構（成功/失敗/理由）

  **Must NOT do**:
  - 自律的なアーキテクチャ変更の実行
  - 承認なしの大規模変更

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 6-A (with Task 6-2)
  - **Blocks**: [6-3]
  - **Blocked By**: [5-3]

  **Acceptance Criteria**:
  - [ ] 自己評価レポートが生成される
  - [ ] 具体的な能力と限界がリストされる
  - [ ] 過去のタスク実行ログが参照される

  **Commit**: YES
  - Message: `feat(agent): add self-assess tool for capability evaluation`

---

- [ ] 6-2. `improvement-plan` ツール — 改善計画生成

  **What to do**:
  - `self-assess`の結果から、次に取り組むべき改善の優先順位付きリストを生成
  - 各改善にリスク評価と推定効果を付与
  - 短期（次のセッション）/ 中期（次の週）/ 長期（将来）のカテゴリ分け
  - **人間の承認必須**: 計画は表示のみ。実行は`/review approve`経由

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 6-A (with Task 6-1)
  - **Blocks**: [6-3]
  - **Blocked By**: [5-3]

  **Acceptance Criteria**:
  - [ ] 改善計画が優先順位付きで生成される
  - [ ] 各項目にリスクと効果が付与される
  - [ ] 計画が`/review`フローに接続されている

  **Commit**: YES (groups with 6-1)
  - Message: `feat(agent): add improvement-plan for strategic self-evolution`

---

- [ ] 6-3. 自己進化デモンストレーション

  **What to do**:
  - 完全な自己進化サイクルのE2Eデモ:
    1. Sibylが`self-assess`で自己評価
    2. `improvement-plan`で計画生成
    3. 人間が計画をレビューし承認
    4. Sibylが承認された改善をTDDで実装
    5. 結果報告と次の`self-assess`
  - このサイクルが正常に1回以上完了すること

  **Recommended Agent Profile**:
  - **Category**: `deep`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Blocked By**: [6-1, 6-2]

  **Acceptance Criteria**:
  - [ ] 完全な自己進化サイクルが1回以上成功
  - [ ] 各ステップがログされている
  - [ ] 改善がコードベースの品質向上に貢献している

  **Commit**: YES
  - Message: `milestone(self-evolving): Sibyl completes first self-evolution cycle`

---

## Commit Strategy

| After Task | Message | Verification |
|------------|---------|--------------|
| 0-1 ~ 0-3 | `feat(tools): add Lisp-aware foundation tools (read-sexp, describe-symbol, eval-form)` | `(asdf:test-system :sibyl)` |
| 0-4 ~ 0-6 | `feat(tools): add macroexpand-form, package-symbols; integrate Phase 0` | `(asdf:test-system :sibyl)` |
| 1-1 ~ 1-2 | `feat(tools): add self-understanding tools (codebase-map, who-calls)` | `(asdf:test-system :sibyl)` |
| 1-3 | `feat(agent): embed self-awareness into system prompt` | `(asdf:test-system :sibyl)` |
| 2-1 ~ 2-3 | `feat(tools): add safe self-modification with condition-system rollback` | `(asdf:test-system :sibyl)` |
| 3-1 ~ 3-3 | `feat(tools/agent): add TDD self-testing infrastructure` | `(asdf:test-system :sibyl)` |
| 4-1 ~ 4-2 | `milestone(bootstrap): Sibyl self-improves its grep tool via TDD` | E2E demo |
| 5-1 ~ 5-3 | `milestone(semi-autonomous): Sibyl proposes and implements improvements` | 3+ improvements |
| 6-1 ~ 6-3 | `milestone(self-evolving): Sibyl completes first self-evolution cycle` | E2E cycle |

---

## Success Criteria

### Phase Gate Criteria

| Phase | Gate | Verification |
|-------|------|-------------|
| 0 | 5つのLisp-awareツールが動作 | `(length (sibyl.tools:list-tools))` = 元の数 + 5 |
| 1 | Sibylが自身のアーキテクチャを出力可能 | `codebase-map` output contains all modules |
| 2 | defunの再定義→ロールバック→ファイル同期が動作 | `safe-redefine` + `sync-to-file` + rollback test |
| 3 | TDDサイクルがプログラム的に回る | RED→GREEN→REFACTOR automated |
| 4 | **Sibylが自分のgrepツールをTDDで改善** | `/improve` E2E success |
| 5 | Sibylが3つ以上の改善を提案・実装 | `/review` cycle × 3 |
| 6 | 自己進化サイクルが1回完了 | `self-assess` → `improvement-plan` → implement → verify |

### Final Checklist
- [ ] 全テストがパスする: `(asdf:test-system :sibyl)`
- [ ] 全Phase 0-3ツールが登録されている
- [ ] Condition Systemによるロールバックが動作する
- [ ] TDDサイクルが自律的に回る
- [ ] Sibylが自分自身のコードを読み・理解し・修正し・テストできる
- [ ] 全変更がファイルに永続化される
- [ ] 人間の承認フローが機能する（Phase 4-6）
