# Sibyl

Common Lisp で実装するコーディングエージェントの研究プロトタイプ。

LLM（Claude / GPT）をバックエンドに、Lisp のマクロ・コンディションシステム・CLOS を活かしたエージェントアーキテクチャを探求する。

## 特徴

- **`deftool` マクロ** — ツールを宣言的に定義する Lisp DSL
- **コンディション + restarts** — ツール実行エラー時に `retry-tool` / `skip-tool` / `use-value` で対話的リカバリ
- **CLOS generic functions** — `complete` / `complete-with-tools` でプロバイダーを抽象化。新規プロバイダーはメソッド追加のみ
- **スレッドセーフ会話管理** — `bordeaux-threads` によるロック付き conversation
- **対話的 REPL** — エージェントとの会話、ツール一覧、履歴表示

## 要件

- [SBCL](http://www.sbcl.org/) (推奨) または他の Common Lisp 処理系
- [Quicklisp](https://www.quicklisp.org/)
- Anthropic API キーまたは OpenAI API キー

## セットアップ

```bash
# SBCL インストール (macOS)
brew install sbcl

# Quicklisp インストール
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
```

プロジェクトを Quicklisp のローカルプロジェクトとしてリンク:

```bash
ln -s /path/to/sibyl ~/quicklisp/local-projects/sibyl
```

## 使い方

```lisp
;; ロード
(ql:quickload :sibyl)

;; REPL 起動
(sibyl:with-config ()
  (sibyl:start-repl :client (sibyl:make-anthropic-client)))
```

環境変数で API キーを設定:

```bash
export ANTHROPIC_API_KEY="sk-ant-..."
# または
export OPENAI_API_KEY="sk-..."
```

### カスタムツールの定義

```lisp
(sibyl.tools:deftool "count-lines"
    (:description "ファイルの行数を数える"
     :parameters ((:name "path" :type "string" :required t
                   :description "ファイルパス")))
  (let* ((path (getf args :path))
         (content (uiop:read-file-string path))
         (lines (count #\Newline content)))
    (format nil "~a: ~a 行" path lines)))
```

### REPL コマンド

| コマンド | 説明 |
|---------|------|
| `/help` | ヘルプ表示 |
| `/tools` | 登録済みツール一覧 |
| `/reset` | 会話リセット |
| `/history` | 会話履歴表示 |
| `/quit` | 終了 |

## プロジェクト構成

```
sibyl/
├── sibyl.asd                 # ASDF システム定義
├── src/
│   ├── packages.lisp         # パッケージ定義
│   ├── conditions.lisp       # コンディション階層
│   ├── config.lisp           # 設定管理
│   ├── util.lisp             # ユーティリティ
│   ├── llm/
│   │   ├── message.lisp      # メッセージ・会話データ構造
│   │   ├── client.lisp       # LLM クライアントプロトコル (CLOS)
│   │   └── providers.lisp    # Anthropic / OpenAI 実装
│   ├── tools/
│   │   ├── protocol.lisp     # deftool マクロ・レジストリ・実行
│   │   └── builtin.lisp      # 組込ツール
│   ├── agent/
│   │   ├── memory.lisp       # コンテキストウィンドウ管理
│   │   └── core.lisp         # エージェントループ
│   └── repl.lisp             # REPL インターフェース
└── tests/
    ├── suite.lisp            # テストスイート (FiveAM)
    ├── tools-test.lisp       # ツールテスト
    └── message-test.lisp     # メッセージテスト
```

## テスト

```lisp
(ql:quickload :sibyl/tests)
(asdf:test-system :sibyl)
```

## 依存ライブラリ

| ライブラリ | 用途 |
|-----------|------|
| [alexandria](https://common-lisp.net/project/alexandria/) | 汎用ユーティリティ |
| [dexador](https://github.com/fukamachi/dexador) | HTTP クライアント |
| [yason](https://github.com/phmarek/yason) | JSON パーサー |
| [cl-ppcre](https://edicl.github.io/cl-ppcre/) | 正規表現 |
| [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) | スレッド |
| [local-time](https://common-lisp.net/project/local-time/) | 時刻処理 |
| [fiveam](https://common-lisp.net/project/fiveam/) | テストフレームワーク |

## ライセンス

MIT
