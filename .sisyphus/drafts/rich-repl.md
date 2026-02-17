# Draft: Rich REPL Interface Enhancement

## Requirements (confirmed)
- LLM応答待ちの表現（スピナー / プログレス表示）
- Ctrl+C 二回でREPL終了
- その他のリッチ化を検討中

## Current Architecture (from research)
- **repl.lisp** (764行): メインループ。`read-line`で入力、`agent-run`でLLM呼出し、`format`で出力
- **ANSI色サポート済**: `format-colored-text`関数、`*use-colors*`トグル、ボックス描画バナー
- **ストリーミング未実装**: `http-post-json`は完全レスポンスを待つ。SSE/チャンク非対応
- **シグナル処理なし**: Ctrl+C/SIGINTのハンドリングゼロ。SBCLデフォルト挙動
- **スピナーなし**: LLM呼出し中にプログレス表示なし
- **bordeaux-threads利用可**: ロック/スレッド作成の基盤あり
- **フックシステム存在**: agent hookで:before-step, :after-step, :on-tool-call, :on-error

## Bug Found
- `*use-colors*`, `*command-count*`, `*command-history*`が`defvar`未定義
  - repl.lisp内で参照されているが宣言がない
  - Sibyl自己進化で追加されたが定義漏れ

## Technical Decisions
- (pending)

## Open Questions
- ストリーミング対応のスコープ
- 外部ライブラリ追加の可否（cl-readline等）
- テスト戦略

## Scope Boundaries
- INCLUDE: (pending)
- EXCLUDE: (pending)
