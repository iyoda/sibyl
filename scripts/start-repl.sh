#!/usr/bin/env bash
set -euo pipefail

MODEL_SPEC="${1:-gpt-5.2-codex}"
SESSION_ID="${2:-}"

if [[ "${MODEL_SPEC}" == ollama:* ]]; then
  OLLAMA_MODEL="${MODEL_SPEC#ollama:}"
  CLIENT_FORM="(sibyl:make-ollama-client :model \"${OLLAMA_MODEL}\")"
else
  case "${MODEL_SPEC}" in
    gpt-5.2-codex)
      CLIENT_FORM='(sibyl:make-openai-client :model "gpt-5.2-codex")'
      ;;
    gpt-5-mini)
      CLIENT_FORM='(sibyl:make-openai-client :model "gpt-5-mini")'
      ;;
    opus-4.6|claude-opus-4-6)
      CLIENT_FORM='(sibyl:make-anthropic-client :model "claude-opus-4-6")'
      ;;
    soonet-4.6|sonnet-4.6|claude-sonnet-4-6)
      CLIENT_FORM='(sibyl:make-anthropic-client :model "claude-sonnet-4-6")'
      ;;
    ollama|gpt-oss|gpt-oss:120b)
      CLIENT_FORM='(sibyl:make-ollama-client :model "gpt-oss:120b")'
      ;;
    *)
      cat >&2 <<'USAGE'
Usage: scripts/start-repl.sh [model] [session-id]

Models:
  gpt-5.2-codex
  gpt-5-mini
  opus-4.6
  soonet-4.6
  ollama
  ollama:<model>   (e.g. ollama:llama3:8b)
USAGE
      exit 1
      ;;
  esac
fi

if [[ -n "${SESSION_ID}" ]]; then
  REPL_FORM="(sibyl:start-repl :client ${CLIENT_FORM} :session-id \"${SESSION_ID}\")"
else
  REPL_FORM="(sibyl:start-repl :client ${CLIENT_FORM})"
fi

sbcl --eval '(ql:quickload :sibyl :silent t)' \
     --eval "(sibyl:with-config () ${REPL_FORM})"
