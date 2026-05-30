#!/bin/bash
# PostToolUse hook: auto-format .lisp files written or edited by Claude
input=$(cat)
file=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')
[[ "$file" == *.lisp ]] || exit 0
[[ -f "$file" ]] || exit 0
emacs --batch "$file" \
  --eval "(setq-default indent-tabs-mode nil)" \
  --eval "(indent-region (point-min) (point-max))" \
  -f save-buffer 2>/dev/null
