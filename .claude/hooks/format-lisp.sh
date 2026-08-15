#!/bin/bash
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PostToolUse hook: auto-format .lisp files written or edited by Claude
input=$(cat)
file=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')
[[ "$file" == *.lisp ]] || exit 0
[[ -f "$file" ]] || exit 0
emacs --batch "$file" \
  --eval "(setq-default indent-tabs-mode nil)" \
  --eval "(indent-region (point-min) (point-max))" \
  -f save-buffer 2>/dev/null
