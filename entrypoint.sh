#!/usr/bin/env bash
set -euo pipefail

# Ensure HOME is set (fallback to /tmp if running as non-standard user)
if [ -z "${HOME:-}" ]; then
    export HOME="/tmp"
fi

# Baked-in config/data
CONFIG_SOURCE="/emacs/.config/emacs"

# Optional per-user copy (mirrors your nvim pattern)
CONFIG_TARGET="$HOME/.config/emacs-docker"

safe_copy() {
  local src="$1" dst="$2"
  if [ ! -e "$dst" ]; then
    mkdir -p "$(dirname "$dst")"
    cp -a "$src" "$dst"
  fi
}

# Copy once, then run against the per-user copy
safe_copy "$CONFIG_SOURCE"  "$CONFIG_TARGET"

export XDG_CONFIG_HOME="$HOME/.config"

# Make elpaca strictly offline at runtime
export ELPACA_NO_AUTOINSTALL=t
export GIT_TERMINAL_PROMPT=0

exec emacs --init-directory $CONFIG_TARGET "$@"
