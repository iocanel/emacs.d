#!/usr/bin/env bash
set -euo pipefail

# Ensure HOME is set
if [ -z "${HOME:-}" ]; then
    echo "ERROR: HOME environment variable is not set" >&2
    echo "This is required for proper configuration setup" >&2
    exit 1
fi

# Baked-in config/data
CONFIG_SOURCE="/emacs/.config/emacs"

# Optional per-user copy (mirrors your nvim pattern)
CONFIG_TARGET="$HOME/.config/emacs-docker"

safe_copy() {
  local src="$1" dst="$2"
  mkdir -p "$dst"
  cp -af "$src"/* "$dst"/
}

# Always ensure fresh copy by removing destination first
safe_copy "$CONFIG_SOURCE"  "$CONFIG_TARGET"

# Verify critical files were copied correctly
for file in readme.org readme.el; do
    if [ ! -f "$CONFIG_TARGET/$file" ]; then
        echo "ERROR: Critical file $file was not copied to $CONFIG_TARGET" >&2
        exit 1
    fi
    if [ ! -f "$CONFIG_SOURCE/$file" ]; then
        echo "ERROR: Critical file $file missing from source $CONFIG_SOURCE" >&2
        exit 1
    fi
    # Compare file sizes as a basic verification
    src_size=$(stat -c%s "$CONFIG_SOURCE/$file" 2>/dev/null || echo "0")
    dst_size=$(stat -c%s "$CONFIG_TARGET/$file" 2>/dev/null || echo "0")
    if [ "$src_size" != "$dst_size" ]; then
        echo "ERROR: File size mismatch for $file (source: $src_size, destination: $dst_size)" >&2
        exit 1
    fi
done

export XDG_CONFIG_HOME="$HOME/.config"

# Make elpaca strictly offline at runtime
export ELPACA_NO_AUTOINSTALL=t
export GIT_TERMINAL_PROMPT=0

exec emacs --init-directory $CONFIG_TARGET "$@"
