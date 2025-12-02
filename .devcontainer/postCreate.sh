#!/usr/bin/env bash
set -euo pipefail

echo "Running devcontainer post-create setup: install ghcup, GHC, cabal, HLS, ormolu"

if command -v ormolu >/dev/null 2>&1; then
  echo "ormolu already installed"
  exit 0
fi

sudo apt-get update
sudo apt-get install -y build-essential curl libgmp-dev ca-certificates git pkg-config

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_ALLOW_DOWNLOAD=1

echo "Installing ghcup..."
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

# Ensure ghcup is available in PATH for this script
export PATH="$HOME/.ghcup/bin:$PATH"

echo "Installing recommended GHC, Cabal and HLS via ghcup"
ghcup install ghc recommended || true
ghcup set ghc recommended || true
ghcup install cabal recommended || true
ghcup set cabal recommended || true
ghcup install hls recommended || true

echo "Updating cabal package list and installing ormolu"
cabal update
cabal v2-install ormolu

echo "Adding ghcup/cabal to PATH in ~/.profile"
# Ensure PATH export is in ~/.profile for future shell sessions
if ! grep -q "ghcup/cabal" ~/.profile 2>/dev/null; then
  cat >> ~/.profile << 'PATHEOF'

# ghcup and cabal
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"
PATHEOF
fi

echo "Devcontainer setup complete"
