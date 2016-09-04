#!/usr/bin/env bash
cabal sandbox init
cabal install

dest="$1"
if [[ -d "$dest" ]]; then
  cp dist/*/build/ho/ho "$dest/ho"
  echo copied ho to "$dest/ho"
else
  echo No install destination specified
fi
