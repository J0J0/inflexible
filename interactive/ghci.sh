#!/bin/bash

MAIN_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")/.."

export inflexible_datadir="$MAIN_DIR"

cabal exec -- ghci -ghci-script "$MAIN_DIR/interactive/dot-ghci" "$@"
