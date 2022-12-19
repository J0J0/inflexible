#!/bin/bash

SAGEENV_PATH="/usr/bin/sage-env"
MAIN_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")/.."

source "$SAGEENV_PATH"
export inflexible_datadir="$MAIN_DIR"

cabal exec -- ghci -ghci-script "$MAIN_DIR/interactive/dot-ghci" -ghci-script "$MAIN_DIR/interactive/dot-ghci-sage" "$@"
