#!/bin/sh

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)

if [ "$#" -eq 0 ]; then
    exec swipl -q -s "$SCRIPT_DIR/src/main.pl" -- --debug-help
fi

exec sh "$SCRIPT_DIR/run.sh" "$@"
