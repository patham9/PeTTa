#!/bin/sh
for f in ./examples/*.metta; do
    base=$(basename "$f")
    case "$base" in repl.metta|gpt.metta|torch.metta|greedy_chess.metta|zmorkspace*.metta)
        continue ;;
    esac
    echo "Running $f"
    output=$(sh run.sh "$f" | grep "is " | grep " should ")
    if ! echo "$output" | grep -q "✅" || echo "$output" | grep -q "❌"; then
        echo "Failure in $f: found $output"
        exit 1
    else
        echo "$output"
    fi
done

exit 0
