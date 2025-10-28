#!/bin/sh
for f in ./examples/*; do
    base=$(basename "$f")
    case "$base" in
        repl.metta|gpt.metta|mm2.metta|lib_*.metta)
            continue ;;
    esac
    echo "Running $f"
    output=$(sh run.sh "$f" | grep "is ")
    if ! echo "$output" | grep -q "✅" || echo "$output" | grep -q "❌"; then
        echo "Failure in $f: found $output"
        exit 1
    else
        echo "$output"
    fi
done

exit 0
