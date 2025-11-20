#!/bin/bash
run_test() {
    f="$1"
    echo "Running $f"
    output=$(sh run.sh "$f" | grep "is " | grep " should ")
    if ! echo "$output" | grep -q "✅" || echo "$output" | grep -q "❌"; then
        echo "Failure in $f: found $output"
        return 1
    else
        echo "$output"
        return 0
    fi
}

pids=""
for f in ./examples/*.metta; do
    base=$(basename "$f")
    case "$base" in repl.metta|gpt.metta|torch.metta|greedy_chess.metta|zmorkspace*.metta)
        continue ;;
    esac
    run_test "$f" &
    pids="$pids $!"
done

status=0
for pid in $pids; do
    wait "$pid"
    code=$?
    if [ $code -ne 0 ]; then
        status=$code
        echo "Stopping tests due to failure..."
        kill $pids 2>/dev/null
        break
    fi
done

exit $status
