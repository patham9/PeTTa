#!/bin/bash

run_test() {
    f="$1"
    echo "Running $f"

    # Capture only lines containing expected markers
    output=$(sh run.sh "$f" | grep "is " | grep " should ")

    # Check for success/failure markers
    has_pass=$(echo "$output" | grep -q "✅"; echo $?)
    has_fail=$(echo "$output" | grep -q "❌"; echo $?)

    if [ $has_fail -eq 0 ] || [ $has_pass -ne 0 ]; then
        echo "FAILURE in $f:"
        echo "$output"
        return 1
    else
        echo "OK: $f"
        echo "$output"
        return 0
    fi
}

pids=""
declare -A pid_to_file

for f in ./examples/*.metta; do
    base=$(basename "$f")
    case "$base" in
        repl.metta|gpt.metta|torch.metta|greedy_chess.metta|zmorkspace*.metta|git_import.metta)
            continue ;;
    esac

    run_test "$f" &
    pid=$!
    pids="$pids $pid"
    pid_to_file[$pid]="$f"
done

status=0
for pid in $pids; do
    wait "$pid"
    code=$?
    if [ $code -ne 0 ]; then
        failed_file="${pid_to_file[$pid]}"
        echo ""
        echo "==============================="
        echo "Stopping tests due to failure:"
        echo "❌ Failed test: $failed_file"
        echo "==============================="
        kill $pids 2>/dev/null
        status=$code
        break
    fi
done

exit $status
