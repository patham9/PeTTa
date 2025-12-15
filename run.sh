SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)

# Set up Python framework path for janus (macOS only, if using asdf-managed Python 3.9)
DYLD_VARS=""
if [ "$(uname)" = "Darwin" ]; then
    PYTHON_VERSION=$(python3 --version 2>&1 | grep -o "3\.9\.[0-9]*" || echo "")
    if [ -n "$PYTHON_VERSION" ]; then
        PYTHON_PREFIX=$(python3 -c "import sys; print(sys.prefix)" 2>/dev/null)
        # Extract the base path (before /Library/Frameworks)
        PYTHON_HOME=$(echo "$PYTHON_PREFIX" | sed 's|/Library/Frameworks.*||')
        FRAMEWORK_PATH="$PYTHON_HOME/Library/Frameworks/Python3.framework/Versions/3.9"
        if [ -d "$FRAMEWORK_PATH" ]; then
            DYLD_VARS="DYLD_LIBRARY_PATH=$FRAMEWORK_PATH DYLD_FALLBACK_LIBRARY_PATH=$FRAMEWORK_PATH"
        fi
    fi
fi

if [ -f $SCRIPT_DIR/mork_ffi/target/release/libmork_ffi.so ]; then
    env $DYLD_VARS LD_PRELOAD=$SCRIPT_DIR/mork_ffi/target/release/libmork_ffi.so \
    swipl --stack_limit=8g -q -s $SCRIPT_DIR/src/main.pl -- "$@" mork
else
    env $DYLD_VARS swipl --stack_limit=8g -q -s $SCRIPT_DIR/src/main.pl -- "$@"
fi
