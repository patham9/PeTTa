if [ -f ./mork_ffi/target/release/libmork_ffi.so ]; then
    LD_PRELOAD=./mork_ffi/target/release/libmork_ffi.so \
    swipl --stack_limit=8g -q -s ./src/main.pl -- "$@" mork
else
    swipl --stack_limit=8g -q -s ./src/main.pl -- "$@"
fi
