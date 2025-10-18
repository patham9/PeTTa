LD_PRELOAD=./mork_ffi/target/release/libmork_ffi.so swipl --stack_limit=8g -q -s ./src/main.pl -- $*
