cd ./mork_ffi
RUSTFLAGS="-C target-cpu=native" cargo build -p mork_ffi --release
nm -D ./target/release/libmork_ffi.so | grep rust_mork
gcc -shared -fPIC -o morklib.so mork.c $(pkg-config --cflags --libs swipl)
