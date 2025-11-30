cd ./mork_ffi
RUSTFLAGS="-C target-cpu=native" cargo build -p mork_ffi --release
nm -D ./target/release/libmork_ffi.so | grep rust_mork
gcc -shared -fPIC -o morklib.so mork.c $(pkg-config --cflags --libs swipl)
echo "Successfully built mork_ffi"

cd ..
cd faiss_ffi
#g++ -fPIC -shared faiss.cpp -o faisslib.so -lfaiss -Wl,-rpath,/usr/local/lib
g++ -std=c++17 -fPIC -shared faiss.cpp \
    -o faisslib.so \
    /usr/local/lib/libfaiss.a \
    -I/usr/local/include \
    -lopenblas -lblas -llapack -lgfortran -lm -lpthread \
    -fopenmp -lgomp \
    $(pkg-config --cflags --libs swipl)
echo "Successfully built faiss_ffi"
