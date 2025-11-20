sudo apt install libopenblas-dev libblas-dev liblapack-dev
git clone https://github.com/facebookresearch/faiss.git
cd faiss
cmake -B build \
    -DFAISS_ENABLE_GPU=OFF \
    -DFAISS_ENABLE_PYTHON=OFF \
    -DBUILD_SHARED_LIBS=OFF   # <— make static lib
cmake --build build --config Release --parallel
sudo cmake --install build
