# MeshCodec

This is a WIP reimplementation of the MeshCodec compression/encoding format found in *Tears of the Kingdom*. MeshCodec makes use of ZStandard for compression/decompression, meshoptimizer for index buffer encoding, and a mix of custom encoding schemes. At the moment, only decompression/decoding is supported and there are no plans to implement compression/encoding.

## Progress

Accurately decompresses all `.bfres.mc` files found in *TotK* (`.chunk` files are still a bit iffy but might work). The main objectives left are cleaning up the codebase (better naming of functions/variables/classes/etc., refactoring to be more naturally structured, etc.) and documentation. MSVC support is currently non-existent until I decided to write a float16 implementation for it.

## Basic Usage

See `tests/src/main.cpp` for example usage.

## Building

Requires at least C++20 (for `std::construct_at`) and CMake 3.18 (probably could go lower but I'm too lazy to test). MSVC is currently unsupported but may be in the future.

To build the test program:

```sh
cmake -B build . -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=Release -DZSTD_BUILD_SHARED=OFF -DZSTD_BUILD_PROGRAMS=OFF -DZSTD_STATIC_LINKING_ONLY=ON -DBUILD_TESTING=ON
cmake --build build
```

## Credit

Some of the code found in `mc_IndexCodec.h`/`mc_IndexCodec.cpp` and `mc_VertexCodec.cpp` is based on Arseny Kapoulkine's [meshoptimizer](https://github.com/zeux/meshoptimizer) and is licensed under the MIT License (it was not added as a library because significant enough changes were made where using the meshoptimizer library directly is impractical). [ZStandard](https://github.com/facebook/zstd) is licened under GPL-2.0 and was developed by Meta. Everything else comes from reverse engineering work and is licensed under under GPL-2.0.