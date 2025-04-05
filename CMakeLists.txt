CMAKE_MINIMUM_REQUIRED(VERSION 3.18)

project(MeshCodec CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    add_compile_options(-fdiagnostics-color=always)
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    add_compile_options(-fcolor-diagnostics)
endif()

add_subdirectory(lib/zstd/build/cmake)

add_library(MeshCodec
    src/include/mc_Types.h

    # meshoptimizer derived-ish
    src/include/mc_IndexCodec.h
    src/include/mc_VertexCodec.h

    # zstandard
    src/include/mc_Zstd.h

    src/include/mc_AttributeCodec.h
    src/include/mc_BitStream.h
    src/include/mc_Codec.h
    src/include/mc_CompressionFormat.h
    src/include/mc_DecompContext.h
    src/include/mc_Float.h
    src/include/mc_IndexDecompressor.h
    src/include/mc_IndexStreamContext.h
    src/include/mc_StackAllocator.h
    src/include/mc_StreamContext.h
    src/include/mc_VertexDecompContext.h
    src/include/mc_VertexDecompressor.h

    src/mc_AttributeCodec.cpp
    src/mc_AttributeStreamInfo.cpp
    src/mc_Codec.cpp
    src/mc_Float.cpp
    src/mc_IndexCodec.cpp
    src/mc_IndexDecompressor.cpp
    src/mc_IndexStreamContext.cpp
    src/mc_StackAllocator.cpp
    src/mc_VertexCodec.cpp
    src/mc_VertexDecompContext.cpp
    src/mc_VertexDecompressor.cpp
    src/mc_Zstd.cpp

    src/mc_MeshCodec.h
    src/mc_MeshCodec.cpp
)

if (MSVC)
    target_compile_options(MeshCodec PRIVATE /W4 /wd4244 /wd4127 /Zc:__cplusplus)
    target_include_directories(MeshCodec SYSTEM PUBLIC lib/float16)
else()
    target_compile_options(MeshCodec PRIVATE -Wall -Wextra -fno-plt)
endif()

target_include_directories(MeshCodec PRIVATE lib/zstd/lib)
target_include_directories(MeshCodec PRIVATE src/include)
target_include_directories(MeshCodec PUBLIC src/)

target_link_libraries(MeshCodec PRIVATE libzstd_static)

if (BUILD_TESTING)
    add_subdirectory(tests)
endif()