#include "mc_MeshCodec.h"

#include <cstring>
#include <fstream>
#include <iostream>
#include <span>
#include <string>
#include <vector>
#include <filesystem>

#define MAX_FILEPATH 0x1000

namespace fs = std::filesystem;
using Byte = mc::u8;

struct Options {
    bool verify = false;
    bool mc = false;
    bool chunk = false;

    void Parse(int argc, char** argv) {
        for (int i = 2; i < argc; ++i) {
            std::string arg = argv[i];
            if (arg == "-v" || arg == "--verify") {
                verify = true;
            } else if (arg == "--mc") {
                mc = true;
            } else if (arg == "--chunk") {
                chunk = true;
            }
        }
        if (!mc && !chunk) {
            mc = chunk = true;
        }
    }
};

const std::string ParseInput(int argc, char** argv, int index) {
    if (argc < index + 1) return "";
    size_t size = strnlen(argv[index], MAX_FILEPATH);
    return std::string{argv[index], argv[index] + size};
}

bool ReadFile(const std::string& path, std::vector<Byte>& data) {
    std::ifstream file(path, std::ios::ate | std::ios::binary);
    if (!file.is_open()) return false;
    data.resize(file.tellg());
    file.seekg(0);
    file.read(reinterpret_cast<char*>(data.data()), data.size());
    return true;
}

void WriteFile(const fs::path& path, const std::span<const Byte>& data) {
    fs::create_directories(path.parent_path());
    std::ofstream file(path, std::ios::binary);
    file.write(reinterpret_cast<const char*>(data.data()), data.size());
}

bool DecompressMCFile(const fs::path& inputPath, std::vector<Byte>& output, void* workMem) {
    std::vector<Byte> data;
    if (!ReadFile(inputPath.string(), data)) return false;
    auto header = reinterpret_cast<const mc::ResMeshCodecPackageHeader*>(data.data());
    size_t decompressedSize = header->GetDecompressedSize();
    output.resize(decompressedSize);
    return mc::DecompressMC(output.data(), decompressedSize, data.data(), data.size(), workMem, 0x10000000);
}

bool DecompressChunkFile(const fs::path& inputPath, std::vector<Byte>& output, void* workMem) {
    std::vector<Byte> data;
    if (!ReadFile(inputPath.string(), data)) return false;
    auto header = reinterpret_cast<const mc::ResChunkHeader*>(data.data());
    size_t decompressedSize = header->decompressedSize;
    output.resize(decompressedSize);
    return mc::DecompressChunk(output.data(), decompressedSize, data.data(), data.size(), workMem, 0x10000000);
}

void DecompressMcAll(const fs::path& romfs, const fs::path& out, void* workMem) {
    for (const auto& entry : fs::recursive_directory_iterator(romfs / "Model")) {
        if (entry.path().extension() == ".mc") {
            std::vector<Byte> output;
            if (DecompressMCFile(entry.path(), output, workMem)) {
                fs::path outFile = out / entry.path().stem();
                WriteFile(outFile, output);
                std::cout << "[MC] Decompressed: " << entry.path().filename() << "\n";
            } else {
                std::cout << "[MC] Failed: " << entry.path().filename() << "\n";
            }
        }
    }
}

void DecompressChunkAll(const fs::path& romfs, const fs::path& out, void* workMem) {
    for (const auto& entry : fs::recursive_directory_iterator(romfs / "Cave/cave017")) {
        if (entry.path().extension() == ".chunk") {
            std::vector<Byte> output;
            if (DecompressChunkFile(entry.path(), output, workMem)) {
                fs::path relative = fs::relative(entry.path(), romfs / "Cave/cave017");
                fs::path outFile = out / relative;
                WriteFile(outFile, output);
                std::cout << "[CHUNK] Decompressed: " << entry.path().filename() << "\n";
            } else {
                std::cout << "[CHUNK] Failed: " << entry.path().filename() << "\n";
            }
        }
    }
}

bool VerifyMc(const fs::path& romfs, const fs::path& expectedPath, void* workMem) {
    bool allGood = true;
    for (const auto& entry : fs::recursive_directory_iterator(romfs / "Model")) {
        if (entry.path().extension() == ".mc") {
            std::vector<Byte> actual;
            if (!DecompressMCFile(entry.path(), actual, workMem)) {
                std::cout << "[MC] Decompression failed: " << entry.path() << "\n";
                allGood = false;
                continue;
            }
            std::vector<Byte> expected;
            fs::path target = expectedPath / entry.path().stem();
            if (!ReadFile(target.string(), expected) || actual != expected) {
                std::cout << "[MC] Mismatch: " << entry.path().filename() << "\n";
                allGood = false;
            }
        }
    }
    return allGood;
}

bool VerifyChunk(const fs::path& romfs, const fs::path& expectedPath, void* workMem) {
    bool allGood = true;
    for (const auto& entry : fs::recursive_directory_iterator(romfs / "Cave/cave017")) {
        if (entry.path().extension() == ".chunk") {
            std::vector<Byte> actual;
            if (!DecompressChunkFile(entry.path(), actual, workMem)) {
                std::cout << "[CHUNK] Decompression failed: " << entry.path() << "\n";
                allGood = false;
                continue;
            }
            fs::path relative = fs::relative(entry.path(), romfs / "Cave/cave017");
            fs::path target = expectedPath / relative;
            std::vector<Byte> expected;
            if (!ReadFile(target.string(), expected) || actual != expected) {
                std::cout << "[CHUNK] Mismatch: " << entry.path() << "\n";
                allGood = false;
            }
        }
    }
    return allGood;
}

int main(int argc, char** argv) {
    const fs::path romfsPath = ParseInput(argc, argv, 1);
    if (romfsPath.empty()) {
        std::cout << "Usage: tool <romfs_path> [--mc] [--chunk] [-v|--verify]\n";
        return 1;
    }

    Options opt;
    opt.Parse(argc, argv);

    void* workMem = malloc(0x10000000);
    bool success = true;

    const fs::path mcPath = fs::path("mc");
    const fs::path chunkPath = fs::path("chunk");

    if (opt.verify) {
    bool mcOk = true, chunkOk = true;

    if (fs::exists(mcPath))
        mcOk = VerifyMc(romfsPath, mcPath, workMem);
    if (fs::exists(chunkPath))
        chunkOk = VerifyChunk(romfsPath, chunkPath, workMem);

    if (mcOk && chunkOk)
        std::cout << "All matched.\n";
    } else {
        if (opt.mc)
            DecompressMcAll(romfsPath, mcPath, workMem);
        if (opt.chunk)
            DecompressChunkAll(romfsPath, chunkPath, workMem);
    }

    free(workMem);
    return 0;
}
