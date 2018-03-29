#include <cstdio>
#include <cstdlib>
#include <cstddef>
#include <cstdint>
#include <atomic>
#include <mutex>

std::atomic<FILE*> fout_atomic{nullptr};
std::mutex fout_mutex;

// Thread-safe initialization for fout
static FILE *get_fout() {
    FILE *fout = fout_atomic.load();
    if (fout != nullptr)
        return fout;

    std::lock_guard<std::mutex> lock(fout_mutex);
    fout = fout_atomic.load();
    if (fout != nullptr)
        return fout;

    const char *out_file = getenv("FAKECHECKS_OUTPUT_FILE");
    if (out_file == nullptr) {
        fout = stderr;
    } else {
        fout = fopen(out_file, "w");
        if (fout == nullptr) {
            fprintf(stderr, "Error opening fakechecks output file '%s', "
                            "writing to stderr instead!\n", out_file);
            fout = stderr;
        }
    }
    fout_atomic.store(fout);
    return fout;
}

extern "C"
void rb_xcheck(uint8_t tag, uint64_t item) {
    auto *fout = get_fout();
    fprintf(fout, "XCHECK(%hhd):%lu/0x%08lx\n", tag, item, item);
}
