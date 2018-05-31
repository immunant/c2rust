#include <cstdio>
#include <cstdlib>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <atomic>
#include <mutex>

#include <alloca.h>
#include <pthread.h>
#include <unistd.h>

std::atomic<FILE*> fout_atomic{nullptr};
std::mutex fout_mutex;

bool append_pid = false;
std::once_flag append_pid_flag;

static void init_flags() {
    std::call_once(append_pid_flag, [] () {
        auto append_pid_var = getenv("FAKECHECKS_APPEND_PID");
        if (strcmp(append_pid_var, "1") == 0 ||
            strcasecmp(append_pid_var, "true") == 0 ||
            strcasecmp(append_pid_var, "yes") == 0) {
            // Append PID to file name
            append_pid = true;
            pthread_atfork(nullptr, nullptr, [] () {
                // On fork, clear fout
                auto old_fout = fout_atomic.exchange(nullptr);
                if (old_fout != nullptr) {
                    fclose(old_fout);
                }
            });
        }
    });
}

// Thread-safe initialization for fout
static FILE *get_fout() {
    init_flags();

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
        if (append_pid) {
            // Append the PID to the file name
            auto pid_out_len = strlen(out_file) + 16;
            char *pid_out_file = static_cast<char*>(alloca(pid_out_len));
            auto pid = getpid();
            snprintf(pid_out_file, pid_out_len, "%s.%d", out_file, pid);
            fout = fopen(pid_out_file, "w");
        } else {
            fout = fopen(out_file, "w");
        }
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
