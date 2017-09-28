#include <dlfcn.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

#include <unordered_map>
#include <mutex>

#define MEMOIZE_FUNC_HASHES   1

uint32_t djb2_hash(const uint8_t *str) {
    uint32_t hash = 5381;
    while (uint32_t c = static_cast<uint32_t>(*str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    return hash;
}

// Cache for the function->hash mapping
// FIXME: this breaks if different libraries get loaded and unloaded
// at the same addresses
std::unordered_map<void*, uint32_t> hash_cache;
std::mutex cache_mutex;

uint32_t get_func_hash(void *func) {
#if MEMOIZE_FUNC_HASHES
    std::lock_guard<std::mutex> lock(cache_mutex);
    auto it = hash_cache.find(func);
    if (it != hash_cache.end())
        return it->second;
#endif

    Dl_info func_info, caller_info;
    dladdr(func, &func_info);
    auto *func_name = reinterpret_cast<const uint8_t*>(func_info.dli_sname);
    uint32_t func_hash = djb2_hash(func_name);
#if MEMOIZE_FUNC_HASHES
    hash_cache[func] = func_hash;
#endif
    return func_hash;
}

extern "C" {
void __cyg_profile_func_enter(void *, void *) __attribute__((no_instrument_function, visibility("default")));

void rb_xcheck(uint8_t tag, uint64_t val) __attribute__((weak));

void __cyg_profile_func_enter(void *func,  void *caller) {
    auto func_hash = get_func_hash(func);
    if (rb_xcheck)
        rb_xcheck(0, static_cast<uint64_t>(func_hash));
}
}
