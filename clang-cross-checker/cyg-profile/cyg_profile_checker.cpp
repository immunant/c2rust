#include <dlfcn.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

#include <unordered_map>
#include <mutex>

uint32_t djb2_hash(const uint8_t *str) {
    uint32_t hash = 5381;
    while (uint32_t c = static_cast<uint32_t>(*str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    return hash;
}

std::unordered_map<void*, uint32_t> hash_cache;
std::mutex cache_mutex;

uint32_t get_func_hash(void *func) {
    std::lock_guard<std::mutex> lock(cache_mutex);
    auto it = hash_cache.find(func);
    if (it != hash_cache.end())
        return it->second;

    Dl_info func_info, caller_info;
    dladdr(func, &func_info);
    auto *func_name = reinterpret_cast<const uint8_t*>(func_info.dli_sname);
    uint32_t func_hash = djb2_hash(func_name);
    hash_cache[func] = func_hash;
    return func_hash;
}

extern "C" {
void __cyg_profile_func_enter(void *, void *) __attribute__((no_instrument_function, visibility("default")));

void rb_xcheck(unsigned long) __attribute__((weak));

void __cyg_profile_func_enter(void *func,  void *caller) {
    auto func_hash = get_func_hash(func);
    if (rb_xcheck)
        rb_xcheck(func_hash);
}
}
