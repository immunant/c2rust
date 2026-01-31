#ifndef CROSS_CHECKS_H
#define CROSS_CHECKS_H

#pragma once

#include <stdint.h>

#define CROSS_CHECK(x)          __attribute__((annotate("cross_check:" x)))
#define DISABLE_XCHECKS(x)      CROSS_CHECK("{ disable_xchecks: " #x " }")
#define DEFAULT_XCHECK          CROSS_CHECK("default")
#define DISABLED_XCHECK         CROSS_CHECK("disabled")
#define FIXED_XCHECK(x)         CROSS_CHECK("{ fixed: " x " }")
#define CUSTOM_XCHECK(x)        CROSS_CHECK("{ custom: \"" x "\" }")
#define CUSTOM_HASH_XCHECK(x)   CROSS_CHECK("{ custom_hash: \"" x "\" }")

#ifdef C2RUST_CROSS_CHECK_VALUE_REAL
extern void c2rust_cross_check_value(uint8_t tag, ...);
#else
static void c2rust_cross_check_value(uint8_t tag, ...) { __builtin_unreachable(); }
#endif

#endif // CROSS_CHECKS_H
