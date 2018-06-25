#ifndef CROSS_CHECKS_H
#define CROSS_CHECKS_H

#pragma once

#define CROSS_CHECK(x)          __attribute__((annotate("cross_check:" x)))
#define DISABLE_XCHECKS(x)      CROSS_CHECK("{ disable_xchecks: " #x " }")
#define DEFAULT_XCHECK          CROSS_CHECK("default")
#define DISABLED_XCHECK         CROSS_CHECK("disabled")
#define FIXED_XCHECK(x)         CROSS_CHECK("{ fixed: " x " }")
#define CUSTOM_XCHECK(x)        CROSS_CHECK("{ custom: " x " }")
#define CUSTOM_HASH_XCHECK(x)   CROSS_CHECK("{ custom_hash: " x " }")

#endif // CROSS_CHECKS_H
