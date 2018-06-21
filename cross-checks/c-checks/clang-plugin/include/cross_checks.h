#ifndef CROSS_CHECKS_H
#define CROSS_CHECKS_H

#pragma once

#define CROSS_CHECK(x)  __attribute__((annotate("cross_check:" x)))
#define DISABLE_XCHECKS(x) CROSS_CHECK("{ disable_xchecks: " #x " }")

#endif // CROSS_CHECKS_H
