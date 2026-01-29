// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks -Xclang -plugin-arg-crosschecks -Xclang --disable-xchecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>

#include <cross_checks.h>

int main() {
  uint16_t a = 42;
  uint32_t b = 1337;
  c2rust_cross_check_value(0x80, a);
  c2rust_cross_check_value(0xff, b);
  return 0;
}
// CHECK: XCHECK(128):8680820740569200732/0x787878787878785c
// CHECK: XCHECK(255):13021231110853800333/0xb4b4b4b4b4b4b18d
