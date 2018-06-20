
TEST_CFLAGS=-g -O2

#PLUGIN_PATH := $(shell find . -name CrossChecks.so)
#RUNTIME_PATH := $(shell find . -name libruntime.a)
PLUGIN_PATH := $(realpath ../../../dependencies/clang-xcheck-plugin.$(shell uname -n)/plugin/CrossChecks.so)
RUNTIME_PATH := $(realpath ../../../dependencies/clang-xcheck-plugin.$(shell uname -n)/runtime/libruntime.a)

# Override this to change the path to the clang binary
#PLUGIN_CC := clang
PLUGIN_CC := $(realpath ../../../dependencies/llvm-6.0.0/build.$(shell uname -n)/bin/clang)

PLUGIN_CC_ARGS := -Xclang -plugin-arg-crosschecks -Xclang -Ctest.c2r

ifneq ($(filter yes true,$(DUMP_AST)),)
PLUGIN_CC_ARGS += -Xclang -ast-dump
endif

ifneq ($(filter yes true,$(DUMP_LLVM)),)
PLUGIN_CC_ARGS += -S -emit-llvm
endif

FAKECHECKS_PATH=`pwd`/../../libfakechecks

.PHONY: clean all test.bin
all: test.bin

clean:
	rm -f test.bin

test.bin: test.c
	@echo Building test...
	./cc_wrapper.sh $(PLUGIN_CC) $(PLUGIN_PATH) $(PLUGIN_CC_ARGS) -ffunction-sections -fuse-ld=gold -Wl,--gc-sections,--icf=safe,-rpath,$(FAKECHECKS_PATH) -L$(FAKECHECKS_PATH) -lfakechecks -std=c11 $(TEST_CFLAGS) -o test.bin $< $(RUNTIME_PATH)
	@echo Running test...
	./test.bin
