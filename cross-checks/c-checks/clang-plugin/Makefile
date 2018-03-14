
TEST_CFLAGS=-g -O2

PLUGIN_PATH := $(shell find . -name CrossChecks.so)
RUNTIME_PATH := $(shell find . -name libruntime.a)

# Override this to change the path to the clang binary
PLUGIN_CC := clang

PLUGIN_CC_ARGS := -Wno-unknown-attributes
PLUGIN_CC_ARGS += -Xclang -load
PLUGIN_CC_ARGS += -Xclang $(PLUGIN_PATH)
PLUGIN_CC_ARGS += -Xclang -add-plugin
PLUGIN_CC_ARGS += -Xclang crosschecks
PLUGIN_CC_ARGS += -Xclang -plugin-arg-crosschecks
PLUGIN_CC_ARGS += -Xclang -Ctest.c2r

ifneq ($(filter yes true,$(DUMP_AST)),)
PLUGIN_CC += -Xclang -ast-dump
endif

ifneq ($(filter yes true,$(DUMP_LLVM)),)
PLUGIN_CC += -S -emit-llvm
endif

FAKECHECKS_PATH=`pwd`/../../libfakechecks

.PHONY: clean all test
all: test

clean:
	rm -f test

test: test.c
	@echo Building test...
	$(PLUGIN_CC) $(PLUGIN_CC_ARGS) -ffunction-sections -fuse-ld=gold -Wl,--gc-sections,--icf=safe,-rpath,$(FAKECHECKS_PATH) -L$(FAKECHECKS_PATH) -lfakechecks -std=c11 $(TEST_CFLAGS) -o test $< $(RUNTIME_PATH)
	@echo Running test...
	./test
