
PLUGIN_PATH := $(shell find . -name CrossChecks.so)
RUNTIME_PATH := $(shell find . -name libruntime.a)

PLUGIN_CC := clang
PLUGIN_CC += -Wno-unknown-attributes
PLUGIN_CC += -Xclang -load
PLUGIN_CC += -Xclang $(PLUGIN_PATH)
PLUGIN_CC += -Xclang -add-plugin
PLUGIN_CC += -Xclang crosschecks
PLUGIN_CC += -Xclang -plugin-arg-crosschecks
PLUGIN_CC += -Xclang -Ctest.c2r

ifneq ($(filter yes true,$(DUMP_AST)),)
PLUGIN_CC += -Xclang -ast-dump
endif

FAKECHECKS_PATH=`pwd`/../../libfakechecks

.PHONY: clean all test
all: test

clean:
	rm -f test

test: test.c
	$(PLUGIN_CC) -L$(FAKECHECKS_PATH) -lfakechecks -std=c11 -g -o test $< $(RUNTIME_PATH)
	env LD_LIBRARY_PATH="$(FAKECHECKS_PATH)" ./test
