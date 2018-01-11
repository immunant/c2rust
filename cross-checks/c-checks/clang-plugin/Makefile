
PLUGIN_PATH := $(shell find . -name CrossChecks.so)

PLUGIN_CC := clang
PLUGIN_CC += -Wno-unknown-attributes
PLUGIN_CC += -Xclang -load
PLUGIN_CC += -Xclang $(PLUGIN_PATH)
PLUGIN_CC += -Xclang -add-plugin
PLUGIN_CC += -Xclang crosschecks
#PLUGIN_CC += -Xclang -ast-dump

FAKECHECKS_PATH=`pwd`/../../libfakechecks

.PHONY: clean all test
all: test

clean:
	rm -f test

test: test.c
	$(PLUGIN_CC) -L$(FAKECHECKS_PATH) -lfakechecks -std=c11 -g -o test $<
	env LD_LIBRARY_PATH="$(FAKECHECKS_PATH)" ./test
