LIB=libcrosscheck.so
CFLAGS=-O2
CXXFLAGS=-O2
LDFLAGS=-O2
LIBS = ../../libfakechecks

.PHONY: all

all: $(LIB)

clean:
	rm -f $(LIB) cyg_profile_checker.o test output

test: CFLAGS += -g
test: test.c | $(LIB)
	$(CC) $(CFLAGS) -finstrument-functions -rdynamic -lcrosscheck -L$(CURDIR) -Wl,-rpath,$(CURDIR) -o $@ $^

fakecheckoutput: test.c | $(LIB)
	$(CC) -g -finstrument-functions -rdynamic  $(CFLAGS) -o $@ $^ -lcrosscheck -ldl -L$(CURDIR) -Wl,-rpath,$(CURDIR) -L$(LIBS) -lfakechecks

$(LIB): CXXFLAGS += -fPIC -fvisibility=hidden
$(LIB): LDFLAGS += -fPIC -ldl -Wl,--as-needed
$(LIB): cyg_profile_checker.o
	$(CXX) -shared $(LDFLAGS) -o $@ $^

