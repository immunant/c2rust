LIB=libcrosscheck.so
CFLAGS=-O2
LDFLAGS=-O2

.PHONY: all

all: $(LIB)

clean:
	rm -f $(LIB) cyg_profile_checker.o test

test: CFLAGS += -g
test: test.c | $(LIB)
	$(CC) $(CFLAGS) -finstrument-functions -rdynamic -lcrosscheck -L$(CURDIR) -Wl,-rpath,$(CURDIR) -o $@ $^

$(LIB): CFLAGS += -fPIC -fvisibility=hidden
$(LIB): LDFLAGS += -fPIC -ldl
$(LIB): cyg_profile_checker.o
	$(CC) -shared $(LDFLAGS) -o $@ $^
