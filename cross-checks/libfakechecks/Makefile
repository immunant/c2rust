LIB=libfakechecks.so
CFLAGS=-O2 -pthread
CXXFLAGS=-O2 -pthread
LDFLAGS=-O2 -pthread

.PHONY: all

all: $(LIB)

clean:
	rm -f $(LIB) fakechecks.o

$(LIB): CXXFLAGS += -fPIC -std=c++14
$(LIB): LDFLAGS += -fPIC
$(LIB): fakechecks.o
	$(CXX) -shared $(LDFLAGS) -o $@ $^
