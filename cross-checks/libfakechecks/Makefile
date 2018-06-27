LIB=libfakechecks.so
CFLAGS=-O2
CXXFLAGS=-O2
LDFLAGS=-O2

.PHONY: all

all: $(LIB)

clean:
	rm -f $(LIB) fakechecks.o

$(LIB): CXXFLAGS += -fPIC -std=c++14
$(LIB): LDFLAGS += -fPIC -lpthread
$(LIB): fakechecks.o
	$(CXX) -shared $(LDFLAGS) -o $@ $^
