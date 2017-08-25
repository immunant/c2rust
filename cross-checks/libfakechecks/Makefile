LIB=libfakechecks.so
CFLAGS=-O2
CXXFLAGS=-O2
LDFLAGS=-O2

.PHONY: all

all: $(LIB)

clean:
	rm -f $(LIB) fakechecks.o

$(LIB): CXXFLAGS += -fPIC
$(LIB): LDFLAGS += -fPIC
$(LIB): fakechecks.o
	$(CXX) -shared $(LDFLAGS) -o $@ $^
