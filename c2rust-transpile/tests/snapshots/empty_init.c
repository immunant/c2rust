typedef struct Scope Scope;
struct Scope {
  Scope *next;
};

Scope *scope = &(Scope){};

typedef struct Foo_s {
    int x;
} Foo_t;

void foo(void) {
    Foo_t f;
}
