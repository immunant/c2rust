typedef struct Scope Scope;
struct Scope {
  Scope *next;
};

Scope *scope = &(Scope){};
