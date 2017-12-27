// Will fail for now, since we the intermediate struct decls are not being
// produced in the final Rust code.

struct {
  struct {
    int l;
  } j;
} k;

void entry(unsigned int n, int buf[]) { }
