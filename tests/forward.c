
// forward declaration of structs should not cause problems.
struct s;
struct s { int x; }; 

void entry(const unsigned int sz, int buf[const]){ 
  if (sz < 1) return;

  struct s foo = { 1 };
  buf[0] = foo.x;
}

