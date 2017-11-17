
/* The external linking tests commented out below do not work because we need a custom C driver.
 * Here is one:
 * 
 *   // main.c (compile with `clang -lSystem -lresolv -lc -lm main.c tests/liblinking.a`)
 *   #include <stdio.h>
 * 
 *   extern in fn = 1;
 *   extern int k = 2;
 * 
 *   int use() { return 20; }
 *   int l() { return 200; }
 * 
 *   extern int mut;
 *   extern int y;
 * 
 *   extern int let(void);
 *   extern int w(void);
 * 
 *   int main(void) {
 *     printf("mut   = %d\n", mut  );
 *     printf("y     = %d\n", y    );
 *     printf("let() = %d\n", let());
 *     printf("w()   = %d\n", w()  );
 *     
 *     printf("foo() = %d\n", foo());
 *     printf("bar() = %d\n", bar());
 * 
 *     return 0;
 *   }

// External linkage, definition elsewhere
extern int k;             // #[no_mangle]

extern int use(void);     // #[link_name = "use"]
extern int l(void);       // #[no_mangle]

int foo() {
  extern int fn;          // #[link_name = "fn"]
  return fn + use();
}
int bar() { return k + l(); }
*/

// External linkage, definition here
extern int mut = 1;       // #[export_name = "mut"]
extern int y = 2;         // #[no_mangle]

extern int let(void) {    // #[export_name = "let"]
  return 3;
}
extern int w(void) {      // #[no_mangle]
  return 4;
}

// Internal linkage
static int crate = 3;     // renamed
static int x = 4;         // unrenamed 

static void ref(void) { } // renamed
static void z(void) { }   // unrenamed

void entry(void) { }
