extern int mut = 1;       // #[export_name = "mut"]
extern int y = 2;         // #[no_mangle]

extern int l(void) {    // #[export_name = "let"]
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
