//! enable_relooper, disallow_current_block

int stmt_expr_func(int y) {

  int x = ({
    int z = 9;

    switch (y) {
      case 0: z += 3;
      case 1: goto lbl;
      default: z += 6; 
    };

    z += 6;
lbl: ;

    z + (++y);
  });

  return x + y;
}
