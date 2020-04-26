int stmt_expr_func(int y) {

  int x = ({
    int z = 9;

    switch (y) {
      case 0: z += 3;
      case 1: goto lbl;
      case 2: return -42;
      default: z += 6; 
    };

    z += 6;
lbl: ;

    z + (++y);
  });

  return x + y;
}
