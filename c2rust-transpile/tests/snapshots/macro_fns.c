#define CONST 42

#define ID(x) x
#define PAREN(x) (x)
#define LIT_ID ID(42)
#define LIT_PAREN PAREN(42)
#define CONST_ID ID(CONST)
#define CONST_PAREN PAREN(CONST)
#define NESTED_ID(x) ID(x)
#define NESTED_PAREN(x) PAREN(x)

void basic(void) {
    int lit_id = LIT_ID;
    int lit_paren = LIT_PAREN;
    int const_id = CONST_ID;
    int const_paren = CONST_PAREN;

    int id_with_lit = ID(42);
    int paren_with_lit = PAREN(42);
    int id_with_const = ID(CONST);
    int paren_with_const = PAREN(CONST);

    int nested_id_with_lit = NESTED_ID(42);
    int nested_paren_with_lit = NESTED_PAREN(42);
    int nested_id_with_const = NESTED_ID(CONST);
    int nested_paren_with_const = NESTED_PAREN(CONST);
}
