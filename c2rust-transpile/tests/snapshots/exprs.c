int puts(const char *str);

static int side_effect(){
    puts("the return of side effect");
    return 0;
}

static int* lvalue_side_effect(){
    puts("the return of side effect");
    static int VAL = 42;
    return &VAL;
}

void unary_without_side_effect(){
    int i = 5;
    -i;
    +i;
    ~i;
    !i;
    &i;
    *&i;
    i++;
    i--;
    --i;
    ++i;
}

void unary_with_side_effect(){
    char *arr[1] = {0};

    -side_effect();
    +side_effect();
    ~side_effect();
    !side_effect();
    &""[side_effect()];
}

void inc_decl_with_rvalue_side_effect() {
    int arr[1] = {0};

    // Increment/decrement, expression value not used
    ++arr[side_effect()];
    --arr[side_effect()];
    arr[side_effect()]++;
    arr[side_effect()]--;

    // Increment/decrement, expression value is used
    int pre_inc = ++arr[side_effect()];
    int pre_dec = --arr[side_effect()];
    int post_inc = arr[side_effect()]++;
    int post_dec = arr[side_effect()]--;
}

void inc_decl_with_lvalue_side_effect() {
    // Increment/decrement, expression value not used
    ++*lvalue_side_effect();
    --*lvalue_side_effect();
    (*lvalue_side_effect())++;
    (*lvalue_side_effect())--;

    // Increment/decrement, expression value is used
    int pre_inc = ++*lvalue_side_effect();
    int pre_dec = --*lvalue_side_effect();
    int post_inc = (*lvalue_side_effect())++;
    int post_dec = (*lvalue_side_effect())--;
}

void unsigned_compound_desugaring(void) {
    enum E { EA };

    int i = 0;
    unsigned int u = 0;
    enum E e = EA;
    e += u;
    i += u;
}

typedef int int_t;

void cast_literals(void) {
    int i = 1L;
    int_t it = 1L;
    int i_neg = -1L;
    int_t it_neg = -1L;
}

void compound_literal(){
    /// https://github.com/immunant/c2rust/issues/1234
    int i = (enum {A, B, C}){1};
}

void statement_expr() {
    ({
        puts("should execute");
        return;
    });

    puts("should be unreachable!");
}
