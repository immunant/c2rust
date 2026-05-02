int puts(const char *str);

static int side_effect(){
    puts("the return of side effect");
    return 10;
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
    char* arr[1] = {0};

    -side_effect();
    +side_effect();
    ~side_effect();
    !side_effect();
    &""[side_effect()];
    *arr[side_effect()];
    ++arr[side_effect()];
    --arr[side_effect()];
    arr[side_effect()]++;
    arr[side_effect()]--;
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
