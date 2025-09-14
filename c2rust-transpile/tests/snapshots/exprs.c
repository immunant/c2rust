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

void compound_literal(){
    /// https://github.com/immunant/c2rust/issues/1234
    int i = (enum {A, B, C}){1};
}
