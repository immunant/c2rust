int side_effect(void);
int *lvalue_side_effect(void);

struct S {
    int x;
    int y;
};

void pure(void) {
    int i = 5;

    -i;
    +i;
    ~i;
    !i;
    &i;
    *&i;

    // Compound literals
    &(struct S){0, 2};
    -(struct S){0, 2}.x;

    // Builtins known to have no side effects
    -__builtin_bswap32(1);
    !__builtin_expect(0, 0);
}

void pure_with_side_effect(){
    char *arr[1] = {0};

    -side_effect();
    +side_effect();
    ~side_effect();
    !side_effect();
    &""[side_effect()];

    &(struct S){side_effect(), 2};
    -(struct S){side_effect(), 2}.x;

    -__builtin_bswap32(side_effect());
    !__builtin_expect(side_effect(), 0);
}

void impure(void) {
    int i = 5;

    i++;
    i--;
    --i;
    ++i;
}

void impure_with_side_effect() {
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

void impure_with_lvalue_side_effect() {
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
