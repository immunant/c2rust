int side_effect(void);
int *lvalue_side_effect(void);

void pure(void) {
    int i = 5;

    -i;
    +i;
    ~i;
    !i;
    &i;
    *&i;
}

void pure_with_side_effect(){
    char *arr[1] = {0};

    -side_effect();
    +side_effect();
    ~side_effect();
    !side_effect();
    &""[side_effect()];
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
