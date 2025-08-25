int side_effect(int* x){
    *x = 1;
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

void unary_with_side_effect(int * const buffer){
    char* arr[1] = {0};

    -side_effect(&buffer[1]);
    +side_effect(&buffer[2]);
    ~side_effect(&buffer[3]);
    !side_effect(&buffer[4]);
    &""[side_effect(&buffer[5])];
    *arr[side_effect(&buffer[6])];
    ++arr[side_effect(&buffer[7])];
    --arr[side_effect(&buffer[8])];
    arr[side_effect(&buffer[9])]++;
    arr[side_effect(&buffer[10])]--;
}
