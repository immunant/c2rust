// This should translate to a straightforward `match`.
int idiomatic_switch(int x) {

    switch (1 + x) {
        case 0:
        case 2: 
            x += 2;
            break;
        case 1:
            x += 1;
            break;
        default:
            x += 3;
    }

    return x;
}
