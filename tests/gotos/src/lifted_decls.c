void foo(int a) {
    switch (a) {
        case 1: {
            char buf[3];

            buf[0] = 'a';
            }
            break;
        case 2: {
            char buf[2];

            buf[0] = 'a';
            }
            break;
        default: {
            char buf[1];

            buf[0] = 'a';
        }
    }
}
