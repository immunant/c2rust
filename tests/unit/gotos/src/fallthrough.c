void stuff(const char *);

int fallthrough_2(int x) {
    switch (x)
    {
    case 0:
        x += 1;
    default:
        x += 1;
    }
    return x;
}

int fallthrough_3(int x) {
    switch (x)
    {
    case 0:
        x += 1;
    case 1:
        x += 1;
    default:
        x += 1;
    }
    return x;
}

int fallthrough_without_default(int x) {
    switch (x)
    {
    case 0:
        x += 1;
    case 1:
        x += 1;
    case 2:
        x += 1;
    }
    return x;
}

int fallthrough_with_early_return(int x) {
    switch (x)
    {
    case 0:
        return 10;
    case 1:
        x += 1;
    default:
        x += 1;
    }
    return x;
}
