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
        x += 1;
    case 1:
        x += 2;
    case 2:
        x += 3;
        return x;
    case 3:
        x += 4;
    case 4:
        x += 5;
    default:
        x += 6;
    }
    x += 7;
    return x;
}
