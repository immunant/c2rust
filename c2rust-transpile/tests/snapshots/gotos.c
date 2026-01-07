int sum(int count) {
    goto a;

    b:
    --count;
    goto d;

    a:;
    int x = 0;
    goto d;

    c:
    return x;

    d:
    if(count <= 0)
    goto c;
    goto e;

    e:
    x += count;
    goto b;
}

int goto_error_simple(int x) {
    if (x) {
        x += 1;
        if (x) {
            x += 2;
            goto error;
        }
    } else {
        x += 3;
        if (x) {
            x += 4;
            goto error;
        }
    }
    return x + 5;

error:
    return x + 6;
}

// Triggers a case where we're inverting the success and error cases. We want
// the code to be wrapped in a `'error: {}` block, but instead we're getting the
// error return placed inside a block with a synthetic label, and then the
// success return placed after the block.
/*
int goto_error_minimal(int x) {
    if (x) {
        if (x)
            goto error;
    } else {
        if (x)
            goto error;
    }
    return x + 1;

error:
    return x + 2;
}
*/

int multi_goto_error(int x) {
    if (x) {
        x += 1;
        if (x)
            if (x)
                goto bad;
        x += 2;
        if (x)
            goto also_bad;
    }

    if (x) {
        x += 3;
        if (x)
            if (x)
                goto bad;
        x += 4;
        if (x)
            goto also_bad;
    }

    return x;

bad:
    return x + 10;

also_bad:
    return x + 20;
}

int multi_goto_error_common(int x) {
    if (x) {
        x += 1;
        if (x)
            if (x)
                goto bad;
        x += 2;
        if (x)
            goto also_bad;
    }

    if (x) {
        x += 3;
        if (x)
            if (x)
                goto bad;
        x += 4;
        if (x)
            goto also_bad;
    }

    return x;

bad:
    x += 5;
    goto common;

also_bad:
    x += 6;

common:
    return x + 10;
}
