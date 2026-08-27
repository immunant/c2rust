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

// Regression test for relooper 2, previously we were inverting the success and
// error cases.
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

int nested_irreducible(int x, int start_with_error) {
    if (x < 0) {
        if (start_with_error)
            goto exit;
    }
    if (start_with_error)
        goto error;
    goto next;

next:
    if (x == 1)
        goto error;
    x -= 1;

error:
    if (x > 0)
        goto next;
    goto exit;

exit:
    return x;
}

int double_irreducible(int x) {
    if (x % 2 == 0) {
        goto l1;
    } else {
        goto l2;
    }

    while (x < 10) {
l1:
        x += 1;

l2:
        x += 1;
    }

    if (x % 2 == 0) {
        goto a;
    } else {
        goto b;
    }

    while (x < 10) {
a:
        x += 1;

b:
        x += 1;
    }

    return x;
}
