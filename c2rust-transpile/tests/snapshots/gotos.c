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

