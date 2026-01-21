int basic_switch(int x) {
    switch (x) {
    case 0:
        x += 1;
        break;

    case 1:
        x += 2;
        if (x) {
            x += 3;
        } else {
            x += 4;
        }
        x += 5;
        break;

    // Multiple cases leading to the same place should get merged into a single
    // arm with an | pattern.
    case 2:
    case 3:
    case 4:
        x += 6;
        break;

    // No default case, needs an empty branch for exhaustiveness.
    }

    // With default case.
    switch (x) {
    case 0:
        x += 7;
        break;
    default:
        x += 8;
        break;
    }

    return x;
}

// Fallthrough!
//
// `match` doesn't support fallthrough between arms the way that `switch` in C
// does, so to emulate fallthrough we use labeled blocks to create `break`
// targets that we can jump to from the various `match` arms.

int fallthrough_many(int x) {
    // With default.
    switch (x)
    {
    case 0:
        x += 1;
    case 1:
        x += 1;
    case 2:
        x += 1;
    case 3:
        x += 1;
    default:
        x += 1;
    }

    // Without default.
    switch (x)
    {
    case 0:
        x += 1;
    case 1:
        x += 1;
    case 2:
        x += 1;
    case 3:
        x += 1;
    case 4:
        x += 1;
    }

    return x;
}

// TODO: The generated control flow here is correct but not laid out well.
// Ideally we'd like to see the code for the cases laid out in the same order
// they appear, falling through neatly into the block for the next case, except
// for the one case that does an early return. Instead the cases are being
// slightly scrambled, resulting in more `break`s than should be necessary.
/*
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
*/
