// Basic loop structures.

int while_loop(int x) {
    while (x) {
        x += 1;
    }
    return x;
}

int for_loop(int x) {
    for (int i = 0; i < 10; i++) {
        x += 1;
    }
    return x;
}

void infinite_loop(int x) {
    for (;;) {
        x += 1;
    }
}

int goto_loop(int x) {
loop:
    x += 1;
    if (x) {
        goto loop;
    }
    return x;
}

int do_while_loop(int x) {
    do {
        x += 1;
    } while (x);
    return x;
}

// Simple break and continue.

int break_while(int x) {
    while (x) {
        if (x) {
            x += 1;
            if (x)
                break;
        }
        x += 2;
    }
    return x;
}

int continue_while(int x) {
    while (x) {
        if (x) {
            x += 1;
            if (x)
                continue;
        }
        x += 2;
    }
    return x;
}

// Simple nested loops.

void trivial_nested_while(int x) {
    while (x) {
        while (x) {
            while (x) {
                x += 1;
            }
        }
    }
}

void nested_while(int x) {
    while (x) {
        while (x) {
            while (x) {
                x += 1;
            }
            x += 2;
        }
        x += 3;
    }
}

void nested_for(int x) {
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            x++;
        }
    }
}

// Multi-level break and continue.

int exit_nested_loops(int x) {
    while (x) {
        while (x) {
            if (x)
                goto break_outer;
            x++;
        }
    }

break_outer:
    return x;
}

int continue_nested_loops(int x) {
top:
    while (x) {
        while (x) {
            if (x)
                goto top;
            x++;
        }
        x += 2;
    }

    return x;
}

int break_and_continue_nested_loops(int x) {
top:
    while (x) {
        while (x) {
            if (x)
                goto break_outer;
            if (x)
                goto top;
            x++;
        }
        x += 2;
    }

break_outer:
    return x;
}

// `continue` in `for` and `do` loops.
//
// `for` and `do` loops are fun because they require doing something at the
// *end* of every loop, which we don't have a direct analog for in Rust. When
// there's a `continue` in a `for` loop, we can't just `continue` in the
// translated Rust because that would skip the logic that's supposed to happen
// at the end of the loop. To handle this we instead generate a block in the
// loop body and use a labeled `break` to jump to the logic at the end of the
// loop.

int for_loop_single_continue(int x) {
    for (int i = 0; i < 10; i++) {
        if (x) {
            if (x)
                continue;
        }
        x += 1;
    }
    return x;
}

int for_loop_multi_continue(int x) {
    for (int i = 0; i < 10; i++) {
        if (x)
            if (x)
                continue;
        x += 1;
        if (x)
            if (x)
                continue;
        x += 2;
    }
    return x;
}

int do_loop_single_continue(int x) {
    do {
        if (x) {
            if (x)
                continue;
        }
        x += 1;
    } while (x);
    return x;
}

int do_loop_multi_continue(int x) {
    do {
        if (x)
            if (x)
                continue;
        x += 1;
        if (x)
            if (x)
                continue;
        x += 2;
    } while (x);
    return x;
}

// These tests verify that we pull the right nodes into a loop when there are
// multiple paths out of the loop. When there are multiple paths out of a loop
// it's valid to leave all of those branches outside of the loop, but doing so
// means that we have to wrap the loop in extra labeled blocks so that we can
// jump to the appropriate code path when exiting the loop. We avoid that by
// pulling in any nodes that can be cleanly inlined into a branch within the
// loop.

int explicit_loop_multi_exit(int x) {
    for (;;) {
        switch (x) {
        case 1:
            x += 1;
            continue;

        case 2:
            x += 2;
            if (x)
                x += 3;
            else
                x += 4;
            x += 5;
            goto out;

        default:
            x += 6;
            goto out;
        }
    }

out:
    x += 4;
    return x;
}

int implicit_loop_multi_exit(int x) {
redo:
    switch (x) {
    case 1:
        x += 1;
        goto redo;

    case 2:
        x += 2;
        if (x)
            x += 3;
        else
            x += 4;
        x += 5;
        break;

    default:
        x += 6;
        break;
    }

    x += 4;
    return x;
}

int loop_returns(int x) {
    while (x) {
        x += 1;
        if (x) {
            x += 2;
            return x;
        }
        x += 3;
        if (x) {
            x += 4;
            return x;
        }
    }

    return x;
}

// Tests that both of the returns land outside the loop, and that we have a
// block labeled `exit`.
//
// NOTE: The way the `if`s are getting turned into `if else`s is not correct,
// but we can't test this case without also triggering that issue.
int loop_gotos(int x) {
    while (x) {
        x += 1;
        if (x) {
            x += 2;
            goto exit;
        }
        x += 3;
        if (x) {
            x += 4;
            goto exit;
        }
        x += 5;
    }

    return x;

exit:
    return -x;
}

int loop_early_return(int x) {
    while (x) {
        x += 1;
        if (x) {
            x += 2;
            return x;
        }
    }

    return -x;
}

// The `if` is getting inverted to `if !x { continue; } x +=2; break;`.
/*
int loop_goto_exit(int x) {
    while (x) {
        x += 1;
        if (x) {
            x += 2;
            goto exit;
        }
    }

    return x;

exit:
    return -x;
}
*/

// These two cases exhibit a couple of bad behaviors, specifically related to
// interactions between the incremental relooper and the new relooper logic:
//
// - We end up with a labeled block containing a loop, i.e. `'a: { loop {} }`.
//   These are supposed to get merged, but something related to the incremental
//   relooper is preventing that from happening.
// - In the `for` version, initialization of `ii` is being placed inside the
//   labeled block, i.e. `'a: { ii = 0; loop { break 'a; } }`. This also
//   prevents us from applying the block label to the loop. This only happens
//   when incremental relooping is enabled.
// - Without the incremental relooper the code that's supposed to go after the
//   loop gets pulled into the loop.
// - We're failing to reconstruct `while` loops in both cases because of
//   previously mentioned issue with labeled blocks and `if` branches getting
//   merged together.
/*
int backwards_for_loop(int x) {
    for (int ii = 0; ii < 10; ii++) {
        if (x)
            continue;
        return x;
    }

    return -x;
}

int backwards_while_loop(int x) {
    int ii = 0;
    while (ii < 10) {
        if (x) {
            ii++;
            continue;
        }
        return x;
    }

    return -x;
}
*/

// Multiple loops in separate branches.

void if_else_loops(int x) {
    if (x) {
        while (x) {
            x += 1;
        }
    } else {
        while (x) {
            x += 2;
        }
    }
}

int goto_loops(int x) {
A:
    if (x) {
        goto B;
    } else {
        goto C;
    }

B:
    x += 1;
    if (x) {
        goto B;
    } else {
        goto D;
    }

C:
    x += 2;
    if (x) {
        goto C;
    } else {
        goto D;
    }

D:
    return x;
}

void goto_into_loops(int x) {
A:
    if (x) {
        goto B;
    } else {
        goto C;
    }

    while (x) {
B:
        x += 1;
    }
    goto D;

    while (x) {
C:
        x += 2;
    }
    goto D;

D:
    return;
}

void goto_separate_loops(int x) {
A:
    if (x) {
        goto B;
    } else {
        goto C;
    }

B:
    do {
        x += 1;
    } while (x);
    goto D;

C:
    do {
        x += 2;
    } while (x);
    goto D;

D:
    return;
}
