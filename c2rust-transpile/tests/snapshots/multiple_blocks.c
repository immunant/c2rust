void multiple_blocks(int a, int b, int c, int d, int e, int f) {
    // First irreducible region: L1 and L2 are both jumped into from
    // outside and from each other, so no single entry dominates both.
    if (a) goto L1;
    goto L2;
L1:
    if (b) goto L2;
    goto L3;
L2:
    if (c) goto L1;
L3:
    ;

    // Second, independent irreducible region: same shape, different labels,
    // so this should become its own separate C2Rust_Block group.
    if (d) goto M1;
    goto M2;
M1:
    if (e) goto M2;
    goto M3;
M2:
    if (f) goto M1;
M3:
    ;
}
