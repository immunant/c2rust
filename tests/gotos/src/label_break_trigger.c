// This function doesn't do anything interesting, so we aren't checking its
// inputs or outputs. However, it does have interesting control flow, so
// we do check that we can generate compiling code for it.
void triggers_label_break(void) {
lbl:
  if (0 < 1)
    return;
  else {
    if (0 < 1)
      goto lbl;
  }
  return;
}
