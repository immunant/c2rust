// Test restoration of #[cfg_attr] during macro collapsing.  Both mods' attrs should be passed
// through unmodified.

#[cfg_attr(all(), allow(warnings))]
mod a {}

#[cfg_attr(any(), allow(warnings))]
mod b {}

fn main() {
    // Need something to rewrite, otherwise `-r alongside` won't actually produce the new file.
    let x = 1 + 1;
}
