// Test restoration of #[cfg] during macro collapsing.

#[cfg(all())]
mod a {}

#[cfg(any())]
mod b {}

fn main() {
    // Need something to rewrite, otherwise `-r alongside` won't actually produce the new file.
    let x = 2;
}
