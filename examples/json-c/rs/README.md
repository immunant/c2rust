This directory contains the Rust translation of the libjson-c C code in the
directory above.

# Running `idiomize`

The root of the crate is `lib.rs`.  You must also pass `--crate-type rlib` as
part of the `rustc` arguments, else you will get an error about missing `main`.

# Ownership analysis

The `idiomize` ownership analysis works on this code, but requires a few
annotations as input:

    ./run-idiomize.sh \
        select box '
            crate;
            desc(
                (foreign_item && fn && (name("free") || name("realloc"))) ||
                (field && path(::arraylist::array_list::free_fn)) ||
                (field && path(::json_object::json_object::_delete))
            );
            desc(match_ty(*mut __t));
        ' \; \
        test_analysis_ownership     # or other idiomize commands


