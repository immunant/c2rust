# C2Rust-Bitfields Crate

This crate is used to generate structs with bitfields in [c2rust](https://www.github.com/immunant/c2rust) translations. It has three primary goals:

* Byte compatibility with equivalent C bitfield structs
* The ability to take references/pointers to non bitfield fields
* Provide methods to read from and write to bitfields

We currently provide a single custom derive, `BitfieldStruct`, as well as a dependent field attribute `bitfield`. The dependent field attribute `padding` may optionally be used as a no-op marker for automated tools.

## Requirements

* Rust 1.30+
* Rust Stable, Beta, or Nightly
* Little Endian Architecture

## Example

Suppose you want to write a super compact date struct which only takes up three bytes. In C this would look like this:

```c
struct date {
    unsigned char day: 5;
    unsigned char month: 4;
    unsigned short year: 15;
} __attribute__((packed));
```

Clang helpfully provides us with this information:

```c
*** Dumping AST Record Layout
         0 | struct date
     0:0-4 |   unsigned char day
     0:5-8 |   unsigned char month
    1:1-15 |   unsigned short year
           | [sizeof=3, align=1]
```

And this is enough to build our rust struct:

```rust
#[repr(C, align(1))]
#[derive(BitfieldStruct)]
struct Date {
    #[bitfield(name = "day", ty = "std::ffi::c_uchar", bits = "0..=4")]
    #[bitfield(name = "month", ty = "std::ffi::c_uchar", bits = "5..=8")]
    #[bitfield(name = "year", ty = "std::ffi::c_ushort", bits = "9..=23")]
    day_month_year: [u8; 3]
}

fn main() {
    let mut date = Date {
        day_month_year: [0; 3]
    };

    date.set_day(18);
    date.set_month(7);
    date.set_year(2000);

    assert_eq!(date.day(), 18);
    assert_eq!(date.month(), 7);
    assert_eq!(date.year(), 2000);
}
```

Furthermore, C bitfield rules for overflow and signed integers are taken into account.

This crate can generate `no_std` compatible code when the `no_std` feature flag
is provided.

## Tests

Since rust doesn't support a `build.rs` exclusively for tests, you must manually compile the c test code and link it in.

```shell
$ clang tests/bitfields.c -c -fPIC -o tests/bitfields.o
$ ar -rc tests/libtest.a tests/bitfields.o
$ RUSTFLAGS="-L `pwd`/tests" cargo test
```

# Acknowledgements

This crate is inspired by the [rust-bitfield](https://github.com/dzamlo/rust-bitfield), [packed_struct](https://github.com/hashmismatch/packed_struct.rs), and [bindgen](https://github.com/rust-lang/rust-bindgen) crates.
