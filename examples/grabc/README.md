# grabc

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Required Manual Changes

The translator will generate this bit of code which will not compile: `static mut cross_cursor: Cursor = ::std::ptr::null_mut::<libc::c_void>() as Cursor;`. This should be replaced with the equivalent code: `static mut cross_cursor: Cursor = 0;` (`Cursor` is just an alias for an int)

## Required Exporter Params

No params are strictly required at this time.

## Required Importer Params

Pass the `--translate-entry` flag to the importer so that a valid rust main function is generated. The relooper `--reloop-cfgs` is also required to generate certain portions of code.

## Linking

Link in X11 with `-L/usr/X11R6/lib -lX11` when compiling C/Rust code.
