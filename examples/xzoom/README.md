# xzoom

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Required Manual Changes

You may need to add `#include <unistd.h>` to xzoom.c for it to properly generate (otherwise `main_0` goes missing). This include is normally only added with the `TIMER` macro enabled, but seems to be required for standard functionality. (We could fork the repo if we want to make this change explicit for the purposes of automated testing.)

## Required Exporter Params

No params are strictly required at this time.

## Required Importer Params

Pass the `--translate-entry` flag to the importer so that a valid rust main function is generated. The relooper `--reloop-cfgs` is also required to generate certain portions of code.

## Linking

Link in X11 with `-L/usr/X11R6/lib -lX11` when compiling C/Rust code.
