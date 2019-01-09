# xzoom

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Required Manual Changes

You may need to add `#include <unistd.h>` to xzoom.c for it to properly generate (otherwise `main_0` goes missing). This include is normally only added with the `TIMER` macro enabled, but seems to be required for standard functionality. (We could fork the repo if we want to make this change explicit for the purposes of automated testing.)

## Required Dependencies

* `clang` >= 5.0
* `sed`

## Transpiling

    $ clang -MJ compile_commands.o.json xzoom.c -L/usr/X11R6/lib -lX11
    $ sed -e '1s/^/[\n/' -e '$s/,$/\n]/' *.o.json > compile_commands.json
    $ c2rust transpile compile_commands.json
    $ rustc xzoom.rs  -L/usr/X11R6/lib -lX11
