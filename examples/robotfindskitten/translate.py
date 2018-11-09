import json
import hashlib
import os
import shlex
import shutil
import sys
from plumbum.cmd import mv, mkdir, rename, sed, rustc, cargo, rm
from plumbum import local

# Path to the root of the robotfindskitten codebase
RFK_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), 'repo'))

sys.path.append(os.path.join(RFK_DIR, '../../../scripts'))
from common import *
import transpile


# List of c2rust-refactor commands to run.

REFACTORINGS = [
        '''
            select target 'crate;' ;
            create_item 'extern crate c2rust_runtime;' inside
        ''',

        'wrapping_arith_to_normal',


        # The two ugly expressions below are the result of expanding some
        # ncurses macros.  We turn them into actual calls to their non-macro
        # implementations so it's easier to manipulate them later.
        '''
            select target 'crate; child(foreign_mod);' ;
            create_item '
                extern "C" {
                    fn wattr_get(win: *mut WINDOW, attrs: *mut attr_t,
                        pair: *mut libc::c_short, opts: *mut libc::c_void) -> libc::c_int;
                    fn wattrset(win: *mut WINDOW, attrs: libc::c_int) -> libc::c_int;
                }
            ' after ;
            create_item 'mod ncurses {}' after ;

            rewrite_expr '
                if !(__win as *const libc::c_void).is_null() {
                    if !(&mut __attrs as *mut attr_t as *const libc::c_void).is_null() {
                        __attrs = (*__win)._attrs
                    } else {
                    };
                    if !(&mut __pair as *mut libc::c_short as *const libc::c_void).is_null() {
                        __pair = (((*__win)._attrs as libc::c_ulong
                            & (((1u32 << 8i32)) - 1u32 << 0i32 + 8i32) as libc::c_ulong)
                            >> 8i32) as libc::c_int as libc::c_short
                    } else {
                    };
                } else {
                }
            ' 'wattr_get(__win, &mut __attrs, &mut __pair, ::std::ptr::null_mut())' ;

            rewrite_expr '
                if !(__win as *const libc::c_void).is_null() {
                    (*__win)._attrs = __attrs
                } else {
                }
            ' 'wattrset(__win, __attrs as libc::c_int)' ;
        ''',


        'struct_assign_to_update',
        'struct_merge_updates',


        # Phase ordering:
        #  1. Convert printf-style functions to use format macros.  This means
        #     string arguments to formatting are now typechecked.
        #  2. Retype `ver` and `messages` to &str.  Fixing up the resulting
        #     errors depends on having proper typechecking of `ver` and
        #     `messages` uses.  After this, we can make `ver` and `messages`
        #     into non-mut statics.
        #  3. Collect remaining mut statics into `struct State`.  This is
        #     easier if we've already removed `mut` from all the immutable
        #     statics.


        # Replace printf/printw/etc with uses of formatting macros.

        r'''
            select func 'item(printw);' ;

            copy_marks func fmt_arg ;
            mark_arg_uses 0 fmt_arg ;

            select fmt_str 'marked(fmt_arg); desc(expr && !match_expr(__e as __t));' ;

            rename_marks fmt_arg target ;
            convert_format_args ;

            clear_marks ;

            select target 'crate; desc(mod && name("ncurses"));' ;
            create_item '
                fn fmt_printw(args: ::std::fmt::Arguments) -> libc::c_int {
                    unsafe {
                        ::printw(b"%s\0" as *const u8 as *const libc::c_char,
                                 ::std::ffi::CString::new(format!("{}", args))
                                     .unwrap().as_ptr())
                    }
                }
            ' after ;
            rewrite_expr 'printw' 'fmt_printw' ;
        ''',

        r'''
            select func 'item(mvprintw);' ;

            copy_marks func fmt_arg ;
            mark_arg_uses 2 fmt_arg ;

            select fmt_str 'marked(fmt_arg); desc(expr && !match_expr(__e as __t));' ;

            rename_marks fmt_arg target ;
            convert_format_args ;

            clear_marks ;

            select target 'crate; desc(mod && name("ncurses"));' ;
            create_item '
                fn fmt_mvprintw(y: libc::c_int, x: libc::c_int,
                                args: ::std::fmt::Arguments) -> libc::c_int {
                    unsafe {
                        ::mvprintw(y, x, b"%s\0" as *const u8 as *const libc::c_char,
                                 ::std::ffi::CString::new(format!("{}", args))
                                     .unwrap().as_ptr())
                    }
                }
            ' after ;
            rewrite_expr 'mvprintw' 'fmt_mvprintw' ;
        ''',

        r'''
            select func 'item(printf);' ;

            copy_marks func fmt_arg ;
            mark_arg_uses 0 fmt_arg ;

            select fmt_str 'marked(fmt_arg); desc(expr && !match_expr(__e as __t));' ;

            rename_marks fmt_arg target ;
            convert_format_args ;

            clear_marks ;

            select target 'crate; desc(mod && name("ncurses"));' ;
            create_item '
                fn fmt_printf(args: ::std::fmt::Arguments) -> libc::c_int {
                    print!("{}", args);
                    0
                }
            ' after ;
            rewrite_expr 'printf' 'fmt_printf' ;
        ''',


        # retype ver + messages

        # Change type of `ver`
        '''
            select target 'item(ver); mark(parent); child(match_ty(*mut libc::c_char));' ;
            rewrite_ty 'marked!(*mut libc::c_char)' "&'static str" ;
            delete_marks target ;
        '''
        # Remove casts from `ver` initializer
        '''
            select target 'marked(parent); desc(match_expr(__e as __t));' ;
            rewrite_expr 'marked!(__e as __t)' '__e' ;
            delete_marks target ;
        '''
        # Convert `ver` initializer from b"..." to "...".
        # Note we can't remove the null terminator yet because we're still
        # using CStr when doing the actual printing.
        '''
            select target 'marked(parent); child(expr);' ;
            bytestr_to_str ;
            delete_marks target ;
        '''
        # Fix up uses of `ver`
        '''
            type_fix_rules '*, &str, *const __t => __old.as_ptr()' ;
        ''',

        '''
            select target 'item(messages); mark(parent);
                child(ty); desc(match_ty(*mut libc::c_char));' ;
            rewrite_ty 'marked!(*mut libc::c_char)' "&'static str" ;
            delete_marks target ;
            select target 'marked(parent); desc(match_expr(__e as __t));' ;
            rewrite_expr 'marked!(__e as __t)' '__e' ;
            delete_marks target ;
            select target 'marked(parent); desc(expr);' ;
            bytestr_to_str ;
            delete_marks target ;
            type_fix_rules
                '*, &str, *const __t => __old.as_ptr()'
                '*, &str, *mut __t => __old.as_ptr() as *mut __t' ;
        ''',

        '''
            select target 'crate; child(static && name("ver|messages"));' ;
            set_mutability imm
        ''',


        # Convert `screen` to a memory-safe array

        '''
            select target 'crate; child(static && name("screen")); child(ty);' ;
            rewrite_ty 'marked!(*mut *mut __t)'
                '*mut ::c2rust_runtime::CBlockPtr<__t>' ;
            type_fix_rules
                'rval, *mut __t, ::c2rust_runtime::CBlockPtr<__u> =>
                    unsafe { ::c2rust_runtime::CBlockPtr::from_ptr(__old) }'
                'rval, *mut __t, *mut __u => __old as *mut __u'
                ;
            rewrite_expr
                '*typed!(__e, ::c2rust_runtime::block_ptr::CBlockOffset<__t>)'
                '*__e.as_mut()' ;
        ''',

        '''
            select target 'crate; child(static && name("screen")); child(ty);' ;
            rewrite_ty 'marked!(*mut __t)'
                '::c2rust_runtime::CBlockPtr<__t>' ;
            type_fix_rules
                'rval, *mut __t, ::c2rust_runtime::CBlockPtr<__u> =>
                    unsafe { ::c2rust_runtime::CBlockPtr::from_ptr(__old) }'
                'rval, *mut __t, *mut __u => __old as *mut __u'
                ;
            rewrite_expr
                '*typed!(__e, ::c2rust_runtime::block_ptr::CBlockOffset<__t>)'
                '*__e.as_mut()' ;
        ''',

        '''
            rewrite_expr 'malloc(__e) as *mut __t as *mut __u' 'malloc(__e) as *mut __u' ;
            rewrite_expr
                '::c2rust_runtime::CBlockPtr::from_ptr(malloc(__e) as *mut __t)'
                '::c2rust_runtime::CBlockPtr::alloc(
                    __e as usize / ::std::mem::size_of::<__t>())'
                ;
        ''',

        '''
            rewrite_ty '::c2rust_runtime::CBlockPtr<__t>' '::c2rust_runtime::CArray<__t>' ;
            rewrite_expr
                '::c2rust_runtime::CBlockPtr::from_ptr'
                '::c2rust_runtime::CArray::from_ptr' ;
            rewrite_expr
                '::c2rust_runtime::CBlockPtr::alloc'
                '::c2rust_runtime::CArray::alloc' ;
            rewrite_expr
                'typed!(__e, ::c2rust_runtime::CArray<__t>).offset(__f)'
                '__e.offset_mut(__f)' ;
            rewrite_expr
                'typed!(__e, ::c2rust_runtime::CArray<__t>).offset_mut(__f).as_mut()'
                '&mut __e[__f as usize]' ;
            rewrite_expr '*&mut __e' '__e' ;
        ''',


        # Convert ncurses calls into pancurses Window methods
        '''
            select target 'crate;' ;
            create_item 'extern crate pancurses;' inside ;
        ''',

        '''
            select target 'crate;' ;
            create_item 'static mut win: Option<::pancurses::Window> = None;' inside ;
        ''',

        '''
            select target 'item(initialize_ncurses);' ;
            create_item '
                fn encode_input(inp: Option<::pancurses::Input>) -> libc::c_int {
                    use ::pancurses::Input::*;
                    let inp = match inp {
                        Some(x) => x,
                        None => return -1,
                    };
                    match inp {
                        // TODO: unicode inputs in the range 256 .. 512 can
                        // collide with ncurses special keycodes
                        Character(c) => c as u32 as libc::c_int,
                        Unknown(i) => i,
                        special => {
                            let idx = ::pancurses::SPECIAL_KEY_CODES.iter()
                                .position(|&k| k == special).unwrap();
                            let code = idx as i32 + ::pancurses::KEY_OFFSET;
                            if code > ::pancurses::KEY_F15 {
                                code + 48
                            } else {
                                code
                            }
                        },
                    }
                }
            ' after ;
        ''',

        # Pure laziness: we write just `win` in all these replacement
        # templates, then replace it with `win.unwrap()` at the end.

        # Wholesale replacement of fmt_printw and fmt_mvprintw.
        r'''
            select target 'item(fmt_printw);' ;
            create_item '
                fn fmt_printw(args: ::std::fmt::Arguments) -> libc::c_int {
                    unsafe {
                        win.printw(&format!("{}", args))
                    }
                }
            ' after ;
            delete_items ;
            clear_marks ;

            select target 'item(fmt_mvprintw);' ;
            create_item '
                fn fmt_mvprintw(y: libc::c_int, x: libc::c_int,
                                args: ::std::fmt::Arguments) -> libc::c_int {
                    unsafe {
                        win.mvprintw(y, x, &format!("{}", args))
                    }
                }
            ' after ;
            delete_items ;
            clear_marks ;

            rewrite_expr 'LINES' 'win.get_max_y()' ;
            rewrite_expr 'COLS' 'win.get_max_x()' ;

            rewrite_expr 'nonl' '::pancurses::nonl' ;
            rewrite_expr 'noecho' '::pancurses::noecho' ;
            rewrite_expr 'cbreak' '::pancurses::cbreak' ;
            rewrite_expr 'has_colors' '::pancurses::has_colors' ;
            rewrite_expr 'start_color' '::pancurses::start_color' ;
            rewrite_expr 'endwin' '::pancurses::endwin' ;
            rewrite_expr 'init_pair' '::pancurses::init_pair' ;

            rewrite_expr 'wrefresh(stdscr)' 'win.refresh()' ;
            rewrite_expr 'wrefresh(curscr)' 'win.refresh()' ;
            rewrite_expr 'keypad(stdscr, __bf)' 'win.keypad(__bf)' ;
            rewrite_expr 'wmove(stdscr, __my, __mx)' 'win.mv(__my, __mx)' ;
            rewrite_expr 'wclear(stdscr)' 'win.clear()' ;
            rewrite_expr 'wclrtoeol(stdscr)' 'win.clrtoeol()' ;
            rewrite_expr 'wgetch(stdscr)' '::encode_input(win.getch())' ;
            rewrite_expr 'waddch(stdscr, __ch)' 'win.addch(__ch)' ;
            rewrite_expr
                'waddnstr(stdscr, __str as *const u8 as *const libc::c_char, __n)'
                "win.addnstr(::std::str::from_utf8(__str).unwrap().trim_end_matches('\0'),
                             __n as usize)" ;
            rewrite_expr
                'wattr_get(stdscr, __attrs, __pair, __e)'
                '{
                    let tmp = win.attrget();
                    *__attrs = tmp.0;
                    *__pair = tmp.1;
                    0
                }' ;
            rewrite_expr
                'wattrset(stdscr, __attrs)'
                'win.attrset(__attrs as ::pancurses::chtype)' ;

            rewrite_expr 'intrflush(__e, __f)' '0' ;

            rewrite_expr 'win' 'win.as_ref().unwrap()' ;

            rewrite_expr 'initscr()' 'win = Some(::pancurses::initscr())' ;
        ''',


        # Collect mutable statics into a single struct

        '''
            select target 'crate; child(static && mut);' ;
            static_collect_to_struct State S
        ''',
        '''
            select target 'crate; desc(fn && !name("main"));' ;
            set_visibility ''
        ''',
        '''
            select target 'crate; child(static && name("S"));' ;
            select user 'crate; desc(fn && !name("main|main_0"));' ;
            static_to_local_ref ;
            static_to_local ;
        ''',


        # Use safe alternatives to libc calls

        '''
            rewrite_expr 'sleep(__e)'
                '::std::thread::sleep(
                    ::std::time::Duration::from_secs(__e as u64))' ;

            select target 'crate;' ;
            create_item 'extern crate rand;' inside ;

            rewrite_expr 'rand()'
                '(::rand::random::<libc::c_uint>() >> 1) as libc::c_int' ;
            rewrite_expr 'srand(__e)' '()' ;

            rewrite_expr 'atoi(__e)'
                '<libc::c_int as ::std::str::FromStr>::from_str(
                    ::std::ffi::CStr::from_ptr(__e).to_str().unwrap()).unwrap()' ;
            clear_marks ;
            select target 'item(atoi);' ;
            delete_items ;

            rewrite_expr 'exit(__e)' '::std::process::exit(__e as i32)' ;
        ''',

        # We can't make `signal` safe, so give it an unsafe block.
        '''
            rewrite_expr 'signal(__e, __f)' 'unsafe { signal(__e, __f) }' ;
        ''',


        # Retype the string argument of `message`
        '''
            select target
                'item(message); child(arg); child(match_ty(*mut libc::c_char));' ;
            rewrite_ty 'marked!(*mut libc::c_char)' '&str' ;
            delete_marks target ;
            type_fix_rules
                '*, &str, *const __t =>
                    ::std::ffi::CString::new(__old.to_owned()).unwrap().as_ptr()'
                '*, *mut __t, &str =>
                    unsafe { ::std::ffi::CStr::from_ptr(__old).to_str().unwrap() }' ;
        ''',


        # Retype main_0's argv, and replace main

        '''
            select target 'item(main);' ;
            create_item '
                fn main() {
                    // Collect argv into a vector.
                    let mut args_owned: Vec<::std::ffi::CString> = Vec::new();
                    for arg in ::std::env::args() {
                        args_owned.push(::std::ffi::CString::new(arg).unwrap());
                    }

                    // Now that the length is known, we can build a CArray.
                    let mut args: ::c2rust_runtime::CArray<Option<&::std::ffi::CStr>> =
                        ::c2rust_runtime::CArray::alloc(args_owned.len() + 1);
                    for i in 0 .. args_owned.len() {
                        args[i] = Some(&args_owned[i]);
                    }
                    // The last element of `args` remains `None`.

                    unsafe {
                        ::std::process::exit(main_0(
                            (args.len() - 1) as libc::c_int,
                            args) as i32);
                    }
                }
            ' after ;
            delete_items ;
            clear_marks ;

            select target 'item(main);' ;
            create_item '
                fn opt_c_str_to_ptr(x: Option<&::std::ffi::CStr>) -> *const libc::c_char {
                    match x {
                        None => ::std::ptr::null(),
                        Some(x) => x.as_ptr(),
                    }
                }
            ' after ;
            clear_marks ;

            select target
                'item(main_0); child(arg && name("argv")); child(ty);' ;
            rewrite_ty 'marked!(*mut *mut libc::c_char)'
                '::c2rust_runtime::CArray<Option<&::std::ffi::CStr>>' ;
            delete_marks target ;

            type_fix_rules
                '*, ::c2rust_runtime::array::CArrayOffset<__t>, __u => *__old'
                '*, ::std::option::Option<&::std::ffi::CStr>, *const i8 =>
                    opt_c_str_to_ptr(__old)'
                ;
        ''',


        # Clean up str -> CStr -> str conversions
        r'''
            rewrite_expr
                '::std::ffi::CStr::from_ptr(
                    cast!(typed!(__e, ::std::ffi::CString).as_ptr()))'
                '__e' ;
            rewrite_expr
                '::std::ffi::CString::new(__e).unwrap().to_str()'
                'Some(&__e)' ;
            rewrite_expr
                '::std::ffi::CStr::from_ptr(
                    cast!(typed!(__e, &str).as_ptr())).to_str()'
                "Some(__e.trim_end_matches('\0'))" ;
            rewrite_expr
                '::std::ffi::CStr::from_ptr(
                    cast!(typed!(__e, &[u8; __f]))).to_str()'
                "Some(::std::str::from_utf8(__e).unwrap().trim_end_matches('\0'))" ;
            rewrite_expr
                '::std::ffi::CStr::from_ptr(cast!(opt_c_str_to_ptr(__e)))'
                '__e.unwrap()' ;

            select target
                'crate; desc(match_expr(::std::str::from_utf8(__e))); desc(expr);' ;
            bytestr_to_str ;
            type_fix_rules '*, &str, &[u8] => __old.as_bytes()' ;
            clear_marks ;

            rewrite_expr
                '::std::str::from_utf8(__e.as_bytes())'
                'Some(__e)' ;

        ''',

        '''
            rewrite_expr
                '::c2rust_runtime::CArray::from_ptr(cast!(0))'
                '::c2rust_runtime::CArray::empty()' ;
        ''',


        # Mark all functions as safe and clean up unsafe blocks
        '''
            select target 'crate; desc(fn);' ;
            set_unsafety safe ;
            fix_unused_unsafe ;
        ''',
]




refactor = get_cmd_or_die(config.RREF_BIN)

def run_refactor(args, mode='inplace'):
    full_args = ['-r', mode, '--cargo'] + args

    ld_lib_path = get_rust_toolchain_libpath()

    # don't overwrite existing ld lib path if any...
    if 'LD_LIBRARY_PATH' in local.env:
        ld_lib_path += ':' + local.env['LD_LIBRARY_PATH']

    with local.env(RUST_BACKTRACE='1',
                   LD_LIBRARY_PATH=ld_lib_path):
        with local.cwd(os.path.join(RFK_DIR, 'rust')):
            refactor[full_args]()


class RefactorHash:
    def __init__(self, cmd, src_path):
        h = hashlib.sha256()

        with open(cmd.executable, 'rb') as f:
            h.update(f.read())

        with open(src_path, 'rb') as f:
            h.update(f.read())

        self.hash = h.digest()
        self.hex = h.hexdigest()

    def extend(self, s):
        h = hashlib.sha256(self.hash)
        h.update(s.encode('utf-8'))
        self.hash = h.digest()
        self.hex = h.hexdigest()



def main():
    os.chdir(RFK_DIR)
    print('in %s' % RFK_DIR)


    # Remove object files that will confuse `transpile`
    rm['-f', 'src/robotfindskitten.o']()


    # Actually translate
    with open('compile_commands.json', 'r') as f:
        transpile.transpile_files(f,
                emit_build_files=False,
                verbose=True)


    # Move rust files into rust/src
    mkdir['-vp', 'rust/src']()
    mv['-v', local.path('src') // '*.rs', 'rust/src/']()


    # Refactor
    src_path = os.path.join(RFK_DIR, 'rust/src/robotfindskitten.rs')
    rf_hash = RefactorHash(refactor, src_path)
    for refactor_str in REFACTORINGS:
        refactor_args = shlex.split(refactor_str)
        rf_hash.extend(refactor_str)

        cache_path = '%s.%s' % (src_path, rf_hash.hex)
        if os.path.isfile(cache_path):
            print('CACHED: %r' % (refactor_args,))
            shutil.copy(cache_path, src_path)
        else:
            print('REFACTOR: %r' % (refactor_args,))
            run_refactor(refactor_args)
            shutil.copy(src_path, cache_path)


if __name__ == '__main__':
    main()
