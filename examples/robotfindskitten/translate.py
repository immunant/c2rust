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


# List of rust-refactor commands to run.

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


        # Replace printf/printw/etc with formatting macros.

        r'''
            select target 'crate; desc(mod && name("ncurses"));' ;
            create_item '
                macro_rules! printw {
                    ($($args:tt)*) => {
                        unsafe {
                            ::printw(b"%s\0" as *const u8 as *const libc::c_char,
                                     ::std::ffi::CString::new(format!($($args)*))
                                        .unwrap().as_ptr())
                        }
                    };
                }
            ' after
        ''',
        '''
            select printw 'item(printw);' ;

            copy_marks printw fmt_arg ;
            mark_arg_uses 0 fmt_arg ;

            select fmt_str 'marked(fmt_arg); desc(expr && !match_expr(__e as __t));' ;

            copy_marks printw calls ;
            mark_callers calls ;

            rename_marks fmt_arg target ;
            convert_format_string ;
            delete_marks target ;

            rename_marks calls target ;
            func_to_macro printw ;
        ''',


        r'''
            select target 'crate; desc(item && name("printw"));' ;
            create_item '
                macro_rules! mvprintw {
                    ($y:expr, $x:expr, $($args:tt)*) => {
                        unsafe {
                            ::mvprintw($y, $x, b"%s\0" as *const u8 as *const libc::c_char,
                                     ::std::ffi::CString::new(format!($($args)*))
                                        .unwrap().as_ptr())
                        }
                    };
                }
            ' after
        ''',
        '''
            select mvprintw 'item(mvprintw);' ;

            copy_marks mvprintw fmt_arg ;
            mark_arg_uses 2 fmt_arg ;

            select fmt_str 'marked(fmt_arg); desc(expr && !match_expr(__e as __t));' ;

            copy_marks mvprintw calls ;
            mark_callers calls ;

            rename_marks fmt_arg target ;
            convert_format_string ;
            delete_marks target ;

            rename_marks calls target ;
            func_to_macro mvprintw ;
        ''',

        '''
            select printf 'item(printf);' ;

            copy_marks printf fmt_arg ;
            mark_arg_uses 0 fmt_arg ;

            select fmt_str 'marked(fmt_arg); desc(expr && !match_expr(__e as __t));' ;

            copy_marks printf calls ;
            mark_callers calls ;

            rename_marks fmt_arg target ;
            convert_format_string ;
            delete_marks target ;

            rename_marks calls target ;
            func_to_macro print ;
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
            select user 'crate; desc(fn && !name("main"));' ;
            static_to_local_ref
        ''',


        # Define safe wrappers for ncurses functions

        '''
            abstract 'lines() -> libc::c_int' 'LINES' ;
            abstract 'cols() -> libc::c_int' 'COLS' ;

            abstract 'initscr_()' 'initscr()' '{ initscr(); }' ;
            abstract 'nonl_() -> libc::c_int' 'nonl()' ;
            abstract 'noecho_() -> libc::c_int' 'noecho()' ;
            abstract 'cbreak_() -> libc::c_int' 'cbreak()' ;
            abstract 'has_colors_() -> bool' 'has_colors()' ;
            abstract 'start_color_() -> libc::c_int' 'start_color()' ;
            abstract 'endwin_() -> libc::c_int' 'endwin()' ;

            abstract 'refresh() -> libc::c_int' 'wrefresh(stdscr)' ;
            abstract 'move_(y: libc::c_int, x: libc::c_int) -> libc::c_int'
                'wmove(stdscr, y, x)' ;
            abstract 'clear() -> libc::c_int' 'wclear(stdscr)' ;
            abstract 'clrtoeol() -> libc::c_int' 'wclrtoeol(stdscr)' ;
            abstract 'getch() -> libc::c_int' 'wgetch(stdscr)' ;
            abstract 'addch(ch: chtype) -> libc::c_int' 'waddch(stdscr, ch)' ;
            abstract 'addnstr(str: &[u8], n: libc::c_int) -> libc::c_int'
                'waddnstr(stdscr, str as *const u8 as *const libc::c_char, n)'
                'waddnstr(stdscr, str.as_ptr() as *const libc::c_char, n)' ;
            abstract
                'attr_get(attrs: &mut attr_t, pair: &mut libc::c_short) -> libc::c_int'
                'wattr_get(stdscr, attrs, pair, __e)'
                'wattr_get(stdscr, attrs, pair, ::std::ptr::null_mut())' ;
            abstract
                'attrset(attrs: libc::c_int) -> libc::c_int'
                'wattrset(stdscr, attrs)' ;

            abstract 'init_pair_(pair: libc::c_short,
                                 f: libc::c_short,
                                 b: libc::c_short) -> libc::c_int'
                'init_pair(pair, f, b)' ;


            select target 'item(ncurses);' ;
            create_item '
                #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
                pub enum WindowToken {
                    StdScr,
                    CurScr,
                }

                impl WindowToken {
                    pub fn as_ptr(self) -> *mut ::WINDOW {
                        match self {
                            WindowToken::StdScr => unsafe { ::stdscr },
                            WindowToken::CurScr => unsafe { ::curscr },
                        }
                    }
                }
            ' inside ;

            rewrite_expr 'stdscr' 'ncurses::WindowToken::StdScr.as_ptr()' ;
            rewrite_expr 'curscr' 'ncurses::WindowToken::CurScr.as_ptr()' ;

            abstract 'keypad_(win: ncurses::WindowToken, bf: bool) -> libc::c_int'
                'keypad(win.as_ptr(), bf)' ;
            abstract 'intrflush_(win: ncurses::WindowToken, bf: bool) -> libc::c_int'
                'intrflush(win.as_ptr(), bf)' ;
            abstract 'wrefresh_(win: ncurses::WindowToken) -> libc::c_int'
                'wrefresh(win.as_ptr())' ;

            copy_marks new target ;
            sink_unsafe ;
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

        # Mark all functions as safe, except for main_0.
        '''
            select target 'crate; desc(fn && !name("main_0"));' ;
            set_unsafety safe ;
        ''',
]




idiomize = get_cmd_or_die(config.RREF_BIN)

def run_idiomize(args, mode='inplace'):
    full_args = ['-r', mode, '--cargo'] + args

    ld_lib_path = get_rust_toolchain_libpath()

    # don't overwrite existing ld lib path if any...
    if 'LD_LIBRARY_PATH' in local.env:
        ld_lib_path += ':' + local.env['LD_LIBRARY_PATH']

    with local.env(RUST_BACKTRACE='1',
                   LD_LIBRARY_PATH=ld_lib_path):
        with local.cwd(os.path.join(RFK_DIR, 'rust')):
            idiomize[full_args]()


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
    rf_hash = RefactorHash(idiomize, src_path)
    for refactor_str in REFACTORINGS:
        refactor_args = shlex.split(refactor_str)
        rf_hash.extend(refactor_str)

        cache_path = '%s.%s' % (src_path, rf_hash.hex)
        if os.path.isfile(cache_path):
            print('CACHED: %r' % (refactor_args,))
            shutil.copy(cache_path, src_path)
        else:
            print('REFACTOR: %r' % (refactor_args,))
            run_idiomize(refactor_args)
            shutil.copy(src_path, cache_path)


if __name__ == '__main__':
    main()
