#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_mut
)]
#![feature(extern_types, libc)]
extern crate libc;
extern "C" {
    pub type ldat;
    #[no_mangle]
    fn printf(_: *const libc::c_char, ...) -> libc::c_int;
    #[no_mangle]
    fn cbreak() -> libc::c_int;
    #[no_mangle]
    fn endwin() -> libc::c_int;
    #[no_mangle]
    fn has_colors() -> bool;
    #[no_mangle]
    fn initscr() -> *mut WINDOW;
    #[no_mangle]
    fn init_pair(_: libc::c_short, _: libc::c_short, _: libc::c_short) -> libc::c_int;
    #[no_mangle]
    fn intrflush(_: *mut WINDOW, _: bool) -> libc::c_int;
    #[no_mangle]
    fn keypad(_: *mut WINDOW, _: bool) -> libc::c_int;
    #[no_mangle]
    fn mvprintw(_: libc::c_int, _: libc::c_int, _: *const libc::c_char, ...) -> libc::c_int;
    #[no_mangle]
    fn noecho() -> libc::c_int;
    #[no_mangle]
    fn nonl() -> libc::c_int;
    #[no_mangle]
    fn printw(_: *const libc::c_char, ...) -> libc::c_int;
    #[no_mangle]
    fn start_color() -> libc::c_int;
    #[no_mangle]
    fn waddch(_: *mut WINDOW, _: chtype) -> libc::c_int;
    #[no_mangle]
    fn waddnstr(_: *mut WINDOW, _: *const libc::c_char, _: libc::c_int) -> libc::c_int;
    #[no_mangle]
    fn wclear(_: *mut WINDOW) -> libc::c_int;
    #[no_mangle]
    fn wclrtoeol(_: *mut WINDOW) -> libc::c_int;
    #[no_mangle]
    fn wgetch(_: *mut WINDOW) -> libc::c_int;
    #[no_mangle]
    fn wmove(_: *mut WINDOW, _: libc::c_int, _: libc::c_int) -> libc::c_int;
    #[no_mangle]
    fn wrefresh(_: *mut WINDOW) -> libc::c_int;
    #[no_mangle]
    static mut curscr: *mut WINDOW;
    #[no_mangle]
    static mut stdscr: *mut WINDOW;
    #[no_mangle]
    static mut COLS: libc::c_int;
    #[no_mangle]
    static mut LINES: libc::c_int;
    #[no_mangle]
    fn signal(__sig: libc::c_int, __handler: __sighandler_t) -> __sighandler_t;
    #[no_mangle]
    fn strtol(_: *const libc::c_char, _: *mut *mut libc::c_char, _: libc::c_int) -> libc::c_long;
    #[no_mangle]
    fn rand() -> libc::c_int;
    #[no_mangle]
    fn srand(__seed: libc::c_uint);
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn exit(_: libc::c_int) -> !;
    #[no_mangle]
    fn time(__timer: *mut time_t) -> time_t;
    #[no_mangle]
    fn sleep(__seconds: libc::c_uint) -> libc::c_uint;
}
pub type __time_t = libc::c_long;
pub type chtype = libc::c_uint;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _win_st {
    pub _cury: libc::c_short,
    pub _curx: libc::c_short,
    pub _maxy: libc::c_short,
    pub _maxx: libc::c_short,
    pub _begy: libc::c_short,
    pub _begx: libc::c_short,
    pub _flags: libc::c_short,
    pub _attrs: attr_t,
    pub _bkgd: chtype,
    pub _notimeout: bool,
    pub _clear: bool,
    pub _leaveok: bool,
    pub _scroll: bool,
    pub _idlok: bool,
    pub _idcok: bool,
    pub _immed: bool,
    pub _sync: bool,
    pub _use_keypad: bool,
    pub _delay: libc::c_int,
    pub _line: *mut ldat,
    pub _regtop: libc::c_short,
    pub _regbottom: libc::c_short,
    pub _parx: libc::c_int,
    pub _pary: libc::c_int,
    pub _parent: *mut WINDOW,
    pub _pad: pdat,
    pub _yoffset: libc::c_short,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdat {
    pub _pad_y: libc::c_short,
    pub _pad_x: libc::c_short,
    pub _pad_top: libc::c_short,
    pub _pad_left: libc::c_short,
    pub _pad_bottom: libc::c_short,
    pub _pad_right: libc::c_short,
}
pub type WINDOW = _win_st;
pub type attr_t = chtype;
pub type __sighandler_t = Option<unsafe extern "C" fn(_: libc::c_int) -> ()>;
pub type time_t = __time_t;
/*
 *robotfindskitten: A Zen simulation
 *
 *Copyright (C) 1997,2000 Leonard Richardson
 *                        leonardr@segfault.org
 *                        http://www.crummy.com/devel/
 *
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License as
 *   published by the Free Software Foundation; either version 2 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or EXISTANCE OF KITTEN.  See the GNU General
 *   Public License for more details.
 *
 *   http://www.gnu.org/copyleft/gpl.html
 *
 */
/*The messages go in a separate file because they are collectively
huge, and you might want to modify them. It would be nice to load
the messages from a text file at run time.*/
/*Constants for our internal representation of the screen.*/
/*Keycode constants*/
/*Nethack keycodes*/
/*EMACS keycodes - Subtracting 64 makes it a control command*/
/*Screen dimensions.*/
/*Macros for generating numbers in different ranges*/
/*I'm feeling randy()!*/
/*Row constants for the animation*/
/*This struct contains all the information we need to display an object
on the screen*/
#[derive(Copy, Clone)]
#[repr(C)]
pub struct screen_object {
    pub x: libc::c_int,
    pub y: libc::c_int,
    pub color: libc::c_int,
    pub bold: bool,
    pub character: libc::c_char,
}
static mut ver: *mut libc::c_char =
    b"1.7320508.406\x00" as *const u8 as *const libc::c_char as *mut libc::c_char;
unsafe extern "C" fn atoi(mut __nptr: *const libc::c_char) -> libc::c_int {
    return strtol(
        __nptr,
        0 as *mut libc::c_void as *mut *mut libc::c_char,
        10i32,
    ) as libc::c_int;
}
/*Be sure to change MESSAGES when you change the array, or bad things
will happen.*/
/*Also, take note that robotfindskitten.c and configure.in
currently have the version number hardcoded into them, and they
should reflect MESSAGES. */
/* Watch out for fenceposts.*/
static mut messages: [*mut libc::c_char; 406] = [
    b"\"I pity the fool who mistakes me for kitten!\", sez Mr. T.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"That\'s just an old tin can.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s an altar to the horse god.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A box of dancing mechanical pencils. They dance! They sing!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s an old Duke Ellington record.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A box of fumigation pellets.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A digital clock. It\'s stuck at 2:17 PM.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"That\'s just a charred human corpse.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"I don\'t know what that is, but it\'s not kitten.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"An empty shopping bag. Paper or plastic?\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Could it be... a big ugly bowling trophy?\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A coat hanger hovers in thin air. Odd.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Not kitten, just a packet of Kool-Aid(tm).\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A freshly-baked pumpkin pie.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A lone, forgotten comma, sits here, sobbing.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"ONE HUNDRED THOUSAND CARPET FIBERS!!!!!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s Richard Nixon\'s nose!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s Lucy Ricardo. \"Aaaah, Ricky!\", she says.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You stumble upon Bill Gates\' stand-up act.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just an autographed copy of the Kama Sutra.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the Will Rogers Highway. Who was Will Rogers, anyway?\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s another robot, more advanced in design than you but strangely immobile.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Leonard Richardson is here, asking people to lick him.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a stupid mask, fashioned after a beagle.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Your State Farm Insurance(tm) representative!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the local draft board.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Seven 1/4\" screws and a piece of plastic.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"An 80286 machine.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"One of those stupid \"Homes of the Stars\" maps.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A signpost saying \"TO KITTEN\". It points in no particular direction.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A hammock stretched between a tree and a volleyball pole.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A Texas Instruments of Destruction calculator.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a dark, amphorous blob of matter.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just a pincushion.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a mighty zombie talking about some love and prosperity.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"Dear robot, you may have already won our 10 MILLION DOLLAR prize...\"\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s just an object.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A mere collection of pixels.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A badly dented high-hat cymbal lies on its side here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A marijuana brownie.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A plush Chewbacca.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Daily hunger conditioner from Australasia\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just some stuff.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Why are you touching this when you should be finding kitten?\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A glorious fan of peacock feathers.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s some compromising photos of Babar the Elephant.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A copy of the Weekly World News. Watch out for the chambered nautilus!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s the proverbial wet blanket.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A \"Get Out of Jail Free\" card.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"An incredibly expensive \"Mad About You\" collector plate.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Paul Moyer\'s necktie.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A haircut and a real job. Now you know where to get one!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"An automated robot-hater. It frowns disapprovingly at you.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"An automated robot-liker. It smiles at you.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a black hole. Don\'t fall in!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just a big brick wall.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"You found kitten! No, just kidding.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Heart of Darkness brand pistachio nuts.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A smoking branding iron shaped like a 24-pin connector.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a Java applet.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"An abandoned used-car lot.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A shameless plug for Crummy: http://www.crummy.com/\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A shameless plug for the UCLA Linux Users Group: http://linux.ucla.edu/\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A can of Spam Lite.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This is another fine mess you\'ve gotten us into, Stanley.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s scenery for \"Waiting for Godot\".\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This grain elevator towers high above you.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A Mentos wrapper.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s the constellation Pisces.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a fly on the wall. Hi, fly!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This kind of looks like kitten, but it\'s not.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a banana! Oh, joy!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A helicopter has crashed here.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Carlos Tarango stands here, doing his best impression of Pat Smear.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A patch of mushrooms grows here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A patch of grape jelly grows here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A spindle, and a grindle, and a bucka-wacka-woom!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A geyser sprays water high into the air.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A toenail? What good is a toenail?\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You\'ve found the fish! Not that it does you much good in this game.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A Buttertonsils bar.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"One of the few remaining discoes.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Ah, the uniform of a Revolutionary-era minuteman.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A punch bowl, filled with punch and lemon slices.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s nothing but a G-thang, baby.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"IT\'S ALIVE! AH HA HA HA HA!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This was no boating accident!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Wait! This isn\'t the poker chip! You\'ve been tricked! DAMN YOU, MENDEZ!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A livery stable! Get your livery!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a perpetual immobility machine.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"\"On this spot in 1962, Henry Winkler was sick.\"\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"There\'s nothing here; it\'s just an optical illusion.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The World\'s Biggest Motzah Ball!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A tribe of cannibals lives here. They eat Malt-O-Meal for breakfast, you know.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This appears to be a rather large stack of trashy romance novels.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Look out! Exclamation points!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A herd of wild coffee mugs slumbers here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a limbo bar! How low can you go?\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the horizon. Now THAT\'S weird.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A vase full of artificial flowers is stuck to the floor here.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A large snake bars your way.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A pair of saloon-style doors swing slowly back and forth here.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s an ordinary bust of Beethoven... but why is it painted green?\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s TV\'s lovable wisecracking Crow! \"Bite me!\", he says.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Hey, look, it\'s war. What is it good for? Absolutely nothing. Say it again.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s the amazing self-referential thing that\'s not kitten.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A flamboyant feather boa. Now you can dress up like Carol Channing!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"Sure hope we get some rain soon,\" says Farmer Joe.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"\"How in heck can I wash my neck if it ain\'t gonna rain no more?\" asks Farmer Al.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"\"Topsoil\'s all gone, ma,\" weeps Lil\' Greg.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This is a large brown bear. Oddly enough, it\'s currently peeing in the woods.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A team of arctic explorers is camped here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This object here appears to be Louis Farrakhan\'s bow tie.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This is the world-famous Chain of Jockstraps.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A trash compactor, compacting away.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This toaster strudel is riddled with bullet holes!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a hologram of a crashed helicopter.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This is a television. On screen you see a robot strangely similar to yourself.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This balogna has a first name, it\'s R-A-N-C-I-D.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A salmon hatchery? Look again. It\'s merely a single salmon.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a rim shot. Ba-da-boom!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s creepy and it\'s kooky, mysterious and spooky. It\'s also somewhat ooky.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This is an anagram.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This object is like an analogy.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a symbol. You see in it a model for all symbols everywhere.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The object pushes back at you.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A traffic signal. It appears to have been recently vandalized.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"There is no kitten!\" cackles the old crone. You are shocked by her blasphemy.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This is a Lagrange point. Don\'t come too close now.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"The dirty old tramp bemoans the loss of his harmonica.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Look, it\'s Fanny the Irishman!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"What in blazes is this?\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s the instruction manual for a previous version of this game.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A brain cell. Oddly enough, it seems to be functioning.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Tea and/or crumpets.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This jukebox has nothing but Cliff Richards albums in it.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a Quaker Oatmeal tube, converted into a drum.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This is a remote control. Being a robot, you keep a wide berth.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a roll of industrial-strength copper wire.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Oh boy! Grub! Er, grubs.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A puddle of mud, where the mudskippers play.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Plenty of nothing.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Look at that, it\'s the Crudmobile.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just Walter Mattheau and Jack Lemmon.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Two crepes, two crepes in a box.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"An autographed copy of \"Primary Colors\", by Anonymous.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Another rabbit? That\'s three today!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a segmentation fault. Core dumped, by the way.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A historical marker showing the actual location of /dev/null.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Thar\'s Mobius Dick, the convoluted whale. Arrr!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a charcoal briquette, smoking away.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A pizza, melting in the sun.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a \"HOME ALONE 2: Lost in New York\" novelty cup.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A stack of 7 inch floppies wobbles precariously.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s nothing but a corrupted floppy. Coaster anyone?\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A section of glowing phosphor cells sings a song of radiation to you.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This TRS-80 III is eerily silent.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A toilet bowl occupies this space.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This peg-leg is stuck in a knothole!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a solitary vacuum tube.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This corroded robot is clutching a mitten.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"\"Hi, I\'m Anson Williams, TV\'s \'Potsy\'.\"\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This subwoofer was blown out in 1974.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Three half-pennies and a wooden nickel.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the missing chapter to \"A Clockwork Orange\".\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a burrito stand flyer. \"Taqueria El Ranchito\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This smiling family is happy because they eat LARD.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Roger Avery, persona un famoso de los Estados Unidos.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Ne\'er but a potted plant.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A parrot, kipping on its back.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A forgotten telephone switchboard.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A forgotten telephone switchboard operator.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s an automated robot-disdainer. It pretends you\'re not there.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a portable hole. A sign reads: \"Closed for the winter\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Just a moldy loaf of bread.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A little glass tub of Carmex. ($.89) Too bad you have no lips.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A Swiss-Army knife. All of its appendages are out. (toothpick lost)\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a zen simulation, trapped within an ASCII character.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a copy of \"The Rubaiyat of Spike Schudy\".\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s \"War and Peace\" (unabridged, very small print).\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A willing, ripe tomato bemoans your inability to digest fruit.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A robot comedian. You feel amused.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s KITT, the talking car.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Here\'s Pete Peterson. His batteries seem to have long gone dead.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"Blup, blup, blup\", says the mud pot.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"More grist for the mill.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Grind \'em up, spit \'em out, they\'re twigs.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"The boom box cranks out an old Ethel Merman tune.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s \"Finding kitten\", published by O\'Reilly and Associates.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Pumpkin pie spice.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s the Bass-Matic \'76! Mmm, that\'s good bass!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"\"Lend us a fiver \'til Thursday\", pleas Andy Capp.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a tape of \'70s rock. All original hits! All original artists!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"You\'ve found the fabled America Online disk graveyard!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Empty jewelboxes litter the landscape.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the astounding meta-object.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Ed McMahon stands here, lost in thought. Seeing you, he bellows, \"YES SIR!\"\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"...thingy???\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s 1000 secrets the government doesn\'t want you to know!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The letters O and R.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A magical... magic thing.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a moment of silence.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s Sirhan-Sirhan, looking guilty.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s \"Chicken Soup for the Kitten-seeking Soulless Robot.\"\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It is a set of wind-up chatter teeth.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It is a cloud shaped like an ox.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You see a snowflake here, melting slowly.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a big block of ice. Something seems to be frozen inside it.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Vladimir Lenin\'s casket rests here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a copy of \"Zen and The Art of Robot Maintenance\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This invisible box contains a pantomime horse.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A mason jar lies here open. It\'s label reads: \"do not open!\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A train of thought chugs through here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This jar of pickles expired in 1957.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Someone\'s identity disk lies here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"\"Yes!\" says the bit.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"\"No!\" says the bit.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A dodecahedron bars your way.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Mr. Hooper is here, surfing.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a big smoking fish.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"You have new mail in /var/spool/robot\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just a monitor with the blue element burnt out.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A pile of coaxial plumbing lies here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a rotten old shoe.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a hundred-dollar bill.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a Dvorak keyboard.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a cardboard box full of 8-tracks.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just a broken hard drive containg the archives of Nerth Pork.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A broken metronome sits here, it\'s needle off to one side.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A sign reads: \"Go home!\"\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A sign reads: \"No robots allowed!\"\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the handheld robotfindskitten game, by Tiger.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This particular monstrosity appears to be ENIAC.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This is a tasty-looking banana creme pie.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A wireframe model of a hot dog rotates in space here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just the empty husk of a locust.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You disturb a murder of crows.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a copy of the robotfindskitten EULA.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s Death.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s an autographed copy of \"Secondary Colors,\" by Bob Ross.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It is a marzipan dreadnought that appears to have melted and stuck.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a DVD of \"Crouching Monkey, Hidden Kitten\", region encoded for the moon.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s Kieran Hervold.  Damn dyslexia!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A non-descript box of crackers.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Carbonated Water, High Fructose Corn Syrup, Color, Phosphoric Acid, Flavors, Caffeine.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"\"Move along! Nothing to see here!\"\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the embalmed corpse of Vladimir Lenin.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A coupon for one free steak-fish at your local family diner.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A set of keys to a 2001 Rolls Royce. Worthless.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A gravestone stands here.  \"Izchak Miller, ascended.\"\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Someone has written \"ad aerarium\" on the ground here.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A large blue eye floats in midair.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This appears to be a statue of Perseus.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"There is an opulent throne here.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a squad of Keystone Kops.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This seems to be junk mail addressed to the finder of the Eye of Larn.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A wondrous and intricate golden amulet.  Too bad you have no neck.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The swampy ground around you seems to stink with disease.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"An animate blob of acid.  Being metallic, you keep well away.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a copy of Knuth with the chapter on kitten-search algorithms torn out.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A crowd of people, and at the center, a popular misconception.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a blind man. When you touch, he exclaims \"It\'s a kitten prospecting robot!\"\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a lost wallet. It\'s owner didn\'t have pets, so you discard it.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This place is called Antarctica. There is no kitten here.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a mousetrap, baited with soap.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A book with \"Don\'t Panic\" in large friendly letters across the cover.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A compendium of haiku about metals.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A discredited cosmology, relic of a bygone era.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A hollow voice says \"Plugh\".\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A knight who says \"Either I am an insane knave, or you will find kitten.\"\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A neural net -- maybe it\'s trying to recognize kitten.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A screwdriver.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A statue of a girl holding a goose like the one in Gottingen, Germany.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A tetradrachm dated \"42 B.C.\"\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A voice booms out \"Onward, kitten soldiers...\"\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"An eminently forgettable zahir.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Apparently, it\'s Edmund Burke.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"For a moment, you feel something in your hands, but it disappears!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Here is a book about Robert Kennedy.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Hey, robot, leave those lists alone.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Ho hum.  Another synthetic a posteriori.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s Asimov\'s Laws of Robotics.  You feel a strange affinity for them.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s Bach\'s Mass in B-minor!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a bug.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a synthetic a priori truth!  Immanuel would be so pleased!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s the Tiki Room.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Just some old play by a Czech playwright, and you can\'t read Czech.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Kitten is the letter \'Q\'.  Oh, wait, maybe not.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Quidquid Latine dictum sit, kitten non est.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Sutro Tower is visible at some distance through the fog.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The Digital Millennium Copyright Act of 1998.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"The United States Court of Appeals for the Federal Circuit.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The non-kitten item like this but with \"false\" and \"true\" switched is true.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"The non-kitten item like this but with \"true\" and \"false\" switched is false.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This is the chapter called \"A Map of the Cat?\" from Feynman\'s autobiography.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This is the forest primeval.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Werner\'s \"Pocket Field Guide to Things That Are Not Kitten\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"You found nettik, but that\'s backwards.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You have found some zinc, but you must not stop here, for you must find kitten.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"\"50 Years Among the Non-Kitten Items\", by Ann Droyd.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"Robot may not injure kitten, or, through inaction, ...\"\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"Address Allocation for Private Internets\" by Yakov Rekhter et al.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"Mail Routing and the Domain System\" by Craig Partridge.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"\"The Theory and Practice of Oligarchical Collectivism\" by Emmanuel Goldstein.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"\"201 Kitten Verbs, Fully Conjugated\".  You look for \"find\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A card shark sits here, practicing his Faro shuffle.  He ignores you.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A copy of DeCSS.  They\'re a dime a dozen these days.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A demonic voice proclaims \"There is no kitten, only Zuul\".  You flee.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A lotus.  You make an interesting pair.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A milk carton, with a black and white picture of kitten on the side.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Any ordinary robot could see from a mile away that this wasn\'t kitten.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A stegosaurus, escaped from the stegosaurusfindsrobot game.  It finds you.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Baling wire and chewing gum.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Chewing gum and baling wire.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Here is no kitten but only rock, rock and no kitten and the sandy road.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Hey, I bet you thought this was kitten.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It is an ancient mariner, and he stoppeth one of three.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It pleases you to be kind to what appears to be kitten -- but it\'s not!\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a blatant plug for Ogg Vorbis, http://www.vorbis.com/\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a business plan for a new startup, kitten.net.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s a revised business plan for a new startup, my.kitten.net.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a square.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It seems to be a copy of \"A Tail of Two Kitties\".\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the Donation of Constantine!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s this message, nothing more.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Lysine, an essential amino acid.  Well, maybe not for robots.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"No kitten here.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"The score for a Czech composer\'s \"Kitten-Finding Symphony in C\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This looks like Bradley\'s \"Appearance and Reality\", but it\'s really not.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This non-kitten item no verb.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"You feel strangely unfulfilled.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"You hit the non-kitten item.  The non-kitten item fails to yowl.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"You suddenly yearn for your distant homeland.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You\'ve found the snows of yesteryear!  So that\'s where they all went to.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Approaching.  One car.  J.  Followed by.  Two car.  M, M.  In five. Minutes.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Free Jon Johansen!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Free Dmitry Sklyarov!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"One person shouts \"What do we want?\" The crowd answers \"Free Dmitry!\"\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Judith Platt insults librarians.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This map is not the territory.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"\"Go back to Libraria!\", says Pat Schroeder.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This is a porcelain kitten-counter.  0, 0, 0, 0, 0...\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"An old bootable business card, unfortunately cracked down the middle.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A kitten sink, for washing kitten (if only kitten liked water).\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A kitten source (to match the kitten sink).\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"If it\'s one thing, it\'s not another.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"If it\'s not one thing, it\'s another.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A caboodle.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A grin.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A hedgehog.  It looks like it knows something important.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"You\'ve found... Oh wait, that\'s just a cat.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Robot should not be touching that.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Air Guitar!!!  NA na NA na!!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"An aromatherapy candle burns with healing light.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You find a bright shiny penny.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a free Jon Johansen!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a free Dmitry Sklyarov!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"The rothe hits!  The rothe hits!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s an Internet chain letter about sodium laureth sulfate.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Ed Witten sits here, pondering string theory.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Something is written here in the dust.  You read: \"rJbotf ndQkttten\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"We wish you a merry kitten, and a happy New Year!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Run away!  Run away!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"You can see right through this copy of Brin\'s \"Transparent Society\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This copy of \"Steal This Book\" has been stolen from a bookstore.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s Roya Naini.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This kit is the fourteenth in a series of kits named with Roman letters.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"This is the tenth key you\'ve found so far.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You find a fraud scheme in which loans are used as security for other loans.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s the phrase \"and her\", written in ancient Greek.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s the author of \"Randomness and Mathematical Proof\".\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s the crusty exoskeleton of an arthropod!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s Emporer Shaddam the 4th\'s planet!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the triangle leg adjacent to an angle divided by the leg opposite it.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a bottle of nail polish remover.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"You found netkit! Way to go, robot!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s the ASCII Floating Head of Seth David Schoen!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A frosted pink party-cake, half eaten.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"A bitchin\' homemade tesla coil.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Conan O\'Brian, sans jawbone.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s either a mirror, or another soulless kitten-seeking robot.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Preoccupation with finding kitten prevents you from investigating further.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Fonzie sits here, mumbling incoherently about a shark and a pair of waterskis.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"The ghost of your dance instructor, his face a paper-white mask of evil.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A bag of groceries taken off the shelf before the expiration date.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A book: Feng Shui, Zen: the art of randomly arranging items that are not kitten.\x00"
        as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"This might be the fountain of youth, but you\'ll never know.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"Tigerbot Hesh.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"Stimutacs.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A canister of pressurized whipped cream, sans whipped cream.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The non-kitten item bites!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"A chain hanging from two posts reminds you of the Gateway Arch.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A mathematician calculates the halting probability of a Turing machine.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A number of short theatrical productions are indexed 1, 2, 3, ... n.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"A technical university in Australia.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It is -- I just feel something wonderful is about to happen.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"It\'s a Cat 5 cable.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a U.S. president.\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    b"It\'s a piece of cloth used to cover a stage in between performances.\x00" as *const u8
        as *const libc::c_char as *mut libc::c_char,
    b"The ionosphere seems charged with meaning.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"This tomography is like, hella axial, man!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s your favorite game -- robotfindscatan!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"Just a man selling an albatross.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"The intermission from a 1930s silent movie.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"It\'s an inverted billiard ball!\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
    b"The spectre of Sherlock Holmes wills you onwards.\x00" as *const u8 as *const libc::c_char
        as *mut libc::c_char,
];
/*
 *Function definitions
 */
/*Initialization and setup functions*/
#[no_mangle]
pub unsafe extern "C" fn initialize_ncurses() {
    signal(2i32, Some(finish));
    initscr();
    keypad(stdscr, 0 != 1i32);
    nonl();
    intrflush(stdscr, 0 != 0i32);
    noecho();
    cbreak();
    if has_colors() {
        start_color();
        init_pair(
            0i32 as libc::c_short,
            0i32 as libc::c_short,
            0i32 as libc::c_short,
        );
        init_pair(
            2i32 as libc::c_short,
            2i32 as libc::c_short,
            0i32 as libc::c_short,
        );
        init_pair(
            1i32 as libc::c_short,
            1i32 as libc::c_short,
            0i32 as libc::c_short,
        );
        init_pair(
            6i32 as libc::c_short,
            6i32 as libc::c_short,
            0i32 as libc::c_short,
        );
        init_pair(
            7i32 as libc::c_short,
            7i32 as libc::c_short,
            0i32 as libc::c_short,
        );
        init_pair(
            5i32 as libc::c_short,
            5i32 as libc::c_short,
            0i32 as libc::c_short,
        );
        init_pair(
            4i32 as libc::c_short,
            4i32 as libc::c_short,
            0i32 as libc::c_short,
        );
        init_pair(
            3i32 as libc::c_short,
            3i32 as libc::c_short,
            0i32 as libc::c_short,
        );
    };
}
unsafe extern "C" fn finish(mut sig: libc::c_int) {
    endwin();
    printf(
        b"%c%c%c\x00" as *const u8 as *const libc::c_char,
        27i32,
        '(' as i32,
        'B' as i32,
    );
    exit(0i32);
}
#[no_mangle]
pub unsafe extern "C" fn initialize_arrays() {
    let mut counter: libc::c_int = 0;
    let mut counter2: libc::c_int = 0;
    let mut empty: screen_object = screen_object {
        x: 0,
        y: 0,
        color: 0,
        bold: false,
        character: 0,
    };
    let mut i: libc::c_int = 0i32;
    screen = malloc(
        (::std::mem::size_of::<*mut libc::c_int>() as libc::c_ulong)
            .wrapping_mul((COLS - 1i32 + 1i32) as libc::c_ulong),
    ) as *mut *mut libc::c_int;
    i = 0i32;
    while i < COLS - 1i32 + 1i32 {
        let ref mut fresh0 = *screen.offset(i as isize);
        *fresh0 = malloc(
            (::std::mem::size_of::<libc::c_int>() as libc::c_ulong)
                .wrapping_mul((LINES - 1i32 + 1i32) as libc::c_ulong),
        ) as *mut libc::c_int;
        i += 1
    }
    empty.x = -1i32;
    empty.y = -1i32;
    empty.color = 0i32;
    empty.bold = 0 != 0i32;
    empty.character = ' ' as i32 as libc::c_char;
    counter = 0i32;
    while counter <= COLS - 1i32 {
        counter2 = 0i32;
        while counter2 <= LINES - 1i32 {
            *(*screen.offset(counter as isize)).offset(counter2 as isize) = -1i32;
            counter2 += 1
        }
        counter += 1
    }
    counter = 0i32;
    while (counter as libc::c_ulong)
        < (::std::mem::size_of::<[*mut libc::c_char; 406]>() as libc::c_ulong)
            .wrapping_div(::std::mem::size_of::<*mut libc::c_char>() as libc::c_ulong)
    {
        used_messages[counter as usize] = 0i32;
        bogus_messages[counter as usize] = 0i32;
        bogus[counter as usize] = empty;
        counter += 1
    }
}
#[no_mangle]
pub static mut bogus: [screen_object; 406] = [screen_object {
    x: 0,
    y: 0,
    color: 0,
    bold: false,
    character: 0,
}; 406];
#[no_mangle]
pub static mut bogus_messages: [libc::c_int; 406] = [0; 406];
#[no_mangle]
pub static mut used_messages: [libc::c_int; 406] = [0; 406];
/* This array contains our internal representation of the screen. The
array is bigger than it needs to be, as we don't need to keep track
of the first few rows of the screen. But that requires making an
offset function and using that everywhere. So not right now. */
#[no_mangle]
pub static mut screen: *mut *mut libc::c_int =
    0 as *const *mut libc::c_int as *mut *mut libc::c_int;
#[no_mangle]
pub unsafe extern "C" fn initialize_robot() {
    robot.x = rand() % (COLS - 1i32) + 1i32;
    robot.y = rand() % (LINES - 1i32 - 3i32 + 1i32) + 3i32;
    robot.character = '#' as i32 as libc::c_char;
    robot.color = 0i32;
    robot.bold = 0 != 0i32;
    *(*screen.offset(robot.x as isize)).offset(robot.y as isize) = 0i32;
}
/*Global variables. Bite me, it's fun.*/
#[no_mangle]
pub static mut robot: screen_object = screen_object {
    x: 0,
    y: 0,
    color: 0,
    bold: false,
    character: 0,
};
#[no_mangle]
pub unsafe extern "C" fn initialize_kitten() {
    loop {
        kitten.x = rand() % (COLS - 1i32) + 1i32;
        kitten.y = rand() % (LINES - 1i32 - 3i32 + 1i32) + 3i32;
        if !(*(*screen.offset(kitten.x as isize)).offset(kitten.y as isize) != -1i32) {
            break;
        }
    }
    loop {
        kitten.character = (rand() % (126i32 - '!' as i32 + 1i32) + '!' as i32) as libc::c_char;
        if !(0 == validchar(kitten.character)) {
            break;
        }
    }
    *(*screen.offset(kitten.x as isize)).offset(kitten.y as isize) = 1i32;
    kitten.color = rand() % 6i32 + 1i32;
    kitten.bold = 0 != if 0 != rand() % 2i32 { 1i32 } else { 0i32 };
}
#[no_mangle]
pub static mut kitten: screen_object = screen_object {
    x: 0,
    y: 0,
    color: 0,
    bold: false,
    character: 0,
};
/*Helper functions*/
#[no_mangle]
pub unsafe extern "C" fn validchar(mut a: libc::c_char) -> libc::c_int {
    match a as libc::c_int {
        35 | 32 | 127 => return 0i32,
        _ => {}
    }
    return 1i32;
}
#[no_mangle]
pub unsafe extern "C" fn initialize_bogus() {
    let mut counter: libc::c_int = 0;
    let mut index: libc::c_int = 0;
    counter = 0i32;
    while counter < num_bogus {
        bogus[counter as usize].color = rand() % 6i32 + 1i32;
        bogus[counter as usize].bold = 0 != if 0 != rand() % 2i32 { 1i32 } else { 0i32 };
        loop {
            bogus[counter as usize].character =
                (rand() % (126i32 - '!' as i32 + 1i32) + '!' as i32) as libc::c_char;
            if !(0 == validchar(bogus[counter as usize].character)) {
                break;
            }
        }
        loop {
            bogus[counter as usize].x = rand() % (COLS - 1i32) + 1i32;
            bogus[counter as usize].y = rand() % (LINES - 1i32 - 3i32 + 1i32) + 3i32;
            if !(*(*screen.offset(bogus[counter as usize].x as isize))
                .offset(bogus[counter as usize].y as isize)
                != -1i32)
            {
                break;
            }
        }
        *(*screen.offset(bogus[counter as usize].x as isize))
            .offset(bogus[counter as usize].y as isize) = counter + 2i32;
        loop {
            index = (rand() as libc::c_ulong).wrapping_rem(
                (::std::mem::size_of::<[*mut libc::c_char; 406]>() as libc::c_ulong)
                    .wrapping_div(::std::mem::size_of::<*mut libc::c_char>() as libc::c_ulong),
            ) as libc::c_int;
            if !(used_messages[index as usize] != 0i32) {
                break;
            }
        }
        bogus_messages[counter as usize] = index;
        used_messages[index as usize] = 1i32;
        counter += 1
    }
}
#[no_mangle]
pub static mut num_bogus: libc::c_int = 0;
#[no_mangle]
pub unsafe extern "C" fn initialize_screen() {
    let mut counter: libc::c_int = 0;
    mvprintw(
        0i32,
        0i32,
        b"robotfindskitten v%s\n\n\x00" as *const u8 as *const libc::c_char,
        ver,
    );
    counter = 0i32;
    while counter <= COLS - 1i32 {
        printw(b"%c\x00" as *const u8 as *const libc::c_char, 95i32);
        counter += 1
    }
    counter = 0i32;
    while counter < num_bogus {
        draw(bogus[counter as usize]);
        counter += 1
    }
    draw(kitten);
    draw(robot);
    wrefresh(stdscr);
}
#[no_mangle]
pub unsafe extern "C" fn draw(mut o: screen_object) {
    full_draw(o, 0 != 0i32);
}
#[no_mangle]
pub unsafe extern "C" fn full_draw(mut o: screen_object, mut in_place: bool) {
    let mut old: attr_t = 0;
    let mut dummy: libc::c_short = 0;
    let mut new: attr_t = 0;
    if !(stdscr as *const libc::c_void).is_null() {
        if !(&mut old as *mut attr_t as *const libc::c_void).is_null() {
            old = (*stdscr)._attrs
        } else {
        };
        if !(&mut dummy as *mut libc::c_short as *const libc::c_void).is_null() {
            dummy = (((*stdscr)._attrs as libc::c_ulong
                & ((1u32 << 8i32).wrapping_sub(1u32) << 0i32 + 8i32) as libc::c_ulong)
                >> 8i32) as libc::c_int as libc::c_short
        } else {
        };
    } else {
    };
    new = (o.color as chtype) << 0i32 + 8i32 & (1u32 << 8i32).wrapping_sub(1u32) << 0i32 + 8i32;
    if o.character as libc::c_int == '#' as i32 {
        new |= 1u32 << 12i32 + 8i32
    }
    if o.character as libc::c_int <= '\u{1a}' as i32 {
        new |= 1u32 << 14i32 + 8i32
    }
    if o.bold {
        new |= 1u32 << 13i32 + 8i32
    }
    if !(stdscr as *const libc::c_void).is_null() {
        (*stdscr)._attrs = new
    } else {
    };
    if in_place {
        printw(
            b"%c\x00" as *const u8 as *const libc::c_char,
            o.character as libc::c_int,
        );
    } else {
        mvprintw(
            o.y,
            o.x,
            b"%c\x00" as *const u8 as *const libc::c_char,
            o.character as libc::c_int,
        );
        wmove(stdscr, o.y, o.x);
    }
    if !(stdscr as *const libc::c_void).is_null() {
        (*stdscr)._attrs = old
    } else {
    };
}
#[no_mangle]
pub unsafe extern "C" fn instructions() {
    let mut dummy: libc::c_char = 0;
    mvprintw(
        0i32,
        0i32,
        b"robotfindskitten v%s\n\x00" as *const u8 as *const libc::c_char,
        ver,
    );
    printw(
        b"By the illustrious Leonard Richardson (C) 1997, 2000\n\x00" as *const u8
            as *const libc::c_char,
    );
    printw(
        b"Written originally for the Nerth Pork robotfindskitten contest\n\n\x00" as *const u8
            as *const libc::c_char,
    );
    printw(b"In this game, you are robot (\x00" as *const u8 as *const libc::c_char);
    draw_in_place(robot);
    printw(b"). Your job is to find kitten. This task\n\x00" as *const u8 as *const libc::c_char);
    printw(
        b"is complicated by the existence of various things which are not kitten.\n\x00"
            as *const u8 as *const libc::c_char,
    );
    printw(
        b"Robot must touch items to determine if they are kitten or not. The game\n\x00"
            as *const u8 as *const libc::c_char,
    );
    printw(
        b"ends when robotfindskitten. Alternatively, you may end the game by hitting\n\x00"
            as *const u8 as *const libc::c_char,
    );
    printw(
        b"the Esc key. See the documentation for more information.\n\n\x00" as *const u8
            as *const libc::c_char,
    );
    printw(b"Press any key to start.\n\x00" as *const u8 as *const libc::c_char);
    wrefresh(stdscr);
    dummy = wgetch(stdscr) as libc::c_char;
    wclear(stdscr);
}
#[no_mangle]
pub unsafe extern "C" fn draw_in_place(mut o: screen_object) {
    full_draw(o, 0 != 1i32);
}
/*Game functions*/
#[no_mangle]
pub unsafe extern "C" fn play_game() {
    let mut old_x: libc::c_int = robot.x;
    let mut old_y: libc::c_int = robot.y;
    let mut input: libc::c_int = 0;
    input = wgetch(stdscr);
    while input != 27i32 && input != 'q' as i32 && input != 'Q' as i32 {
        process_input(input);
        if !(old_x == robot.x && old_y == robot.y) {
            if wmove(stdscr, old_y, old_x) == -1i32 {
            } else {
                waddch(stdscr, ' ' as i32 as chtype);
            };
            *(*screen.offset(old_x as isize)).offset(old_y as isize) = -1i32;
            draw(robot);
            wrefresh(stdscr);
            *(*screen.offset(robot.x as isize)).offset(robot.y as isize) = 0i32;
            old_x = robot.x;
            old_y = robot.y
        }
        input = wgetch(stdscr)
    }
    message(b"Bye!\x00" as *const u8 as *const libc::c_char as *mut libc::c_char);
    wrefresh(stdscr);
    finish(0i32);
}
#[no_mangle]
pub unsafe extern "C" fn message(mut message_0: *mut libc::c_char) {
    wmove(stdscr, 1i32, 0i32);
    wclrtoeol(stdscr);
    mvprintw(
        1i32,
        0i32,
        b"%.*s\x00" as *const u8 as *const libc::c_char,
        COLS,
        message_0,
    );
    wmove(stdscr, robot.y, robot.x);
    wrefresh(stdscr);
}
#[no_mangle]
pub unsafe extern "C" fn process_input(mut input: libc::c_int) {
    let mut check_x: libc::c_int = robot.x;
    let mut check_y: libc::c_int = robot.y;
    match input {
        12 => {
            wrefresh(curscr);
        }
        259 | 107 | 75 | 16 => check_y -= 1,
        262 | 121 | 89 => {
            check_x -= 1;
            check_y -= 1
        }
        339 | 117 | 85 => {
            check_x += 1;
            check_y -= 1
        }
        258 | 106 | 74 | 14 => check_y += 1,
        360 | 98 | 66 => {
            check_x -= 1;
            check_y += 1
        }
        338 | 110 | 78 => {
            check_x += 1;
            check_y += 1
        }
        260 | 104 | 72 | 2 => check_x -= 1,
        261 | 108 | 76 | 6 => check_x += 1,
        0 => {}
        _ => {
            message(
                b"Invalid input: Use direction keys or Esc.\x00" as *const u8 as *const libc::c_char
                    as *mut libc::c_char,
            );
            return;
        }
    }
    if check_y < 3i32 || check_y > LINES - 1i32 || check_x < 0i32 || check_x > COLS - 1i32 {
        return;
    }
    if *(*screen.offset(check_x as isize)).offset(check_y as isize) != -1i32 {
        match *(*screen.offset(check_x as isize)).offset(check_y as isize) {
            0 => {}
            1 => {
                /*We didn't move, or we're stuck in a
                time warp or something.*/
                wmove(stdscr, 1i32, 0i32);
                wclrtoeol(stdscr);
                play_animation(input);
            }
            _ => {
                message(
                    messages[bogus_messages[(*(*screen.offset(check_x as isize))
                        .offset(check_y as isize)
                        - 2i32) as usize] as usize],
                );
            }
        }
        return;
    }
    robot.x = check_x;
    robot.y = check_y;
}
#[no_mangle]
pub unsafe extern "C" fn play_animation(mut input: libc::c_int) {
    let mut counter: libc::c_int = 0;
    counter = 4i32;
    while counter > 0i32 {
        if wmove(stdscr, 1i32, 50i32 + counter + 1i32) == -1i32 {
        } else {
            waddch(stdscr, ' ' as i32 as chtype);
        };
        wmove(stdscr, 1i32, 50i32 + counter);
        if input == 0o405i32 || input == 0o402i32 || input == 0o540i32 || input == 0o535i32 {
            draw_in_place(kitten);
        } else {
            draw_in_place(robot);
        }
        if wmove(stdscr, 1i32, 50i32 - counter) == -1i32 {
        } else {
            waddch(stdscr, ' ' as i32 as chtype);
        };
        wmove(stdscr, 1i32, 50i32 - counter + 1i32);
        if input == 0o405i32 || input == 0o402i32 || input == 0o540i32 || input == 0o535i32 {
            draw_in_place(robot);
        } else {
            draw_in_place(kitten);
        }
        wrefresh(stdscr);
        sleep(1i32 as libc::c_uint);
        counter -= 1
    }
    wmove(stdscr, 1i32, 0i32);
    waddnstr(
        stdscr,
        b"You found kitten! Way to go, robot!\x00" as *const u8 as *const libc::c_char,
        -1i32,
    );
    wrefresh(stdscr);
    finish(0i32);
}
unsafe fn main_0(mut argc: libc::c_int, mut argv: *mut *mut libc::c_char) -> libc::c_int {
    if argc == 1i32 {
        num_bogus = 20i32
    } else {
        num_bogus = atoi(*argv.offset(1isize));
        if num_bogus < 0i32
            || num_bogus as libc::c_ulong
                > (::std::mem::size_of::<[*mut libc::c_char; 406]>() as libc::c_ulong)
                    .wrapping_div(::std::mem::size_of::<*mut libc::c_char>() as libc::c_ulong)
        {
            printf(
                b"Run-time parameter must be between 0 and %d.\n\x00" as *const u8
                    as *const libc::c_char,
                (::std::mem::size_of::<[*mut libc::c_char; 406]>() as libc::c_ulong)
                    .wrapping_div(::std::mem::size_of::<*mut libc::c_char>() as libc::c_ulong),
            );
            exit(0i32);
        }
    }
    srand(time(0 as *mut time_t) as libc::c_uint);
    printf(
        b"%c%c%c\x00" as *const u8 as *const libc::c_char,
        27i32,
        '(' as i32,
        'U' as i32,
    );
    initialize_ncurses();
    initialize_arrays();
    initialize_robot();
    initialize_kitten();
    initialize_bogus();
    instructions();
    initialize_screen();
    play_game();
    return 0;
}
pub fn main() {
    let mut args: Vec<*mut libc::c_char> = Vec::new();
    for arg in ::std::env::args() {
        args.push(
            ::std::ffi::CString::new(arg)
                .expect("Failed to convert argument into CString.")
                .into_raw(),
        );
    }
    args.push(::std::ptr::null_mut());
    unsafe {
        ::std::process::exit(main_0(
            (args.len() - 1) as libc::c_int,
            args.as_mut_ptr() as *mut *mut libc::c_char,
        ) as i32)
    }
}
