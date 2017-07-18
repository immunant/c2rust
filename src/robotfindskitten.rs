#![feature(alloc_system, allocator_api, global_allocator)]
extern crate alloc_system;
#[global_allocator]
static ALLOC: alloc_system::System = alloc_system::System;


extern "C" {
    static mut COLS: i32;
    static mut LINES: i32;
    fn atoi(__nptr: *const u8) -> i32;
    fn cbreak() -> i32;
    static mut curscr: *mut _win_st;
    fn endwin() -> i32;
    fn exit(__status: i32);
    fn has_colors() -> bool;
    fn init_pair(arg1: i16, arg2: i16, arg3: i16) -> i32;
    fn initscr() -> *mut _win_st;
    fn intrflush(arg1: *mut _win_st, arg2: bool) -> i32;
    fn keypad(arg1: *mut _win_st, arg2: bool) -> i32;
    fn malloc(__size: usize) -> *mut ::std::os::raw::c_void;
    fn mvprintw(arg1: i32, arg2: i32, arg3: *const u8, ...) -> i32;
    fn noecho() -> i32;
    fn nonl() -> i32;
    fn printf(__format: *const u8, ...) -> i32;
    fn printw(arg1: *const u8, ...) -> i32;
    fn rand() -> i32;
    fn signal(__sig: i32, __handler: unsafe extern "C" fn(i32)) -> unsafe extern "C" fn(i32);
    fn sleep(__seconds: u32) -> u32;
    fn srand(__seed: u32);
    fn start_color() -> i32;
    static mut stdscr: *mut _win_st;
    fn time(__timer: *mut isize) -> isize;
    fn waddch(arg1: *mut _win_st, arg2: usize) -> i32;
    fn waddnstr(arg1: *mut _win_st, arg2: *const u8, arg3: i32) -> i32;
    fn wclear(arg1: *mut _win_st) -> i32;
    fn wclrtoeol(arg1: *mut _win_st) -> i32;
    fn wgetch(arg1: *mut _win_st) -> i32;
    fn wmove(arg1: *mut _win_st, arg2: i32, arg3: i32) -> i32;
    fn wrefresh(arg1: *mut _win_st) -> i32;
}

mod wrap {
    use super::_win_st;
    pub unsafe fn atoi(__nptr: *const u8) -> i32 {
        ::atoi(__nptr)
    }
    pub fn cbreak() -> i32 {
        unsafe { ::cbreak() }
    }
    pub fn endwin() -> i32 {
        unsafe { ::endwin() }
    }
    pub fn exit(__status: i32) {
        unsafe { ::exit(__status) }
    }
    pub fn has_colors() -> bool {
        unsafe { ::has_colors() }
    }
    pub fn init_pair(arg1: i16, arg2: i16, arg3: i16) -> i32 {
        unsafe { ::init_pair(arg1, arg2, arg3) }
    }
    pub fn initscr() -> *mut _win_st {
        unsafe { ::initscr() }
    }
    pub fn intrflush(arg2: bool) -> i32 {
        unsafe { ::intrflush(::stdscr, arg2) }
    }
    pub fn keypad(arg2: bool) -> i32 {
        unsafe { ::keypad(::stdscr, arg2) }
    }
    pub fn malloc(__size: usize) -> *mut ::std::os::raw::c_void {
        unsafe { ::malloc(__size) }
    }
    pub fn noecho() -> i32 {
        unsafe { ::noecho() }
    }
    pub fn nonl() -> i32 {
        unsafe { ::nonl() }
    }
    pub fn rand() -> i32 {
        unsafe { ::rand() }
    }
    pub unsafe fn signal(
        __sig: i32,
        __handler: unsafe extern "C" fn(i32),
    ) -> unsafe extern "C" fn(i32) {
        ::signal(__sig, __handler)
    }
    pub fn sleep(__seconds: u32) -> u32 {
        unsafe { ::sleep(__seconds) }
    }
    pub fn srand(__seed: u32) {
        unsafe { ::srand(__seed) }
    }
    pub fn start_color() -> i32 {
        unsafe { ::start_color() }
    }
    pub unsafe fn time(__timer: *mut isize) -> isize {
        ::time(__timer)
    }
    pub fn waddch(arg2: usize) -> i32 {
        unsafe { ::waddch(::stdscr, arg2) }
    }
    pub fn waddnstr(arg2: *const u8, arg3: i32) -> i32 {
        unsafe { ::waddnstr(::stdscr, arg2, arg3) }
    }
    pub fn wclear() -> i32 {
        unsafe { ::wclear(::stdscr) }
    }
    pub fn wclrtoeol() -> i32 {
        unsafe { ::wclrtoeol(::stdscr) }
    }
    pub fn wgetch() -> i32 {
        unsafe { ::wgetch(::stdscr) }
    }
    pub fn wmove(arg2: i32, arg3: i32) -> i32 {
        unsafe { ::wmove(::stdscr, arg2, arg3) }
    }
    pub fn wrefresh() -> i32 {
        unsafe { ::wrefresh(::stdscr) }
    }
    pub fn wrefresh_curscr() -> i32 {
        unsafe { ::wrefresh(::curscr) }
    }
}

pub enum ldat {
}

static mut ver: *mut u8 = 0 as *mut u8;

fn _init_ver() {
    unsafe {
        ver = (*b"1.7320508.406\0").as_ptr() as (*mut u8);
    }
}

static mut messages: [*mut u8; 406] = [0 as *mut u8; 406];

fn _init_messages() {
    unsafe {
        messages
    = [   (*b"\"I pity the fool who mistakes me for kitten!\", sez Mr. T.\0").as_ptr(
          ) as (*mut u8),
          (*b"That\'s just an old tin can.\0").as_ptr() as (*mut u8),
          (*b"It\'s an altar to the horse god.\0").as_ptr() as (*mut u8),
          (*b"A box of dancing mechanical pencils. They dance! They sing!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s an old Duke Ellington record.\0").as_ptr() as (*mut u8),
          (*b"A box of fumigation pellets.\0").as_ptr() as (*mut u8),
          (*b"A digital clock. It\'s stuck at 2:17 PM.\0").as_ptr(
          ) as (*mut u8),
          (*b"That\'s just a charred human corpse.\0").as_ptr() as (*mut u8),
          (*b"I don\'t know what that is, but it\'s not kitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"An empty shopping bag. Paper or plastic?\0").as_ptr(
          ) as (*mut u8),
          (*b"Could it be... a big ugly bowling trophy?\0").as_ptr(
          ) as (*mut u8),
          (*b"A coat hanger hovers in thin air. Odd.\0").as_ptr(
          ) as (*mut u8),
          (*b"Not kitten, just a packet of Kool-Aid(tm).\0").as_ptr(
          ) as (*mut u8),
          (*b"A freshly-baked pumpkin pie.\0").as_ptr() as (*mut u8),
          (*b"A lone, forgotten comma, sits here, sobbing.\0").as_ptr(
          ) as (*mut u8),
          (*b"ONE HUNDRED THOUSAND CARPET FIBERS!!!!!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s Richard Nixon\'s nose!\0").as_ptr() as (*mut u8),
          (*b"It\'s Lucy Ricardo. \"Aaaah, Ricky!\", she says.\0").as_ptr(
          ) as (*mut u8),
          (*b"You stumble upon Bill Gates\' stand-up act.\0").as_ptr(
          ) as (*mut u8),
          (*b"Just an autographed copy of the Kama Sutra.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the Will Rogers Highway. Who was Will Rogers, anyway?\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s another robot, more advanced in design than you but strangely immobile.\0").as_ptr(
          ) as (*mut u8),
          (*b"Leonard Richardson is here, asking people to lick him.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a stupid mask, fashioned after a beagle.\0").as_ptr(
          ) as (*mut u8),
          (*b"Your State Farm Insurance(tm) representative!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the local draft board.\0").as_ptr() as (*mut u8),
          (*b"Seven 1/4\" screws and a piece of plastic.\0").as_ptr(
          ) as (*mut u8),
          (*b"An 80286 machine.\0").as_ptr() as (*mut u8),
          (*b"One of those stupid \"Homes of the Stars\" maps.\0").as_ptr(
          ) as (*mut u8),
          (*b"A signpost saying \"TO KITTEN\". It points in no particular direction.\0").as_ptr(
          ) as (*mut u8),
          (*b"A hammock stretched between a tree and a volleyball pole.\0").as_ptr(
          ) as (*mut u8),
          (*b"A Texas Instruments of Destruction calculator.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a dark, amphorous blob of matter.\0").as_ptr(
          ) as (*mut u8),
          (*b"Just a pincushion.\0").as_ptr() as (*mut u8),
          (*b"It\'s a mighty zombie talking about some love and prosperity.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Dear robot, you may have already won our 10 MILLION DOLLAR prize...\"\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s just an object.\0").as_ptr() as (*mut u8),
          (*b"A mere collection of pixels.\0").as_ptr() as (*mut u8),
          (*b"A badly dented high-hat cymbal lies on its side here.\0").as_ptr(
          ) as (*mut u8),
          (*b"A marijuana brownie.\0").as_ptr() as (*mut u8),
          (*b"A plush Chewbacca.\0").as_ptr() as (*mut u8),
          (*b"Daily hunger conditioner from Australasia\0").as_ptr(
          ) as (*mut u8),
          (*b"Just some stuff.\0").as_ptr() as (*mut u8),
          (*b"Why are you touching this when you should be finding kitten?\0").as_ptr(
          ) as (*mut u8),
          (*b"A glorious fan of peacock feathers.\0").as_ptr() as (*mut u8),
          (*b"It\'s some compromising photos of Babar the Elephant.\0").as_ptr(
          ) as (*mut u8),
          (*b"A copy of the Weekly World News. Watch out for the chambered nautilus!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the proverbial wet blanket.\0").as_ptr() as (*mut u8),
          (*b"A \"Get Out of Jail Free\" card.\0").as_ptr() as (*mut u8),
          (*b"An incredibly expensive \"Mad About You\" collector plate.\0").as_ptr(
          ) as (*mut u8),
          (*b"Paul Moyer\'s necktie.\0").as_ptr() as (*mut u8),
          (*b"A haircut and a real job. Now you know where to get one!\0").as_ptr(
          ) as (*mut u8),
          (*b"An automated robot-hater. It frowns disapprovingly at you.\0").as_ptr(
          ) as (*mut u8),
          (*b"An automated robot-liker. It smiles at you.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a black hole. Don\'t fall in!\0").as_ptr() as (*mut u8),
          (*b"Just a big brick wall.\0").as_ptr() as (*mut u8),
          (*b"You found kitten! No, just kidding.\0").as_ptr() as (*mut u8),
          (*b"Heart of Darkness brand pistachio nuts.\0").as_ptr(
          ) as (*mut u8),
          (*b"A smoking branding iron shaped like a 24-pin connector.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a Java applet.\0").as_ptr() as (*mut u8),
          (*b"An abandoned used-car lot.\0").as_ptr() as (*mut u8),
          (*b"A shameless plug for Crummy: http://www.crummy.com/\0").as_ptr(
          ) as (*mut u8),
          (*b"A shameless plug for the UCLA Linux Users Group: http://linux.ucla.edu/\0").as_ptr(
          ) as (*mut u8),
          (*b"A can of Spam Lite.\0").as_ptr() as (*mut u8),
          (*b"This is another fine mess you\'ve gotten us into, Stanley.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s scenery for \"Waiting for Godot\".\0").as_ptr(
          ) as (*mut u8),
          (*b"This grain elevator towers high above you.\0").as_ptr(
          ) as (*mut u8),
          (*b"A Mentos wrapper.\0").as_ptr() as (*mut u8),
          (*b"It\'s the constellation Pisces.\0").as_ptr() as (*mut u8),
          (*b"It\'s a fly on the wall. Hi, fly!\0").as_ptr() as (*mut u8),
          (*b"This kind of looks like kitten, but it\'s not.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a banana! Oh, joy!\0").as_ptr() as (*mut u8),
          (*b"A helicopter has crashed here.\0").as_ptr() as (*mut u8),
          (*b"Carlos Tarango stands here, doing his best impression of Pat Smear.\0").as_ptr(
          ) as (*mut u8),
          (*b"A patch of mushrooms grows here.\0").as_ptr() as (*mut u8),
          (*b"A patch of grape jelly grows here.\0").as_ptr() as (*mut u8),
          (*b"A spindle, and a grindle, and a bucka-wacka-woom!\0").as_ptr(
          ) as (*mut u8),
          (*b"A geyser sprays water high into the air.\0").as_ptr(
          ) as (*mut u8),
          (*b"A toenail? What good is a toenail?\0").as_ptr() as (*mut u8),
          (*b"You\'ve found the fish! Not that it does you much good in this game.\0").as_ptr(
          ) as (*mut u8),
          (*b"A Buttertonsils bar.\0").as_ptr() as (*mut u8),
          (*b"One of the few remaining discoes.\0").as_ptr() as (*mut u8),
          (*b"Ah, the uniform of a Revolutionary-era minuteman.\0").as_ptr(
          ) as (*mut u8),
          (*b"A punch bowl, filled with punch and lemon slices.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s nothing but a G-thang, baby.\0").as_ptr() as (*mut u8),
          (*b"IT\'S ALIVE! AH HA HA HA HA!\0").as_ptr() as (*mut u8),
          (*b"This was no boating accident!\0").as_ptr() as (*mut u8),
          (*b"Wait! This isn\'t the poker chip! You\'ve been tricked! DAMN YOU, MENDEZ!\0").as_ptr(
          ) as (*mut u8),
          (*b"A livery stable! Get your livery!\0").as_ptr() as (*mut u8),
          (*b"It\'s a perpetual immobility machine.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"On this spot in 1962, Henry Winkler was sick.\"\0").as_ptr(
          ) as (*mut u8),
          (*b"There\'s nothing here; it\'s just an optical illusion.\0").as_ptr(
          ) as (*mut u8),
          (*b"The World\'s Biggest Motzah Ball!\0").as_ptr() as (*mut u8),
          (*b"A tribe of cannibals lives here. They eat Malt-O-Meal for breakfast, you know.\0").as_ptr(
          ) as (*mut u8),
          (*b"This appears to be a rather large stack of trashy romance novels.\0").as_ptr(
          ) as (*mut u8),
          (*b"Look out! Exclamation points!\0").as_ptr() as (*mut u8),
          (*b"A herd of wild coffee mugs slumbers here.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a limbo bar! How low can you go?\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the horizon. Now THAT\'S weird.\0").as_ptr(
          ) as (*mut u8),
          (*b"A vase full of artificial flowers is stuck to the floor here.\0").as_ptr(
          ) as (*mut u8),
          (*b"A large snake bars your way.\0").as_ptr() as (*mut u8),
          (*b"A pair of saloon-style doors swing slowly back and forth here.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s an ordinary bust of Beethoven... but why is it painted green?\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s TV\'s lovable wisecracking Crow! \"Bite me!\", he says.\0").as_ptr(
          ) as (*mut u8),
          (*b"Hey, look, it\'s war. What is it good for? Absolutely nothing. Say it again.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the amazing self-referential thing that\'s not kitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"A flamboyant feather boa. Now you can dress up like Carol Channing!\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Sure hope we get some rain soon,\" says Farmer Joe.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"How in heck can I wash my neck if it ain\'t gonna rain no more?\" asks Farmer Al.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Topsoil\'s all gone, ma,\" weeps Lil\' Greg.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is a large brown bear. Oddly enough, it\'s currently peeing in the woods.\0").as_ptr(
          ) as (*mut u8),
          (*b"A team of arctic explorers is camped here.\0").as_ptr(
          ) as (*mut u8),
          (*b"This object here appears to be Louis Farrakhan\'s bow tie.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is the world-famous Chain of Jockstraps.\0").as_ptr(
          ) as (*mut u8),
          (*b"A trash compactor, compacting away.\0").as_ptr() as (*mut u8),
          (*b"This toaster strudel is riddled with bullet holes!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a hologram of a crashed helicopter.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is a television. On screen you see a robot strangely similar to yourself.\0").as_ptr(
          ) as (*mut u8),
          (*b"This balogna has a first name, it\'s R-A-N-C-I-D.\0").as_ptr(
          ) as (*mut u8),
          (*b"A salmon hatchery? Look again. It\'s merely a single salmon.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a rim shot. Ba-da-boom!\0").as_ptr() as (*mut u8),
          (*b"It\'s creepy and it\'s kooky, mysterious and spooky. It\'s also somewhat ooky.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is an anagram.\0").as_ptr() as (*mut u8),
          (*b"This object is like an analogy.\0").as_ptr() as (*mut u8),
          (*b"It\'s a symbol. You see in it a model for all symbols everywhere.\0").as_ptr(
          ) as (*mut u8),
          (*b"The object pushes back at you.\0").as_ptr() as (*mut u8),
          (*b"A traffic signal. It appears to have been recently vandalized.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"There is no kitten!\" cackles the old crone. You are shocked by her blasphemy.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is a Lagrange point. Don\'t come too close now.\0").as_ptr(
          ) as (*mut u8),
          (*b"The dirty old tramp bemoans the loss of his harmonica.\0").as_ptr(
          ) as (*mut u8),
          (*b"Look, it\'s Fanny the Irishman!\0").as_ptr() as (*mut u8),
          (*b"What in blazes is this?\0").as_ptr() as (*mut u8),
          (*b"It\'s the instruction manual for a previous version of this game.\0").as_ptr(
          ) as (*mut u8),
          (*b"A brain cell. Oddly enough, it seems to be functioning.\0").as_ptr(
          ) as (*mut u8),
          (*b"Tea and/or crumpets.\0").as_ptr() as (*mut u8),
          (*b"This jukebox has nothing but Cliff Richards albums in it.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a Quaker Oatmeal tube, converted into a drum.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is a remote control. Being a robot, you keep a wide berth.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a roll of industrial-strength copper wire.\0").as_ptr(
          ) as (*mut u8),
          (*b"Oh boy! Grub! Er, grubs.\0").as_ptr() as (*mut u8),
          (*b"A puddle of mud, where the mudskippers play.\0").as_ptr(
          ) as (*mut u8),
          (*b"Plenty of nothing.\0").as_ptr() as (*mut u8),
          (*b"Look at that, it\'s the Crudmobile.\0").as_ptr() as (*mut u8),
          (*b"Just Walter Mattheau and Jack Lemmon.\0").as_ptr(
          ) as (*mut u8),
          (*b"Two crepes, two crepes in a box.\0").as_ptr() as (*mut u8),
          (*b"An autographed copy of \"Primary Colors\", by Anonymous.\0").as_ptr(
          ) as (*mut u8),
          (*b"Another rabbit? That\'s three today!\0").as_ptr() as (*mut u8),
          (*b"It\'s a segmentation fault. Core dumped, by the way.\0").as_ptr(
          ) as (*mut u8),
          (*b"A historical marker showing the actual location of /dev/null.\0").as_ptr(
          ) as (*mut u8),
          (*b"Thar\'s Mobius Dick, the convoluted whale. Arrr!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a charcoal briquette, smoking away.\0").as_ptr(
          ) as (*mut u8),
          (*b"A pizza, melting in the sun.\0").as_ptr() as (*mut u8),
          (*b"It\'s a \"HOME ALONE 2: Lost in New York\" novelty cup.\0").as_ptr(
          ) as (*mut u8),
          (*b"A stack of 7 inch floppies wobbles precariously.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s nothing but a corrupted floppy. Coaster anyone?\0").as_ptr(
          ) as (*mut u8),
          (*b"A section of glowing phosphor cells sings a song of radiation to you.\0").as_ptr(
          ) as (*mut u8),
          (*b"This TRS-80 III is eerily silent.\0").as_ptr() as (*mut u8),
          (*b"A toilet bowl occupies this space.\0").as_ptr() as (*mut u8),
          (*b"This peg-leg is stuck in a knothole!\0").as_ptr() as (*mut u8),
          (*b"It\'s a solitary vacuum tube.\0").as_ptr() as (*mut u8),
          (*b"This corroded robot is clutching a mitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Hi, I\'m Anson Williams, TV\'s \'Potsy\'.\"\0").as_ptr(
          ) as (*mut u8),
          (*b"This subwoofer was blown out in 1974.\0").as_ptr(
          ) as (*mut u8),
          (*b"Three half-pennies and a wooden nickel.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the missing chapter to \"A Clockwork Orange\".\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a burrito stand flyer. \"Taqueria El Ranchito\".\0").as_ptr(
          ) as (*mut u8),
          (*b"This smiling family is happy because they eat LARD.\0").as_ptr(
          ) as (*mut u8),
          (*b"Roger Avery, persona un famoso de los Estados Unidos.\0").as_ptr(
          ) as (*mut u8),
          (*b"Ne\'er but a potted plant.\0").as_ptr() as (*mut u8),
          (*b"A parrot, kipping on its back.\0").as_ptr() as (*mut u8),
          (*b"A forgotten telephone switchboard.\0").as_ptr() as (*mut u8),
          (*b"A forgotten telephone switchboard operator.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s an automated robot-disdainer. It pretends you\'re not there.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a portable hole. A sign reads: \"Closed for the winter\".\0").as_ptr(
          ) as (*mut u8),
          (*b"Just a moldy loaf of bread.\0").as_ptr() as (*mut u8),
          (*b"A little glass tub of Carmex. ($.89) Too bad you have no lips.\0").as_ptr(
          ) as (*mut u8),
          (*b"A Swiss-Army knife. All of its appendages are out. (toothpick lost)\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a zen simulation, trapped within an ASCII character.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a copy of \"The Rubaiyat of Spike Schudy\".\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s \"War and Peace\" (unabridged, very small print).\0").as_ptr(
          ) as (*mut u8),
          (*b"A willing, ripe tomato bemoans your inability to digest fruit.\0").as_ptr(
          ) as (*mut u8),
          (*b"A robot comedian. You feel amused.\0").as_ptr() as (*mut u8),
          (*b"It\'s KITT, the talking car.\0").as_ptr() as (*mut u8),
          (*b"Here\'s Pete Peterson. His batteries seem to have long gone dead.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Blup, blup, blup\", says the mud pot.\0").as_ptr(
          ) as (*mut u8),
          (*b"More grist for the mill.\0").as_ptr() as (*mut u8),
          (*b"Grind \'em up, spit \'em out, they\'re twigs.\0").as_ptr(
          ) as (*mut u8),
          (*b"The boom box cranks out an old Ethel Merman tune.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s \"Finding kitten\", published by O\'Reilly and Associates.\0").as_ptr(
          ) as (*mut u8),
          (*b"Pumpkin pie spice.\0").as_ptr() as (*mut u8),
          (*b"It\'s the Bass-Matic \'76! Mmm, that\'s good bass!\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Lend us a fiver \'til Thursday\", pleas Andy Capp.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a tape of \'70s rock. All original hits! All original artists!\0").as_ptr(
          ) as (*mut u8),
          (*b"You\'ve found the fabled America Online disk graveyard!\0").as_ptr(
          ) as (*mut u8),
          (*b"Empty jewelboxes litter the landscape.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the astounding meta-object.\0").as_ptr() as (*mut u8),
          (*b"Ed McMahon stands here, lost in thought. Seeing you, he bellows, \"YES SIR!\"\0").as_ptr(
          ) as (*mut u8),
          (*b"...thingy???\0").as_ptr() as (*mut u8),
          (*b"It\'s 1000 secrets the government doesn\'t want you to know!\0").as_ptr(
          ) as (*mut u8),
          (*b"The letters O and R.\0").as_ptr() as (*mut u8),
          (*b"A magical... magic thing.\0").as_ptr() as (*mut u8),
          (*b"It\'s a moment of silence.\0").as_ptr() as (*mut u8),
          (*b"It\'s Sirhan-Sirhan, looking guilty.\0").as_ptr() as (*mut u8),
          (*b"It\'s \"Chicken Soup for the Kitten-seeking Soulless Robot.\"\0").as_ptr(
          ) as (*mut u8),
          (*b"It is a set of wind-up chatter teeth.\0").as_ptr(
          ) as (*mut u8),
          (*b"It is a cloud shaped like an ox.\0").as_ptr() as (*mut u8),
          (*b"You see a snowflake here, melting slowly.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a big block of ice. Something seems to be frozen inside it.\0").as_ptr(
          ) as (*mut u8),
          (*b"Vladimir Lenin\'s casket rests here.\0").as_ptr() as (*mut u8),
          (*b"It\'s a copy of \"Zen and The Art of Robot Maintenance\".\0").as_ptr(
          ) as (*mut u8),
          (*b"This invisible box contains a pantomime horse.\0").as_ptr(
          ) as (*mut u8),
          (*b"A mason jar lies here open. It\'s label reads: \"do not open!\".\0").as_ptr(
          ) as (*mut u8),
          (*b"A train of thought chugs through here.\0").as_ptr(
          ) as (*mut u8),
          (*b"This jar of pickles expired in 1957.\0").as_ptr() as (*mut u8),
          (*b"Someone\'s identity disk lies here.\0").as_ptr() as (*mut u8),
          (*b"\"Yes!\" says the bit.\0").as_ptr() as (*mut u8),
          (*b"\"No!\" says the bit.\0").as_ptr() as (*mut u8),
          (*b"A dodecahedron bars your way.\0").as_ptr() as (*mut u8),
          (*b"Mr. Hooper is here, surfing.\0").as_ptr() as (*mut u8),
          (*b"It\'s a big smoking fish.\0").as_ptr() as (*mut u8),
          (*b"You have new mail in /var/spool/robot\0").as_ptr(
          ) as (*mut u8),
          (*b"Just a monitor with the blue element burnt out.\0").as_ptr(
          ) as (*mut u8),
          (*b"A pile of coaxial plumbing lies here.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a rotten old shoe.\0").as_ptr() as (*mut u8),
          (*b"It\'s a hundred-dollar bill.\0").as_ptr() as (*mut u8),
          (*b"It\'s a Dvorak keyboard.\0").as_ptr() as (*mut u8),
          (*b"It\'s a cardboard box full of 8-tracks.\0").as_ptr(
          ) as (*mut u8),
          (*b"Just a broken hard drive containg the archives of Nerth Pork.\0").as_ptr(
          ) as (*mut u8),
          (*b"A broken metronome sits here, it\'s needle off to one side.\0").as_ptr(
          ) as (*mut u8),
          (*b"A sign reads: \"Go home!\"\0").as_ptr() as (*mut u8),
          (*b"A sign reads: \"No robots allowed!\"\0").as_ptr() as (*mut u8),
          (*b"It\'s the handheld robotfindskitten game, by Tiger.\0").as_ptr(
          ) as (*mut u8),
          (*b"This particular monstrosity appears to be ENIAC.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is a tasty-looking banana creme pie.\0").as_ptr(
          ) as (*mut u8),
          (*b"A wireframe model of a hot dog rotates in space here.\0").as_ptr(
          ) as (*mut u8),
          (*b"Just the empty husk of a locust.\0").as_ptr() as (*mut u8),
          (*b"You disturb a murder of crows.\0").as_ptr() as (*mut u8),
          (*b"It\'s a copy of the robotfindskitten EULA.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s Death.\0").as_ptr() as (*mut u8),
          (*b"It\'s an autographed copy of \"Secondary Colors,\" by Bob Ross.\0").as_ptr(
          ) as (*mut u8),
          (*b"It is a marzipan dreadnought that appears to have melted and stuck.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a DVD of \"Crouching Monkey, Hidden Kitten\", region encoded for the moon.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s Kieran Hervold.  Damn dyslexia!\0").as_ptr(
          ) as (*mut u8),
          (*b"A non-descript box of crackers.\0").as_ptr() as (*mut u8),
          (*b"Carbonated Water, High Fructose Corn Syrup, Color, Phosphoric Acid, Flavors, Caffeine.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Move along! Nothing to see here!\"\0").as_ptr() as (*mut u8),
          (*b"It\'s the embalmed corpse of Vladimir Lenin.\0").as_ptr(
          ) as (*mut u8),
          (*b"A coupon for one free steak-fish at your local family diner.\0").as_ptr(
          ) as (*mut u8),
          (*b"A set of keys to a 2001 Rolls Royce. Worthless.\0").as_ptr(
          ) as (*mut u8),
          (*b"A gravestone stands here.  \"Izchak Miller, ascended.\"\0").as_ptr(
          ) as (*mut u8),
          (*b"Someone has written \"ad aerarium\" on the ground here.\0").as_ptr(
          ) as (*mut u8),
          (*b"A large blue eye floats in midair.\0").as_ptr() as (*mut u8),
          (*b"This appears to be a statue of Perseus.\0").as_ptr(
          ) as (*mut u8),
          (*b"There is an opulent throne here.\0").as_ptr() as (*mut u8),
          (*b"It\'s a squad of Keystone Kops.\0").as_ptr() as (*mut u8),
          (*b"This seems to be junk mail addressed to the finder of the Eye of Larn.\0").as_ptr(
          ) as (*mut u8),
          (*b"A wondrous and intricate golden amulet.  Too bad you have no neck.\0").as_ptr(
          ) as (*mut u8),
          (*b"The swampy ground around you seems to stink with disease.\0").as_ptr(
          ) as (*mut u8),
          (*b"An animate blob of acid.  Being metallic, you keep well away.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a copy of Knuth with the chapter on kitten-search algorithms torn out.\0").as_ptr(
          ) as (*mut u8),
          (*b"A crowd of people, and at the center, a popular misconception.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a blind man. When you touch, he exclaims \"It\'s a kitten prospecting robot!\"\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a lost wallet. It\'s owner didn\'t have pets, so you discard it.\0").as_ptr(
          ) as (*mut u8),
          (*b"This place is called Antarctica. There is no kitten here.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a mousetrap, baited with soap.\0").as_ptr() as (*mut u8),
          (*b"A book with \"Don\'t Panic\" in large friendly letters across the cover.\0").as_ptr(
          ) as (*mut u8),
          (*b"A compendium of haiku about metals.\0").as_ptr() as (*mut u8),
          (*b"A discredited cosmology, relic of a bygone era.\0").as_ptr(
          ) as (*mut u8),
          (*b"A hollow voice says \"Plugh\".\0").as_ptr() as (*mut u8),
          (*b"A knight who says \"Either I am an insane knave, or you will find kitten.\"\0").as_ptr(
          ) as (*mut u8),
          (*b"A neural net -- maybe it\'s trying to recognize kitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"A screwdriver.\0").as_ptr() as (*mut u8),
          (*b"A statue of a girl holding a goose like the one in Gottingen, Germany.\0").as_ptr(
          ) as (*mut u8),
          (*b"A tetradrachm dated \"42 B.C.\"\0").as_ptr() as (*mut u8),
          (*b"A voice booms out \"Onward, kitten soldiers...\"\0").as_ptr(
          ) as (*mut u8),
          (*b"An eminently forgettable zahir.\0").as_ptr() as (*mut u8),
          (*b"Apparently, it\'s Edmund Burke.\0").as_ptr() as (*mut u8),
          (*b"For a moment, you feel something in your hands, but it disappears!\0").as_ptr(
          ) as (*mut u8),
          (*b"Here is a book about Robert Kennedy.\0").as_ptr() as (*mut u8),
          (*b"Hey, robot, leave those lists alone.\0").as_ptr() as (*mut u8),
          (*b"Ho hum.  Another synthetic a posteriori.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s Asimov\'s Laws of Robotics.  You feel a strange affinity for them.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s Bach\'s Mass in B-minor!\0").as_ptr() as (*mut u8),
          (*b"It\'s a bug.\0").as_ptr() as (*mut u8),
          (*b"It\'s a synthetic a priori truth!  Immanuel would be so pleased!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the Tiki Room.\0").as_ptr() as (*mut u8),
          (*b"Just some old play by a Czech playwright, and you can\'t read Czech.\0").as_ptr(
          ) as (*mut u8),
          (*b"Kitten is the letter \'Q\'.  Oh, wait, maybe not.\0").as_ptr(
          ) as (*mut u8),
          (*b"Quidquid Latine dictum sit, kitten non est.\0").as_ptr(
          ) as (*mut u8),
          (*b"Sutro Tower is visible at some distance through the fog.\0").as_ptr(
          ) as (*mut u8),
          (*b"The Digital Millennium Copyright Act of 1998.\0").as_ptr(
          ) as (*mut u8),
          (*b"The United States Court of Appeals for the Federal Circuit.\0").as_ptr(
          ) as (*mut u8),
          (*b"The non-kitten item like this but with \"false\" and \"true\" switched is true.\0").as_ptr(
          ) as (*mut u8),
          (*b"The non-kitten item like this but with \"true\" and \"false\" switched is false.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is the chapter called \"A Map of the Cat?\" from Feynman\'s autobiography.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is the forest primeval.\0").as_ptr() as (*mut u8),
          (*b"Werner\'s \"Pocket Field Guide to Things That Are Not Kitten\".\0").as_ptr(
          ) as (*mut u8),
          (*b"You found nettik, but that\'s backwards.\0").as_ptr(
          ) as (*mut u8),
          (*b"You have found some zinc, but you must not stop here, for you must find kitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"50 Years Among the Non-Kitten Items\", by Ann Droyd.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Robot may not injure kitten, or, through inaction, ...\"\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Address Allocation for Private Internets\" by Yakov Rekhter et al.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"Mail Routing and the Domain System\" by Craig Partridge.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"The Theory and Practice of Oligarchical Collectivism\" by Emmanuel Goldstein.\0").as_ptr(
          ) as (*mut u8),
          (*b"\"201 Kitten Verbs, Fully Conjugated\".  You look for \"find\".\0").as_ptr(
          ) as (*mut u8),
          (*b"A card shark sits here, practicing his Faro shuffle.  He ignores you.\0").as_ptr(
          ) as (*mut u8),
          (*b"A copy of DeCSS.  They\'re a dime a dozen these days.\0").as_ptr(
          ) as (*mut u8),
          (*b"A demonic voice proclaims \"There is no kitten, only Zuul\".  You flee.\0").as_ptr(
          ) as (*mut u8),
          (*b"A lotus.  You make an interesting pair.\0").as_ptr(
          ) as (*mut u8),
          (*b"A milk carton, with a black and white picture of kitten on the side.\0").as_ptr(
          ) as (*mut u8),
          (*b"Any ordinary robot could see from a mile away that this wasn\'t kitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"A stegosaurus, escaped from the stegosaurusfindsrobot game.  It finds you.\0").as_ptr(
          ) as (*mut u8),
          (*b"Baling wire and chewing gum.\0").as_ptr() as (*mut u8),
          (*b"Chewing gum and baling wire.\0").as_ptr() as (*mut u8),
          (*b"Here is no kitten but only rock, rock and no kitten and the sandy road.\0").as_ptr(
          ) as (*mut u8),
          (*b"Hey, I bet you thought this was kitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"It is an ancient mariner, and he stoppeth one of three.\0").as_ptr(
          ) as (*mut u8),
          (*b"It pleases you to be kind to what appears to be kitten -- but it\'s not!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a blatant plug for Ogg Vorbis, http://www.vorbis.com/\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a business plan for a new startup, kitten.net.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a revised business plan for a new startup, my.kitten.net.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a square.\0").as_ptr() as (*mut u8),
          (*b"It seems to be a copy of \"A Tail of Two Kitties\".\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the Donation of Constantine!\0").as_ptr() as (*mut u8),
          (*b"It\'s this message, nothing more.\0").as_ptr() as (*mut u8),
          (*b"Lysine, an essential amino acid.  Well, maybe not for robots.\0").as_ptr(
          ) as (*mut u8),
          (*b"No kitten here.\0").as_ptr() as (*mut u8),
          (*b"The score for a Czech composer\'s \"Kitten-Finding Symphony in C\".\0").as_ptr(
          ) as (*mut u8),
          (*b"This looks like Bradley\'s \"Appearance and Reality\", but it\'s really not.\0").as_ptr(
          ) as (*mut u8),
          (*b"This non-kitten item no verb.\0").as_ptr() as (*mut u8),
          (*b"You feel strangely unfulfilled.\0").as_ptr() as (*mut u8),
          (*b"You hit the non-kitten item.  The non-kitten item fails to yowl.\0").as_ptr(
          ) as (*mut u8),
          (*b"You suddenly yearn for your distant homeland.\0").as_ptr(
          ) as (*mut u8),
          (*b"You\'ve found the snows of yesteryear!  So that\'s where they all went to.\0").as_ptr(
          ) as (*mut u8),
          (*b"Approaching.  One car.  J.  Followed by.  Two car.  M, M.  In five. Minutes.\0").as_ptr(
          ) as (*mut u8),
          (*b"Free Jon Johansen!\0").as_ptr() as (*mut u8),
          (*b"Free Dmitry Sklyarov!\0").as_ptr() as (*mut u8),
          (*b"One person shouts \"What do we want?\" The crowd answers \"Free Dmitry!\"\0").as_ptr(
          ) as (*mut u8),
          (*b"Judith Platt insults librarians.\0").as_ptr() as (*mut u8),
          (*b"This map is not the territory.\0").as_ptr() as (*mut u8),
          (*b"\"Go back to Libraria!\", says Pat Schroeder.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is a porcelain kitten-counter.  0, 0, 0, 0, 0...\0").as_ptr(
          ) as (*mut u8),
          (*b"An old bootable business card, unfortunately cracked down the middle.\0").as_ptr(
          ) as (*mut u8),
          (*b"A kitten sink, for washing kitten (if only kitten liked water).\0").as_ptr(
          ) as (*mut u8),
          (*b"A kitten source (to match the kitten sink).\0").as_ptr(
          ) as (*mut u8),
          (*b"If it\'s one thing, it\'s not another.\0").as_ptr(
          ) as (*mut u8),
          (*b"If it\'s not one thing, it\'s another.\0").as_ptr(
          ) as (*mut u8),
          (*b"A caboodle.\0").as_ptr() as (*mut u8),
          (*b"A grin.\0").as_ptr() as (*mut u8),
          (*b"A hedgehog.  It looks like it knows something important.\0").as_ptr(
          ) as (*mut u8),
          (*b"You\'ve found... Oh wait, that\'s just a cat.\0").as_ptr(
          ) as (*mut u8),
          (*b"Robot should not be touching that.\0").as_ptr() as (*mut u8),
          (*b"Air Guitar!!!  NA na NA na!!\0").as_ptr() as (*mut u8),
          (*b"An aromatherapy candle burns with healing light.\0").as_ptr(
          ) as (*mut u8),
          (*b"You find a bright shiny penny.\0").as_ptr() as (*mut u8),
          (*b"It\'s a free Jon Johansen!\0").as_ptr() as (*mut u8),
          (*b"It\'s a free Dmitry Sklyarov!\0").as_ptr() as (*mut u8),
          (*b"The rothe hits!  The rothe hits!\0").as_ptr() as (*mut u8),
          (*b"It\'s an Internet chain letter about sodium laureth sulfate.\0").as_ptr(
          ) as (*mut u8),
          (*b"Ed Witten sits here, pondering string theory.\0").as_ptr(
          ) as (*mut u8),
          (*b"Something is written here in the dust.  You read: \"rJbotf ndQkttten\".\0").as_ptr(
          ) as (*mut u8),
          (*b"We wish you a merry kitten, and a happy New Year!\0").as_ptr(
          ) as (*mut u8),
          (*b"Run away!  Run away!\0").as_ptr() as (*mut u8),
          (*b"You can see right through this copy of Brin\'s \"Transparent Society\".\0").as_ptr(
          ) as (*mut u8),
          (*b"This copy of \"Steal This Book\" has been stolen from a bookstore.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s Roya Naini.\0").as_ptr() as (*mut u8),
          (*b"This kit is the fourteenth in a series of kits named with Roman letters.\0").as_ptr(
          ) as (*mut u8),
          (*b"This is the tenth key you\'ve found so far.\0").as_ptr(
          ) as (*mut u8),
          (*b"You find a fraud scheme in which loans are used as security for other loans.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the phrase \"and her\", written in ancient Greek.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the author of \"Randomness and Mathematical Proof\".\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the crusty exoskeleton of an arthropod!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s Emporer Shaddam the 4th\'s planet!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s the triangle leg adjacent to an angle divided by the leg opposite it.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a bottle of nail polish remover.\0").as_ptr(
          ) as (*mut u8),
          (*b"You found netkit! Way to go, robot!\0").as_ptr() as (*mut u8),
          (*b"It\'s the ASCII Floating Head of Seth David Schoen!\0").as_ptr(
          ) as (*mut u8),
          (*b"A frosted pink party-cake, half eaten.\0").as_ptr(
          ) as (*mut u8),
          (*b"A bitchin\' homemade tesla coil.\0").as_ptr() as (*mut u8),
          (*b"Conan O\'Brian, sans jawbone.\0").as_ptr() as (*mut u8),
          (*b"It\'s either a mirror, or another soulless kitten-seeking robot.\0").as_ptr(
          ) as (*mut u8),
          (*b"Preoccupation with finding kitten prevents you from investigating further.\0").as_ptr(
          ) as (*mut u8),
          (*b"Fonzie sits here, mumbling incoherently about a shark and a pair of waterskis.\0").as_ptr(
          ) as (*mut u8),
          (*b"The ghost of your dance instructor, his face a paper-white mask of evil.\0").as_ptr(
          ) as (*mut u8),
          (*b"A bag of groceries taken off the shelf before the expiration date.\0").as_ptr(
          ) as (*mut u8),
          (*b"A book: Feng Shui, Zen: the art of randomly arranging items that are not kitten.\0").as_ptr(
          ) as (*mut u8),
          (*b"This might be the fountain of youth, but you\'ll never know.\0").as_ptr(
          ) as (*mut u8),
          (*b"Tigerbot Hesh.\0").as_ptr() as (*mut u8),
          (*b"Stimutacs.\0").as_ptr() as (*mut u8),
          (*b"A canister of pressurized whipped cream, sans whipped cream.\0").as_ptr(
          ) as (*mut u8),
          (*b"The non-kitten item bites!\0").as_ptr() as (*mut u8),
          (*b"A chain hanging from two posts reminds you of the Gateway Arch.\0").as_ptr(
          ) as (*mut u8),
          (*b"A mathematician calculates the halting probability of a Turing machine.\0").as_ptr(
          ) as (*mut u8),
          (*b"A number of short theatrical productions are indexed 1, 2, 3, ... n.\0").as_ptr(
          ) as (*mut u8),
          (*b"A technical university in Australia.\0").as_ptr() as (*mut u8),
          (*b"It is -- I just feel something wonderful is about to happen.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s a Cat 5 cable.\0").as_ptr() as (*mut u8),
          (*b"It\'s a U.S. president.\0").as_ptr() as (*mut u8),
          (*b"It\'s a piece of cloth used to cover a stage in between performances.\0").as_ptr(
          ) as (*mut u8),
          (*b"The ionosphere seems charged with meaning.\0").as_ptr(
          ) as (*mut u8),
          (*b"This tomography is like, hella axial, man!\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s your favorite game -- robotfindscatan!\0").as_ptr(
          ) as (*mut u8),
          (*b"Just a man selling an albatross.\0").as_ptr() as (*mut u8),
          (*b"The intermission from a 1930s silent movie.\0").as_ptr(
          ) as (*mut u8),
          (*b"It\'s an inverted billiard ball!\0").as_ptr() as (*mut u8),
          (*b"The spectre of Sherlock Holmes wills you onwards.\0").as_ptr(
          ) as (*mut u8)
      ];
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct screen_object {
    pub x: i32,
    pub y: i32,
    pub color: i32,
    pub bold: bool,
    pub character: u8,
}

impl Clone for screen_object {
    fn clone(&self) -> Self {
        *self
    }
}













pub struct State {
    robot: screen_object,
    kitten: screen_object,
    num_bogus: i32,
    bogus: [screen_object; 406],
    bogus_messages: [i32; 406],
    used_messages: [i32; 406],
    screen: *mut *mut i32,
}
static mut S: State = State {
    robot: ((screen_object {
                 x: (0i32),
                 y: (0i32),
                 color: (0i32),
                 bold: (false),
                 character: (0u8),
             })),
    kitten: ((screen_object {
                  x: (0i32),
                  y: (0i32),
                  color: (0i32),
                  bold: (false),
                  character: (0u8),
              })),
    num_bogus: (0i32),
    bogus: ([(screen_object {
                  x: (0i32),
                  y: (0i32),
                  color: (0i32),
                  bold: (false),
                  character: (0u8),
              }); 406]),
    bogus_messages: ([0i32; 406]),
    used_messages: ([0i32; 406]),
    screen: (0i32 as (*mut ::std::os::raw::c_void) as (*mut *mut i32)),
};

impl State {
    #[no_mangle]
    pub unsafe extern "C" fn message(&mut self, mut message: *mut u8) {
        (::wrap::wmove)((1i32), (0i32));
        (::wrap::wclrtoeol)();
        mvprintw(1i32, 0i32, (*b"%.*s\0").as_ptr(), COLS, message);
        (::wrap::wmove)((((*(self)).robot).y), (((*(self)).robot).x));
        (::wrap::wrefresh)();
    }
    #[no_mangle]
    pub unsafe extern "C" fn play_game(&mut self) {
        let mut old_x: i32 = ((*(self)).robot).x;
        let mut old_y: i32 = ((*(self)).robot).y;
        let mut input: i32 = ::std::mem::uninitialized();
        input = ((::wrap::wgetch)());
        while (input != 27i32 && (input != b'q' as (i32)) && (input != b'Q' as (i32))) {
            ((self)).process_input((input));
            if !(old_x == ((*(self)).robot).x && (old_y == ((*(self)).robot).y)) {
                if ((::wrap::wmove)((old_y), (old_x))) == -1i32 {
                    -1i32;
                } else {
                    (::wrap::waddch)((b' ' as (usize)));
                }
                *(*((*(self)).screen).offset(old_x as (isize))).offset(old_y as (isize)) = -1i32;
                draw(((*(self)).robot));
                (::wrap::wrefresh)();
                *(*((*(self)).screen).offset(((*(self)).robot).x as (isize)))
                    .offset(((*(self)).robot).y as (isize)) = 0i32;
                old_x = ((*(self)).robot).x;
                old_y = ((*(self)).robot).y;
            }
            input = ((::wrap::wgetch)());
        }
        ((self)).message(((*b"Bye!\0").as_ptr() as (*mut u8)));
        (::wrap::wrefresh)();
        finish(0i32);
    }
    #[no_mangle]
    pub unsafe extern "C" fn process_input(&mut self, mut input: i32) {
        let mut check_x: i32 = ((*(self)).robot).x;
        let mut check_y: i32 = ((*(self)).robot).y;
        if !(input == 0i32) {
            if input == b'F' as (i32) - 64i32 || input == b'L' as (i32) ||
                input == b'l' as (i32) || input == 0o405i32
            {
                check_x = check_x + 1;
            } else if input == b'B' as (i32) - 64i32 || input == b'H' as (i32) ||
                       input == b'h' as (i32) || input == 0o404i32
            {
                check_x = check_x - 1;
            } else if input == b'N' as (i32) || input == b'n' as (i32) || input == 0o522i32 {
                check_x = check_x + 1;
                check_y = check_y + 1;
            } else if input == b'B' as (i32) || input == b'b' as (i32) || input == 0o550i32 {
                check_x = check_x - 1;
                check_y = check_y + 1;
            } else if input == b'N' as (i32) - 64i32 || input == b'J' as (i32) ||
                       input == b'j' as (i32) || input == 0o402i32
            {
                check_y = check_y + 1;
            } else if input == b'U' as (i32) || input == b'u' as (i32) || input == 0o523i32 {
                check_x = check_x + 1;
                check_y = check_y - 1;
            } else if input == b'Y' as (i32) || input == b'y' as (i32) || input == 0o406i32 {
                check_x = check_x - 1;
                check_y = check_y - 1;
            } else if input == b'P' as (i32) - 64i32 || input == b'K' as (i32) ||
                       input == b'k' as (i32) || input == 0o403i32
            {
                check_y = check_y - 1;
            } else if input == b'L' as (i32) - 64i32 {
                (::wrap::wrefresh_curscr)();
            } else {
                ((self)).message(
                    ((*b"Invalid input: Use direction keys or Esc.\0").as_ptr() as (*mut u8)),
                );
                return;
            }
        }
        if check_y < 3i32 || check_y > LINES - 1i32 || check_x < 0i32 || check_x > COLS - 1i32 {
        } else if *(*((*(self)).screen).offset(check_x as (isize))).offset(check_y as (isize)) !=
                   -1i32
        {
            let switch2 =
                *(*((*(self)).screen).offset(check_x as (isize))).offset(check_y as (isize));
            if !(switch2 == 0i32) {
                if switch2 == 1i32 {
                    (::wrap::wmove)((1i32), (0i32));
                    (::wrap::wclrtoeol)();
                    ((self)).play_animation((input));
                } else {
                    let index =
                        ((*(self)).bogus_messages)[(*(*((*(self)).screen).offset(
                            check_x as (isize),
                        )).offset(check_y as (isize)) -
                                                        2i32) as
                                                       (usize)] as (usize);
                    ((self)).message((messages[index]));
                }
            }
        } else {
            (((*(self)).robot)) = (::screen_object {
                                       x: (check_x),
                                       y: (check_y),
                                       ..((*(self)).robot)
                                   });
        }
    }
    #[no_mangle]
    pub unsafe extern "C" fn play_animation(&mut self, mut input: i32) {
        let mut counter: i32 = ::std::mem::uninitialized();
        counter = 4i32;
        while (counter > 0i32) {
            if ((::wrap::wmove)((1i32), (50i32 + counter + 1i32))) == -1i32 {
                -1i32;
            } else {
                (::wrap::waddch)((b' ' as (usize)));
            }
            (::wrap::wmove)((1i32), (50i32 + counter));
            if input == 0o405i32 || input == 0o402i32 || input == 0o540i32 || input == 0o535i32 {
                draw_in_place(((*(self)).kitten));
            } else {
                draw_in_place(((*(self)).robot));
            }
            if ((::wrap::wmove)((1i32), (50i32 - counter))) == -1i32 {
                -1i32;
            } else {
                (::wrap::waddch)((b' ' as (usize)));
            }
            (::wrap::wmove)((1i32), (50i32 - counter + 1i32));
            if input == 0o405i32 || input == 0o402i32 || input == 0o540i32 || input == 0o535i32 {
                draw_in_place(((*(self)).robot));
            } else {
                draw_in_place(((*(self)).kitten));
            }
            (::wrap::wrefresh)();
            (::wrap::sleep)(1u32);
            counter = counter - 1;
        }
        (::wrap::wmove)((1i32), (0i32));
        (::wrap::waddnstr)(
            ((*b"You found kitten! Way to go, robot!\0").as_ptr()),
            (-1i32),
        );
        (::wrap::wrefresh)();
        finish(0i32);
    }
    #[no_mangle]
    pub unsafe extern "C" fn instructions(&mut self) {
        let mut dummy: u8 = ::std::mem::uninitialized();
        mvprintw(0i32, 0i32, (*b"robotfindskitten v%s\n\0").as_ptr(), ver);
        printw(
            (*b"By the illustrious Leonard Richardson (C) 1997, 2000\n\0").as_ptr(),
        );
        printw(
            (*b"Written originally for the Nerth Pork robotfindskitten contest\n\n\0").as_ptr(),
        );
        printw((*b"In this game, you are robot (\0").as_ptr());
        draw_in_place(((*(self)).robot));
        printw((*b"). Your job is to find kitten. This task\n\0").as_ptr());
        printw(
            (*b"is complicated by the existence of various things which are not kitten.\n\0")
                .as_ptr(),
        );
        printw(
            (*b"Robot must touch items to determine if they are kitten or not. The game\n\0")
                .as_ptr(),
        );
        printw(
            (*b"ends when robotfindskitten. Alternatively, you may end the game by hitting\n\0")
                .as_ptr(),
        );
        printw(
            (*b"the Esc key. See the documentation for more information.\n\n\0").as_ptr(),
        );
        printw((*b"Press any key to start.\n\0").as_ptr());
        (::wrap::wrefresh)();
        dummy = ((::wrap::wgetch)()) as (u8);
        (::wrap::wclear)();
    }
    #[no_mangle]
    pub unsafe extern "C" fn initialize_arrays(&mut self) {
        let mut counter: i32 = ::std::mem::uninitialized();
        let mut counter2: i32 = ::std::mem::uninitialized();
        let mut empty: screen_object = ::std::mem::uninitialized();
        let mut i: i32 = 0i32;
        ((*(self)).screen) = (::wrap::malloc)(
            ((::std::mem::size_of::<*mut i32>()) *
                 (((COLS - 1i32 + 1i32) as (usize)))),
        ) as (*mut *mut i32);
        for i in ((0i32)..(COLS - 1i32 + 1i32)) {
            *((*(self)).screen).offset(i as (isize)) =
                (::wrap::malloc)(
                    ((::std::mem::size_of::<i32>()) * (((LINES - 1i32 + 1i32) as (usize)))),
                ) as (*mut i32);
        }
        (empty) = (::screen_object {
                       x: (-1i32),
                       y: (-1i32),
                       color: (0i32),
                       bold: (false),
                       character: (b' '),
                       ..(empty)
                   });
        counter = 0i32;
        while (counter <= COLS - 1i32) {
            counter2 = 0i32;
            while (counter2 <= LINES - 1i32) {
                *(*((*(self)).screen).offset(counter as (isize))).offset(counter2 as (isize)) =
                    -1i32;
                counter2 = counter2 + 1;
            }
            counter = counter + 1;
        }
        for counter in ((0i32)..(406i32)) {
            ((*(self)).used_messages)[counter as (usize)] = 0i32;
            ((*(self)).bogus_messages)[counter as (usize)] = 0i32;
            ((*(self)).bogus)[counter as (usize)] = empty;
        }
    }
    #[no_mangle]
    pub unsafe extern "C" fn initialize_robot(&mut self) {
        (((*(self)).robot)) = (::screen_object {
                                   x: ((::wrap::rand)() % (COLS - 1i32) + 1i32),
                                   y: ((::wrap::rand)() % (LINES - 1i32 - 3i32 + 1i32) + 3i32),
                                   character: (b'#'),
                                   color: (0i32),
                                   bold: (false),
                                   ..((*(self)).robot)
                               });
        *(*((*(self)).screen).offset(((*(self)).robot).x as (isize)))
            .offset(((*(self)).robot).y as (isize)) = 0i32;
    }
    #[no_mangle]
    pub unsafe extern "C" fn initialize_kitten(&mut self) {
        loop {
            (((*(self)).kitten)) = (::screen_object {
                                        x: ((::wrap::rand)() % (COLS - 1i32) + 1i32),
                                        y: ((::wrap::rand)() % (LINES - 1i32 - 3i32 + 1i32) + 3i32),
                                        ..((*(self)).kitten)
                                    });
            if !(*(*((*(self)).screen).offset(((*(self)).kitten).x as (isize)))
                     .offset(((*(self)).kitten).y as (isize)) != -1i32)
            {
                break;
            }
        }
        loop {
            (((*(self)).kitten)) =
                (::screen_object {
                     character: (((::wrap::rand)() % (126i32 - b'!' as (i32) + 1i32) +
                                      b'!' as (i32)) as (u8)),
                     ..((*(self)).kitten)
                 });
            if !(validchar(((*(self)).kitten).character) == 0) {
                break;
            }
        }
        *(*((*(self)).screen).offset(((*(self)).kitten).x as (isize)))
            .offset(((*(self)).kitten).y as (isize)) = 1i32;
        (((*(self)).kitten)) = (::screen_object {
                                    color: ((::wrap::rand)() % 6i32 + 1i32),
                                    bold: (if (::wrap::rand)() % 2i32 != 0 {
                                               1i32
                                           } else {
                                               0i32
                                           } != 0),
                                    ..((*(self)).kitten)
                                });
    }
    #[no_mangle]
    pub unsafe extern "C" fn initialize_bogus(&mut self) {
        let mut counter: i32 = ::std::mem::uninitialized();
        let mut index: i32 = ::std::mem::uninitialized();
        for counter in ((0i32)..(((*(self)).num_bogus))) {
            (((*(self)).bogus)[counter as (usize)]) =
                (::screen_object {
                     color: ((::wrap::rand)() % 6i32 + 1i32),
                     bold: (if (::wrap::rand)() % 2i32 != 0 {
                                1i32
                            } else {
                                0i32
                            } != 0),
                     ..(((*(self)).bogus)[counter as (usize)])
                 });
            loop {
                (((*(self)).bogus)[counter as (usize)]) =
                    (::screen_object {
                         character: (((::wrap::rand)() % (126i32 - b'!' as (i32) + 1i32) +
                                          b'!' as (i32)) as
                                         (u8)),
                         ..(((*(self)).bogus)[counter as (usize)])
                     });
                if !(validchar(((*(self)).bogus)[counter as (usize)].character) == 0) {
                    break;
                }
            }
            loop {
                (((*(self)).bogus)[counter as (usize)]) =
                    (::screen_object {
                         x: ((::wrap::rand)() % (COLS - 1i32) + 1i32),
                         y: ((::wrap::rand)() % (LINES - 1i32 - 3i32 + 1i32) + 3i32),
                         ..(((*(self)).bogus)[counter as (usize)])
                     });
                if !(*(*((*(self)).screen).offset(
                    ((*(self)).bogus)[counter as (usize)]
                        .x as (isize),
                )).offset(((*(self)).bogus)[counter as (usize)].y as (isize)) !=
                         -1i32)
                {
                    break;
                }
            }
            *(*((*(self)).screen).offset(((*(self)).bogus)[counter as (usize)].x as (isize)))
                .offset(((*(self)).bogus)[counter as (usize)].y as (isize)) = counter + 2i32;
            loop {
                index = (::wrap::rand)() % 406i32;
                if !(((*(self)).used_messages)[index as (usize)] != 0i32) {
                    break;
                }
            }
            ((*(self)).bogus_messages)[counter as (usize)] = index;
            ((*(self)).used_messages)[index as (usize)] = 1i32;
        }
    }
    #[no_mangle]
    pub unsafe extern "C" fn initialize_screen(&mut self) {
        let mut counter: i32 = ::std::mem::uninitialized();
        mvprintw(0i32, 0i32, (*b"robotfindskitten v%s\n\n\0").as_ptr(), ver);
        counter = 0i32;
        while (counter <= COLS - 1i32) {
            printw((*b"%c\0").as_ptr(), 95i32);
            counter = counter + 1;
        }
        for counter in ((0i32)..(((*(self)).num_bogus))) {
            draw(((*(self)).bogus)[counter as (usize)]);
        }
        draw(((*(self)).kitten));
        draw(((*(self)).robot));
        (::wrap::wrefresh)();
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct pdat {
    pub _pad_y: i16,
    pub _pad_x: i16,
    pub _pad_top: i16,
    pub _pad_left: i16,
    pub _pad_bottom: i16,
    pub _pad_right: i16,
}

impl Clone for pdat {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct _win_st {
    pub _cury: i16,
    pub _curx: i16,
    pub _maxy: i16,
    pub _maxx: i16,
    pub _begy: i16,
    pub _begx: i16,
    pub _flags: i16,
    pub _attrs: usize,
    pub _bkgd: usize,
    pub _notimeout: bool,
    pub _clear: bool,
    pub _leaveok: bool,
    pub _scroll: bool,
    pub _idlok: bool,
    pub _idcok: bool,
    pub _immed: bool,
    pub _sync: bool,
    pub _use_keypad: bool,
    pub _delay: i32,
    pub _line: *mut ldat,
    pub _regtop: i16,
    pub _regbottom: i16,
    pub _parx: i32,
    pub _pary: i32,
    pub _parent: *mut _win_st,
    pub _pad: pdat,
    pub _yoffset: i16,
}

impl Clone for _win_st {
    fn clone(&self) -> Self {
        *self
    }
}

#[no_mangle]
pub unsafe extern "C" fn full_draw(mut o: screen_object, mut in_place: bool) {
    let mut old: usize = ::std::mem::uninitialized();
    let mut dummy: i16 = ::std::mem::uninitialized();
    let mut new: usize = ::std::mem::uninitialized();
    if &mut old as (*mut usize) != 0i32 as (*mut ::std::os::raw::c_void) as (*mut usize) {
        *(&mut old as (*mut usize)) = if !stdscr.is_null() {
            (*stdscr)._attrs
        } else {
            0usize
        };
    } else {
        0i32;
    }
    if &mut dummy as (*mut i16) != 0i32 as (*mut ::std::os::raw::c_void) as (*mut i16) {
        *(&mut dummy as (*mut i16)) = if !stdscr.is_null() {
            (((*stdscr)._attrs & ((1usize << 8i32)) - (1usize) << 0i32 + 8i32) >> 8i32) as (i32)
        } else {
            0i32
        } as (i16);
    } else {
        0i32;
    }
    0i32;
    new = o.color as (usize) << 0i32 + 8i32;
    if o.character as (i32) == b'#' as (i32) {
        new = new | 1usize << 12i32 + 8i32;
    }
    if o.character as (i32) <= b'\x1A' as (i32) {
        new = new | 1usize << 14i32 + 8i32;
    }
    if o.bold {
        new = new | 1usize << 13i32 + 8i32;
    }
    if !stdscr.is_null() {
        (*stdscr) = ::_win_st {
            _attrs: (new),
            ..(*stdscr)
        };
        0i32;
    } else {
        -1i32;
    }
    if in_place {
        printw((*b"%c\0").as_ptr(), o.character as (i32));
    } else {
        mvprintw(o.y, o.x, (*b"%c\0").as_ptr(), o.character as (i32));
        (::wrap::wmove)((o.y), (o.x));
    }
    if !stdscr.is_null() {
        (*stdscr) = ::_win_st {
            _attrs: (old),
            ..(*stdscr)
        };
        0i32;
    } else {
        -1i32;
    }
}

#[no_mangle]
pub extern "C" fn draw(mut o: screen_object) {
    unsafe {
        full_draw(o, false);
    }
}

#[no_mangle]
pub extern "C" fn draw_in_place(mut o: screen_object) {
    unsafe {
        full_draw(o, true);
    }
}







#[no_mangle]
pub unsafe extern "C" fn finish(mut sig: i32) {
    (::wrap::endwin)();
    printf((*b"%c%c%c\0").as_ptr(), 27i32, b'(' as (i32), b'B' as (i32));
    (::wrap::exit)(0i32);
}

#[no_mangle]
pub extern "C" fn validchar(mut a: u8) -> i32 {
    unsafe {
        if a as (i32) == 127i32 || a as (i32) == b' ' as (i32) || a as (i32) == b'#' as (i32) {
            0i32
        } else {
            1i32
        }
    }
}







#[no_mangle]
pub extern "C" fn initialize_ncurses() {
    unsafe {
        (::wrap::signal)(2i32, finish);
        (::wrap::initscr)();
        (::wrap::keypad)((true));
        (::wrap::nonl)();
        (::wrap::intrflush)((false));
        (::wrap::noecho)();
        (::wrap::cbreak)();
        if (::wrap::has_colors)() {
            (::wrap::start_color)();
            (::wrap::init_pair)(0i16, 0i16, 0i16);
            (::wrap::init_pair)(2i16, 2i16, 0i16);
            (::wrap::init_pair)(1i16, 1i16, 0i16);
            (::wrap::init_pair)(6i16, 6i16, 0i16);
            (::wrap::init_pair)(7i16, 7i16, 0i16);
            (::wrap::init_pair)(5i16, 5i16, 0i16);
            (::wrap::init_pair)(4i16, 4i16, 0i16);
            (::wrap::init_pair)(3i16, 3i16, 0i16);
        }
    }
}









fn main() {
    use std::os::unix::ffi::OsStringExt;

    _init_ver();
    _init_messages();

    let mut argv_storage = ::std::env::args_os()
        .map(|str| {
            let mut vec = str.into_vec();
            vec.push(b'\0');
            vec
        })
        .collect::<Vec<_>>();
    let mut argv = argv_storage
        .iter_mut()
        .map(|vec| vec.as_mut_ptr())
        .chain(Some(::std::ptr::null_mut()))
        .collect::<Vec<_>>();
    let ret = unsafe { _c_main(argv_storage.len() as (i32), argv.as_mut_ptr()) };
    ::std::process::exit(ret);
}

#[no_mangle]
pub unsafe extern "C" fn _c_main(mut argc: i32, mut argv: *mut *mut u8) -> i32 {
    if argc == 1i32 {
        (S.num_bogus) = 20i32;
    } else {
        (S.num_bogus) = (::wrap::atoi)(*argv.offset(1isize) as (*const u8));
        if (S.num_bogus) < 0i32 || (S.num_bogus) > 406i32 {
            printf(
                (*b"Run-time parameter must be between 0 and %d.\n\0").as_ptr(),
                406i32,
            );
            (::wrap::exit)(0i32);
        }
    }
    (::wrap::srand)((::wrap::time)(0i32 as (*mut isize)) as (u32));
    printf((*b"%c%c%c\0").as_ptr(), 27i32, b'(' as (i32), b'U' as (i32));
    initialize_ncurses();
    ((&mut S)).initialize_arrays();
    ((&mut S)).initialize_robot();
    ((&mut S)).initialize_kitten();
    ((&mut S)).initialize_bogus();
    ((&mut S)).instructions();
    ((&mut S)).initialize_screen();
    ((&mut S)).play_game();
    0
}
