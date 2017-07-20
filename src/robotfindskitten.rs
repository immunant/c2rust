#![feature(alloc_system, allocator_api, global_allocator)]
extern crate alloc_system;

use std::ffi::CStr;
use std::os::raw::c_char;

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

macro_rules! mvprintw {
    ($x:expr, $y:expr, $fmt:expr $(, $arg:expr)*) => {
        ::wrap::mvprintw($x, $y,
                         ::std::ffi::CStr::from_bytes_with_nul(
                             format!($fmt $(, $arg)*).as_bytes()).unwrap())
    };
}

macro_rules! printf {
    ($fmt:expr $(, $arg:expr)*) => {
        ::wrap::printf(::std::ffi::CStr::from_bytes_with_nul(
                format!($fmt $(, $arg)*).as_bytes()).unwrap())
    };
}

macro_rules! printw {
    ($fmt:expr $(, $arg:expr)*) => {
        ::wrap::printw(::std::ffi::CStr::from_bytes_with_nul(
                format!($fmt $(, $arg)*).as_bytes()).unwrap())
    };
}

mod wrap {
    use std::ffi::CStr;
    use super::_win_st;

    pub fn lines() -> i32 {
        unsafe { ::LINES }
    }
    pub fn cols() -> i32 {
        unsafe { ::COLS }
    }

    pub fn mvprintw(x: i32, y: i32, s: &CStr) -> i32 {
        unsafe { ::mvprintw(x, y, b"%s\0".as_ptr(), s.as_ptr()) }
    }
    pub fn printf(s: &CStr) -> i32 {
        unsafe { ::printf(b"%s\0".as_ptr(), s.as_ptr()) }
    }
    pub fn printw(s: &CStr) -> i32 {
        unsafe { ::printw(b"%s\0".as_ptr(), s.as_ptr()) }
    }

    pub fn atoi(__nptr: &CStr) -> i32 {
        unsafe { ::atoi(((__nptr).as_ptr() as *mut u8)) }
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
    pub unsafe fn time() -> isize {
        ::time(::std::ptr::null_mut())
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

static messages: [&[u8]; 406]
    = [   (b"\"I pity the fool who mistakes me for kitten!\", sez Mr. T.\x00"),
          (b"That\'s just an old tin can.\x00"),
          (b"It\'s an altar to the horse god.\x00"),
          (b"A box of dancing mechanical pencils. They dance! They sing!\x00"),
          (b"It\'s an old Duke Ellington record.\x00"),
          (b"A box of fumigation pellets.\x00"),
          (b"A digital clock. It\'s stuck at 2:17 PM.\x00"),
          (b"That\'s just a charred human corpse.\x00"),
          (b"I don\'t know what that is, but it\'s not kitten.\x00"),
          (b"An empty shopping bag. Paper or plastic?\x00"),
          (b"Could it be... a big ugly bowling trophy?\x00"),
          (b"A coat hanger hovers in thin air. Odd.\x00"),
          (b"Not kitten, just a packet of Kool-Aid(tm).\x00"),
          (b"A freshly-baked pumpkin pie.\x00"),
          (b"A lone, forgotten comma, sits here, sobbing.\x00"),
          (b"ONE HUNDRED THOUSAND CARPET FIBERS!!!!!\x00"),
          (b"It\'s Richard Nixon\'s nose!\x00"),
          (b"It\'s Lucy Ricardo. \"Aaaah, Ricky!\", she says.\x00"),
          (b"You stumble upon Bill Gates\' stand-up act.\x00"),
          (b"Just an autographed copy of the Kama Sutra.\x00"),
          (b"It\'s the Will Rogers Highway. Who was Will Rogers, anyway?\x00"),
          (b"It\'s another robot, more advanced in design than you but strangely immobile.\x00"),
          (b"Leonard Richardson is here, asking people to lick him.\x00"),
          (b"It\'s a stupid mask, fashioned after a beagle.\x00"),
          (b"Your State Farm Insurance(tm) representative!\x00"),
          (b"It\'s the local draft board.\x00"),
          (b"Seven 1/4\" screws and a piece of plastic.\x00"),
          (b"An 80286 machine.\x00"),
          (b"One of those stupid \"Homes of the Stars\" maps.\x00"),
          (b"A signpost saying \"TO KITTEN\". It points in no particular direction.\x00"),
          (b"A hammock stretched between a tree and a volleyball pole.\x00"),
          (b"A Texas Instruments of Destruction calculator.\x00"),
          (b"It\'s a dark, amphorous blob of matter.\x00"),
          (b"Just a pincushion.\x00"),
          (b"It\'s a mighty zombie talking about some love and prosperity.\x00"),
          (b"\"Dear robot, you may have already won our 10 MILLION DOLLAR prize...\"\x00"),
          (b"It\'s just an object.\x00"),
          (b"A mere collection of pixels.\x00"),
          (b"A badly dented high-hat cymbal lies on its side here.\x00"),
          (b"A marijuana brownie.\x00"),
          (b"A plush Chewbacca.\x00"),
          (b"Daily hunger conditioner from Australasia\x00"),
          (b"Just some stuff.\x00"),
          (b"Why are you touching this when you should be finding kitten?\x00"),
          (b"A glorious fan of peacock feathers.\x00"),
          (b"It\'s some compromising photos of Babar the Elephant.\x00"),
          (b"A copy of the Weekly World News. Watch out for the chambered nautilus!\x00"),
          (b"It\'s the proverbial wet blanket.\x00"),
          (b"A \"Get Out of Jail Free\" card.\x00"),
          (b"An incredibly expensive \"Mad About You\" collector plate.\x00"),
          (b"Paul Moyer\'s necktie.\x00"),
          (b"A haircut and a real job. Now you know where to get one!\x00"),
          (b"An automated robot-hater. It frowns disapprovingly at you.\x00"),
          (b"An automated robot-liker. It smiles at you.\x00"),
          (b"It\'s a black hole. Don\'t fall in!\x00"),
          (b"Just a big brick wall.\x00"),
          (b"You found kitten! No, just kidding.\x00"),
          (b"Heart of Darkness brand pistachio nuts.\x00"),
          (b"A smoking branding iron shaped like a 24-pin connector.\x00"),
          (b"It\'s a Java applet.\x00"),
          (b"An abandoned used-car lot.\x00"),
          (b"A shameless plug for Crummy: http://www.crummy.com/\x00"),
          (b"A shameless plug for the UCLA Linux Users Group: http://linux.ucla.edu/\x00"),
          (b"A can of Spam Lite.\x00"),
          (b"This is another fine mess you\'ve gotten us into, Stanley.\x00"),
          (b"It\'s scenery for \"Waiting for Godot\".\x00"),
          (b"This grain elevator towers high above you.\x00"),
          (b"A Mentos wrapper.\x00"),
          (b"It\'s the constellation Pisces.\x00"),
          (b"It\'s a fly on the wall. Hi, fly!\x00"),
          (b"This kind of looks like kitten, but it\'s not.\x00"),
          (b"It\'s a banana! Oh, joy!\x00"),
          (b"A helicopter has crashed here.\x00"),
          (b"Carlos Tarango stands here, doing his best impression of Pat Smear.\x00"),
          (b"A patch of mushrooms grows here.\x00"),
          (b"A patch of grape jelly grows here.\x00"),
          (b"A spindle, and a grindle, and a bucka-wacka-woom!\x00"),
          (b"A geyser sprays water high into the air.\x00"),
          (b"A toenail? What good is a toenail?\x00"),
          (b"You\'ve found the fish! Not that it does you much good in this game.\x00"),
          (b"A Buttertonsils bar.\x00"),
          (b"One of the few remaining discoes.\x00"),
          (b"Ah, the uniform of a Revolutionary-era minuteman.\x00"),
          (b"A punch bowl, filled with punch and lemon slices.\x00"),
          (b"It\'s nothing but a G-thang, baby.\x00"),
          (b"IT\'S ALIVE! AH HA HA HA HA!\x00"),
          (b"This was no boating accident!\x00"),
          (b"Wait! This isn\'t the poker chip! You\'ve been tricked! DAMN YOU, MENDEZ!\x00"),
          (b"A livery stable! Get your livery!\x00"),
          (b"It\'s a perpetual immobility machine.\x00"),
          (b"\"On this spot in 1962, Henry Winkler was sick.\"\x00"),
          (b"There\'s nothing here; it\'s just an optical illusion.\x00"),
          (b"The World\'s Biggest Motzah Ball!\x00"),
          (b"A tribe of cannibals lives here. They eat Malt-O-Meal for breakfast, you know.\x00"),
          (b"This appears to be a rather large stack of trashy romance novels.\x00"),
          (b"Look out! Exclamation points!\x00"),
          (b"A herd of wild coffee mugs slumbers here.\x00"),
          (b"It\'s a limbo bar! How low can you go?\x00"),
          (b"It\'s the horizon. Now THAT\'S weird.\x00"),
          (b"A vase full of artificial flowers is stuck to the floor here.\x00"),
          (b"A large snake bars your way.\x00"),
          (b"A pair of saloon-style doors swing slowly back and forth here.\x00"),
          (b"It\'s an ordinary bust of Beethoven... but why is it painted green?\x00"),
          (b"It\'s TV\'s lovable wisecracking Crow! \"Bite me!\", he says.\x00"),
          (b"Hey, look, it\'s war. What is it good for? Absolutely nothing. Say it again.\x00"),
          (b"It\'s the amazing self-referential thing that\'s not kitten.\x00"),
          (b"A flamboyant feather boa. Now you can dress up like Carol Channing!\x00"),
          (b"\"Sure hope we get some rain soon,\" says Farmer Joe.\x00"),
          (b"\"How in heck can I wash my neck if it ain\'t gonna rain no more?\" asks Farmer Al.\x00"),
          (b"\"Topsoil\'s all gone, ma,\" weeps Lil\' Greg.\x00"),
          (b"This is a large brown bear. Oddly enough, it\'s currently peeing in the woods.\x00"),
          (b"A team of arctic explorers is camped here.\x00"),
          (b"This object here appears to be Louis Farrakhan\'s bow tie.\x00"),
          (b"This is the world-famous Chain of Jockstraps.\x00"),
          (b"A trash compactor, compacting away.\x00"),
          (b"This toaster strudel is riddled with bullet holes!\x00"),
          (b"It\'s a hologram of a crashed helicopter.\x00"),
          (b"This is a television. On screen you see a robot strangely similar to yourself.\x00"),
          (b"This balogna has a first name, it\'s R-A-N-C-I-D.\x00"),
          (b"A salmon hatchery? Look again. It\'s merely a single salmon.\x00"),
          (b"It\'s a rim shot. Ba-da-boom!\x00"),
          (b"It\'s creepy and it\'s kooky, mysterious and spooky. It\'s also somewhat ooky.\x00"),
          (b"This is an anagram.\x00"),
          (b"This object is like an analogy.\x00"),
          (b"It\'s a symbol. You see in it a model for all symbols everywhere.\x00"),
          (b"The object pushes back at you.\x00"),
          (b"A traffic signal. It appears to have been recently vandalized.\x00"),
          (b"\"There is no kitten!\" cackles the old crone. You are shocked by her blasphemy.\x00"),
          (b"This is a Lagrange point. Don\'t come too close now.\x00"),
          (b"The dirty old tramp bemoans the loss of his harmonica.\x00"),
          (b"Look, it\'s Fanny the Irishman!\x00"),
          (b"What in blazes is this?\x00"),
          (b"It\'s the instruction manual for a previous version of this game.\x00"),
          (b"A brain cell. Oddly enough, it seems to be functioning.\x00"),
          (b"Tea and/or crumpets.\x00"),
          (b"This jukebox has nothing but Cliff Richards albums in it.\x00"),
          (b"It\'s a Quaker Oatmeal tube, converted into a drum.\x00"),
          (b"This is a remote control. Being a robot, you keep a wide berth.\x00"),
          (b"It\'s a roll of industrial-strength copper wire.\x00"),
          (b"Oh boy! Grub! Er, grubs.\x00"),
          (b"A puddle of mud, where the mudskippers play.\x00"),
          (b"Plenty of nothing.\x00"),
          (b"Look at that, it\'s the Crudmobile.\x00"),
          (b"Just Walter Mattheau and Jack Lemmon.\x00"),
          (b"Two crepes, two crepes in a box.\x00"),
          (b"An autographed copy of \"Primary Colors\", by Anonymous.\x00"),
          (b"Another rabbit? That\'s three today!\x00"),
          (b"It\'s a segmentation fault. Core dumped, by the way.\x00"),
          (b"A historical marker showing the actual location of /dev/null.\x00"),
          (b"Thar\'s Mobius Dick, the convoluted whale. Arrr!\x00"),
          (b"It\'s a charcoal briquette, smoking away.\x00"),
          (b"A pizza, melting in the sun.\x00"),
          (b"It\'s a \"HOME ALONE 2: Lost in New York\" novelty cup.\x00"),
          (b"A stack of 7 inch floppies wobbles precariously.\x00"),
          (b"It\'s nothing but a corrupted floppy. Coaster anyone?\x00"),
          (b"A section of glowing phosphor cells sings a song of radiation to you.\x00"),
          (b"This TRS-80 III is eerily silent.\x00"),
          (b"A toilet bowl occupies this space.\x00"),
          (b"This peg-leg is stuck in a knothole!\x00"),
          (b"It\'s a solitary vacuum tube.\x00"),
          (b"This corroded robot is clutching a mitten.\x00"),
          (b"\"Hi, I\'m Anson Williams, TV\'s \'Potsy\'.\"\x00"),
          (b"This subwoofer was blown out in 1974.\x00"),
          (b"Three half-pennies and a wooden nickel.\x00"),
          (b"It\'s the missing chapter to \"A Clockwork Orange\".\x00"),
          (b"It\'s a burrito stand flyer. \"Taqueria El Ranchito\".\x00"),
          (b"This smiling family is happy because they eat LARD.\x00"),
          (b"Roger Avery, persona un famoso de los Estados Unidos.\x00"),
          (b"Ne\'er but a potted plant.\x00"),
          (b"A parrot, kipping on its back.\x00"),
          (b"A forgotten telephone switchboard.\x00"),
          (b"A forgotten telephone switchboard operator.\x00"),
          (b"It\'s an automated robot-disdainer. It pretends you\'re not there.\x00"),
          (b"It\'s a portable hole. A sign reads: \"Closed for the winter\".\x00"),
          (b"Just a moldy loaf of bread.\x00"),
          (b"A little glass tub of Carmex. ($.89) Too bad you have no lips.\x00"),
          (b"A Swiss-Army knife. All of its appendages are out. (toothpick lost)\x00"),
          (b"It\'s a zen simulation, trapped within an ASCII character.\x00"),
          (b"It\'s a copy of \"The Rubaiyat of Spike Schudy\".\x00"),
          (b"It\'s \"War and Peace\" (unabridged, very small print).\x00"),
          (b"A willing, ripe tomato bemoans your inability to digest fruit.\x00"),
          (b"A robot comedian. You feel amused.\x00"),
          (b"It\'s KITT, the talking car.\x00"),
          (b"Here\'s Pete Peterson. His batteries seem to have long gone dead.\x00"),
          (b"\"Blup, blup, blup\", says the mud pot.\x00"),
          (b"More grist for the mill.\x00"),
          (b"Grind \'em up, spit \'em out, they\'re twigs.\x00"),
          (b"The boom box cranks out an old Ethel Merman tune.\x00"),
          (b"It\'s \"Finding kitten\", published by O\'Reilly and Associates.\x00"),
          (b"Pumpkin pie spice.\x00"),
          (b"It\'s the Bass-Matic \'76! Mmm, that\'s good bass!\x00"),
          (b"\"Lend us a fiver \'til Thursday\", pleas Andy Capp.\x00"),
          (b"It\'s a tape of \'70s rock. All original hits! All original artists!\x00"),
          (b"You\'ve found the fabled America Online disk graveyard!\x00"),
          (b"Empty jewelboxes litter the landscape.\x00"),
          (b"It\'s the astounding meta-object.\x00"),
          (b"Ed McMahon stands here, lost in thought. Seeing you, he bellows, \"YES SIR!\"\x00"),
          (b"...thingy???\x00"),
          (b"It\'s 1000 secrets the government doesn\'t want you to know!\x00"),
          (b"The letters O and R.\x00"),
          (b"A magical... magic thing.\x00"),
          (b"It\'s a moment of silence.\x00"),
          (b"It\'s Sirhan-Sirhan, looking guilty.\x00"),
          (b"It\'s \"Chicken Soup for the Kitten-seeking Soulless Robot.\"\x00"),
          (b"It is a set of wind-up chatter teeth.\x00"),
          (b"It is a cloud shaped like an ox.\x00"),
          (b"You see a snowflake here, melting slowly.\x00"),
          (b"It\'s a big block of ice. Something seems to be frozen inside it.\x00"),
          (b"Vladimir Lenin\'s casket rests here.\x00"),
          (b"It\'s a copy of \"Zen and The Art of Robot Maintenance\".\x00"),
          (b"This invisible box contains a pantomime horse.\x00"),
          (b"A mason jar lies here open. It\'s label reads: \"do not open!\".\x00"),
          (b"A train of thought chugs through here.\x00"),
          (b"This jar of pickles expired in 1957.\x00"),
          (b"Someone\'s identity disk lies here.\x00"),
          (b"\"Yes!\" says the bit.\x00"),
          (b"\"No!\" says the bit.\x00"),
          (b"A dodecahedron bars your way.\x00"),
          (b"Mr. Hooper is here, surfing.\x00"),
          (b"It\'s a big smoking fish.\x00"),
          (b"You have new mail in /var/spool/robot\x00"),
          (b"Just a monitor with the blue element burnt out.\x00"),
          (b"A pile of coaxial plumbing lies here.\x00"),
          (b"It\'s a rotten old shoe.\x00"),
          (b"It\'s a hundred-dollar bill.\x00"),
          (b"It\'s a Dvorak keyboard.\x00"),
          (b"It\'s a cardboard box full of 8-tracks.\x00"),
          (b"Just a broken hard drive containg the archives of Nerth Pork.\x00"),
          (b"A broken metronome sits here, it\'s needle off to one side.\x00"),
          (b"A sign reads: \"Go home!\"\x00"),
          (b"A sign reads: \"No robots allowed!\"\x00"),
          (b"It\'s the handheld robotfindskitten game, by Tiger.\x00"),
          (b"This particular monstrosity appears to be ENIAC.\x00"),
          (b"This is a tasty-looking banana creme pie.\x00"),
          (b"A wireframe model of a hot dog rotates in space here.\x00"),
          (b"Just the empty husk of a locust.\x00"),
          (b"You disturb a murder of crows.\x00"),
          (b"It\'s a copy of the robotfindskitten EULA.\x00"),
          (b"It\'s Death.\x00"),
          (b"It\'s an autographed copy of \"Secondary Colors,\" by Bob Ross.\x00"),
          (b"It is a marzipan dreadnought that appears to have melted and stuck.\x00"),
          (b"It\'s a DVD of \"Crouching Monkey, Hidden Kitten\", region encoded for the moon.\x00"),
          (b"It\'s Kieran Hervold.  Damn dyslexia!\x00"),
          (b"A non-descript box of crackers.\x00"),
          (b"Carbonated Water, High Fructose Corn Syrup, Color, Phosphoric Acid, Flavors, Caffeine.\x00"),
          (b"\"Move along! Nothing to see here!\"\x00"),
          (b"It\'s the embalmed corpse of Vladimir Lenin.\x00"),
          (b"A coupon for one free steak-fish at your local family diner.\x00"),
          (b"A set of keys to a 2001 Rolls Royce. Worthless.\x00"),
          (b"A gravestone stands here.  \"Izchak Miller, ascended.\"\x00"),
          (b"Someone has written \"ad aerarium\" on the ground here.\x00"),
          (b"A large blue eye floats in midair.\x00"),
          (b"This appears to be a statue of Perseus.\x00"),
          (b"There is an opulent throne here.\x00"),
          (b"It\'s a squad of Keystone Kops.\x00"),
          (b"This seems to be junk mail addressed to the finder of the Eye of Larn.\x00"),
          (b"A wondrous and intricate golden amulet.  Too bad you have no neck.\x00"),
          (b"The swampy ground around you seems to stink with disease.\x00"),
          (b"An animate blob of acid.  Being metallic, you keep well away.\x00"),
          (b"It\'s a copy of Knuth with the chapter on kitten-search algorithms torn out.\x00"),
          (b"A crowd of people, and at the center, a popular misconception.\x00"),
          (b"It\'s a blind man. When you touch, he exclaims \"It\'s a kitten prospecting robot!\"\x00"),
          (b"It\'s a lost wallet. It\'s owner didn\'t have pets, so you discard it.\x00"),
          (b"This place is called Antarctica. There is no kitten here.\x00"),
          (b"It\'s a mousetrap, baited with soap.\x00"),
          (b"A book with \"Don\'t Panic\" in large friendly letters across the cover.\x00"),
          (b"A compendium of haiku about metals.\x00"),
          (b"A discredited cosmology, relic of a bygone era.\x00"),
          (b"A hollow voice says \"Plugh\".\x00"),
          (b"A knight who says \"Either I am an insane knave, or you will find kitten.\"\x00"),
          (b"A neural net -- maybe it\'s trying to recognize kitten.\x00"),
          (b"A screwdriver.\x00"),
          (b"A statue of a girl holding a goose like the one in Gottingen, Germany.\x00"),
          (b"A tetradrachm dated \"42 B.C.\"\x00"),
          (b"A voice booms out \"Onward, kitten soldiers...\"\x00"),
          (b"An eminently forgettable zahir.\x00"),
          (b"Apparently, it\'s Edmund Burke.\x00"),
          (b"For a moment, you feel something in your hands, but it disappears!\x00"),
          (b"Here is a book about Robert Kennedy.\x00"),
          (b"Hey, robot, leave those lists alone.\x00"),
          (b"Ho hum.  Another synthetic a posteriori.\x00"),
          (b"It\'s Asimov\'s Laws of Robotics.  You feel a strange affinity for them.\x00"),
          (b"It\'s Bach\'s Mass in B-minor!\x00"),
          (b"It\'s a bug.\x00"),
          (b"It\'s a synthetic a priori truth!  Immanuel would be so pleased!\x00"),
          (b"It\'s the Tiki Room.\x00"),
          (b"Just some old play by a Czech playwright, and you can\'t read Czech.\x00"),
          (b"Kitten is the letter \'Q\'.  Oh, wait, maybe not.\x00"),
          (b"Quidquid Latine dictum sit, kitten non est.\x00"),
          (b"Sutro Tower is visible at some distance through the fog.\x00"),
          (b"The Digital Millennium Copyright Act of 1998.\x00"),
          (b"The United States Court of Appeals for the Federal Circuit.\x00"),
          (b"The non-kitten item like this but with \"false\" and \"true\" switched is true.\x00"),
          (b"The non-kitten item like this but with \"true\" and \"false\" switched is false.\x00"),
          (b"This is the chapter called \"A Map of the Cat?\" from Feynman\'s autobiography.\x00"),
          (b"This is the forest primeval.\x00"),
          (b"Werner\'s \"Pocket Field Guide to Things That Are Not Kitten\".\x00"),
          (b"You found nettik, but that\'s backwards.\x00"),
          (b"You have found some zinc, but you must not stop here, for you must find kitten.\x00"),
          (b"\"50 Years Among the Non-Kitten Items\", by Ann Droyd.\x00"),
          (b"\"Robot may not injure kitten, or, through inaction, ...\"\x00"),
          (b"\"Address Allocation for Private Internets\" by Yakov Rekhter et al.\x00"),
          (b"\"Mail Routing and the Domain System\" by Craig Partridge.\x00"),
          (b"\"The Theory and Practice of Oligarchical Collectivism\" by Emmanuel Goldstein.\x00"),
          (b"\"201 Kitten Verbs, Fully Conjugated\".  You look for \"find\".\x00"),
          (b"A card shark sits here, practicing his Faro shuffle.  He ignores you.\x00"),
          (b"A copy of DeCSS.  They\'re a dime a dozen these days.\x00"),
          (b"A demonic voice proclaims \"There is no kitten, only Zuul\".  You flee.\x00"),
          (b"A lotus.  You make an interesting pair.\x00"),
          (b"A milk carton, with a black and white picture of kitten on the side.\x00"),
          (b"Any ordinary robot could see from a mile away that this wasn\'t kitten.\x00"),
          (b"A stegosaurus, escaped from the stegosaurusfindsrobot game.  It finds you.\x00"),
          (b"Baling wire and chewing gum.\x00"),
          (b"Chewing gum and baling wire.\x00"),
          (b"Here is no kitten but only rock, rock and no kitten and the sandy road.\x00"),
          (b"Hey, I bet you thought this was kitten.\x00"),
          (b"It is an ancient mariner, and he stoppeth one of three.\x00"),
          (b"It pleases you to be kind to what appears to be kitten -- but it\'s not!\x00"),
          (b"It\'s a blatant plug for Ogg Vorbis, http://www.vorbis.com/\x00"),
          (b"It\'s a business plan for a new startup, kitten.net.\x00"),
          (b"It\'s a revised business plan for a new startup, my.kitten.net.\x00"),
          (b"It\'s a square.\x00"),
          (b"It seems to be a copy of \"A Tail of Two Kitties\".\x00"),
          (b"It\'s the Donation of Constantine!\x00"),
          (b"It\'s this message, nothing more.\x00"),
          (b"Lysine, an essential amino acid.  Well, maybe not for robots.\x00"),
          (b"No kitten here.\x00"),
          (b"The score for a Czech composer\'s \"Kitten-Finding Symphony in C\".\x00"),
          (b"This looks like Bradley\'s \"Appearance and Reality\", but it\'s really not.\x00"),
          (b"This non-kitten item no verb.\x00"),
          (b"You feel strangely unfulfilled.\x00"),
          (b"You hit the non-kitten item.  The non-kitten item fails to yowl.\x00"),
          (b"You suddenly yearn for your distant homeland.\x00"),
          (b"You\'ve found the snows of yesteryear!  So that\'s where they all went to.\x00"),
          (b"Approaching.  One car.  J.  Followed by.  Two car.  M, M.  In five. Minutes.\x00"),
          (b"Free Jon Johansen!\x00"),
          (b"Free Dmitry Sklyarov!\x00"),
          (b"One person shouts \"What do we want?\" The crowd answers \"Free Dmitry!\"\x00"),
          (b"Judith Platt insults librarians.\x00"),
          (b"This map is not the territory.\x00"),
          (b"\"Go back to Libraria!\", says Pat Schroeder.\x00"),
          (b"This is a porcelain kitten-counter.  0, 0, 0, 0, 0...\x00"),
          (b"An old bootable business card, unfortunately cracked down the middle.\x00"),
          (b"A kitten sink, for washing kitten (if only kitten liked water).\x00"),
          (b"A kitten source (to match the kitten sink).\x00"),
          (b"If it\'s one thing, it\'s not another.\x00"),
          (b"If it\'s not one thing, it\'s another.\x00"),
          (b"A caboodle.\x00"),
          (b"A grin.\x00"),
          (b"A hedgehog.  It looks like it knows something important.\x00"),
          (b"You\'ve found... Oh wait, that\'s just a cat.\x00"),
          (b"Robot should not be touching that.\x00"),
          (b"Air Guitar!!!  NA na NA na!!\x00"),
          (b"An aromatherapy candle burns with healing light.\x00"),
          (b"You find a bright shiny penny.\x00"),
          (b"It\'s a free Jon Johansen!\x00"),
          (b"It\'s a free Dmitry Sklyarov!\x00"),
          (b"The rothe hits!  The rothe hits!\x00"),
          (b"It\'s an Internet chain letter about sodium laureth sulfate.\x00"),
          (b"Ed Witten sits here, pondering string theory.\x00"),
          (b"Something is written here in the dust.  You read: \"rJbotf ndQkttten\".\x00"),
          (b"We wish you a merry kitten, and a happy New Year!\x00"),
          (b"Run away!  Run away!\x00"),
          (b"You can see right through this copy of Brin\'s \"Transparent Society\".\x00"),
          (b"This copy of \"Steal This Book\" has been stolen from a bookstore.\x00"),
          (b"It\'s Roya Naini.\x00"),
          (b"This kit is the fourteenth in a series of kits named with Roman letters.\x00"),
          (b"This is the tenth key you\'ve found so far.\x00"),
          (b"You find a fraud scheme in which loans are used as security for other loans.\x00"),
          (b"It\'s the phrase \"and her\", written in ancient Greek.\x00"),
          (b"It\'s the author of \"Randomness and Mathematical Proof\".\x00"),
          (b"It\'s the crusty exoskeleton of an arthropod!\x00"),
          (b"It\'s Emporer Shaddam the 4th\'s planet!\x00"),
          (b"It\'s the triangle leg adjacent to an angle divided by the leg opposite it.\x00"),
          (b"It\'s a bottle of nail polish remover.\x00"),
          (b"You found netkit! Way to go, robot!\x00"),
          (b"It\'s the ASCII Floating Head of Seth David Schoen!\x00"),
          (b"A frosted pink party-cake, half eaten.\x00"),
          (b"A bitchin\' homemade tesla coil.\x00"),
          (b"Conan O\'Brian, sans jawbone.\x00"),
          (b"It\'s either a mirror, or another soulless kitten-seeking robot.\x00"),
          (b"Preoccupation with finding kitten prevents you from investigating further.\x00"),
          (b"Fonzie sits here, mumbling incoherently about a shark and a pair of waterskis.\x00"),
          (b"The ghost of your dance instructor, his face a paper-white mask of evil.\x00"),
          (b"A bag of groceries taken off the shelf before the expiration date.\x00"),
          (b"A book: Feng Shui, Zen: the art of randomly arranging items that are not kitten.\x00"),
          (b"This might be the fountain of youth, but you\'ll never know.\x00"),
          (b"Tigerbot Hesh.\x00"),
          (b"Stimutacs.\x00"),
          (b"A canister of pressurized whipped cream, sans whipped cream.\x00"),
          (b"The non-kitten item bites!\x00"),
          (b"A chain hanging from two posts reminds you of the Gateway Arch.\x00"),
          (b"A mathematician calculates the halting probability of a Turing machine.\x00"),
          (b"A number of short theatrical productions are indexed 1, 2, 3, ... n.\x00"),
          (b"A technical university in Australia.\x00"),
          (b"It is -- I just feel something wonderful is about to happen.\x00"),
          (b"It\'s a Cat 5 cable.\x00"),
          (b"It\'s a U.S. president.\x00"),
          (b"It\'s a piece of cloth used to cover a stage in between performances.\x00"),
          (b"The ionosphere seems charged with meaning.\x00"),
          (b"This tomography is like, hella axial, man!\x00"),
          (b"It\'s your favorite game -- robotfindscatan!\x00"),
          (b"Just a man selling an albatross.\x00"),
          (b"The intermission from a 1930s silent movie.\x00"),
          (b"It\'s an inverted billiard ball!\x00"),
          (b"The spectre of Sherlock Holmes wills you onwards.\x00")
      ];

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
    screen: Box<[Box<[i32]>]>,
}

impl State {
    #[no_mangle]
    pub extern "C" fn message(&mut self, mut message: &CStr) {
        (::wrap::wmove)((1i32), (0i32));
        (::wrap::wclrtoeol)();
        unsafe {
            mvprintw!(
                1i32,
                0i32,
                "{:.*}\u{0}",
                ::wrap::cols() as usize,
                (CStr::from_ptr((message.as_ptr()) as *mut i8))
                    .to_str()
                    .unwrap()
            );
        }
        (::wrap::wmove)((((*(self)).robot).y), (((*(self)).robot).x));
        (::wrap::wrefresh)();
    }
    #[no_mangle]
    pub extern "C" fn play_game(&mut self) {
        let mut old_x: i32 = ((*(self)).robot).x;
        let mut old_y: i32 = ((*(self)).robot).y;
        let mut input: i32 = ((::wrap::wgetch)());
        while (input != 27i32 && (input != b'q' as (i32)) && (input != b'Q' as (i32))) {
            ((self)).process_input((input));
            if !(old_x == ((*(self)).robot).x && (old_y == ((*(self)).robot).y)) {
                if ((::wrap::wmove)((old_y), (old_x))) == -1i32 {
                    -1i32;
                } else {
                    (::wrap::waddch)((b' ' as (usize)));
                }
                (((*(self)).screen)[(old_x as (isize)) as usize][(old_y as (isize)) as usize]) =
                    -1i32;
                draw(((*(self)).robot));
                (::wrap::wrefresh)();
                (((*(self)).screen)[(((*(self)).robot).x as (isize)) as usize][(((*(self)).robot)
                                                                                    .y as
                                                                                    (isize)) as
                                                                                   usize]) = 0i32;
                old_x = ((*(self)).robot).x;
                old_y = ((*(self)).robot).y;
            }
            input = ((::wrap::wgetch)());
        }
        ((self)).message((((CStr::from_bytes_with_nul((b"Bye!\0")).unwrap()))));
        (::wrap::wrefresh)();
        finish(0i32);
    }
    #[no_mangle]
    pub extern "C" fn process_input(&mut self, mut input: i32) {
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
                    (((CStr::from_bytes_with_nul(
                        (b"Invalid input: Use direction keys or Esc.\0"),
                    ).unwrap()))),
                );
                return;
            }
        }
        if check_y < 3i32 || check_y > (::wrap::lines()) - 1i32 || check_x < 0i32 ||
            check_x > (::wrap::cols()) - 1i32
        {
        } else if (((*(self)).screen)[(check_x as (isize)) as usize][(check_y as (isize)) as
                                                                         usize]) !=
                   -1i32
        {
            let switch2 = (((*(self)).screen)[(check_x as (isize)) as usize][(check_y as (isize)) as
                                                                                 usize]);
            if !(switch2 == 0i32) {
                if switch2 == 1i32 {
                    (::wrap::wmove)((1i32), (0i32));
                    (::wrap::wclrtoeol)();
                    ((self)).play_animation((input));
                } else {
                    let index =
                        ((*(self)).bogus_messages)[((((*(self)).screen)[(check_x as (isize)) as
                                                                            usize]
                                                         [(check_y as (isize)) as usize]) -
                                                        2i32) as
                                                       (usize)] as (usize);
                    ((self)).message(
                        ((CStr::from_bytes_with_nul((((messages)[(index)]))).unwrap())),
                    );
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
    pub extern "C" fn play_animation(&mut self, mut input: i32) {
        let mut counter: i32 = (4i32);
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
    pub extern "C" fn instructions(&mut self) {
        unsafe {
            mvprintw!(
                0i32,
                0i32,
                "robotfindskitten v{:}\n\u{0}",
                CStr::from_ptr(ver as *mut i8).to_str().unwrap()
            );
            printw!("By the illustrious Leonard Richardson (C) 1997, 2000\n\u{0}");
            printw!("Written originally for the Nerth Pork robotfindskitten contest\n\n\u{0}");
            printw!("In this game, you are robot (\u{0}");
            draw_in_place(((*(self)).robot));
            printw!("). Your job is to find kitten. This task\n\u{0}");
            printw!(
                "is complicated by the existence of various things which are not kitten.\n\u{0}"
            );
            printw!(
                "Robot must touch items to determine if they are kitten or not. The game\n\u{0}"
            );
            printw!(
                "ends when robotfindskitten. Alternatively, you may end the game by hitting\n\u{0}"
            );
            printw!("the Esc key. See the documentation for more information.\n\n\u{0}");
            printw!("Press any key to start.\n\u{0}");
            (::wrap::wrefresh)();
            let mut dummy: u8 = (((::wrap::wgetch)()) as (u8));
            (::wrap::wclear)();
        }
    }
    #[no_mangle]
    pub extern "C" fn initialize_arrays(&mut self) {
        let mut i: i32 = 0i32;
        ((*(self)).screen) = (0..(::wrap::cols()))
            .map(|_| {
                (0..(::wrap::lines()))
                    .map(|_| 0)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let mut empty: screen_object = (::screen_object {
                                            x: (-1i32),
                                            y: (-1i32),
                                            color: (0i32),
                                            bold: (false),
                                            character: (b' '),
                                        });
        let mut counter: i32 = (0i32);
        while (counter <= (::wrap::cols()) - 1i32) {

            let mut counter2: i32 = (0i32);
            while (counter2 <= (::wrap::lines()) - 1i32) {
                (((*(self)).screen)[(counter as (isize)) as usize][(counter2 as (isize)) as
                                                                       usize]) = -1i32;
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
    pub extern "C" fn initialize_robot(&mut self) {
        (((*(self)).robot)) = (::screen_object {
                                   x: ((::wrap::rand)() % ((::wrap::cols()) - 1i32) + 1i32),
                                   y: ((::wrap::rand)() % ((::wrap::lines()) - 1i32 - 3i32 + 1i32) +
                                           3i32),
                                   character: (b'#'),
                                   color: (0i32),
                                   bold: (false),
                                   ..((*(self)).robot)
                               });
        (((*(self)).screen)[(((*(self)).robot).x as (isize)) as usize][(((*(self)).robot).y as
                                                                            (isize)) as
                                                                           usize]) = 0i32;
    }
    #[no_mangle]
    pub extern "C" fn initialize_kitten(&mut self) {
        loop {
            (((*(self)).kitten)) =
                (::screen_object {
                     x: ((::wrap::rand)() % ((::wrap::cols()) - 1i32) + 1i32),
                     y: ((::wrap::rand)() % ((::wrap::lines()) - 1i32 - 3i32 + 1i32) + 3i32),
                     ..((*(self)).kitten)
                 });
            if !((((*(self)).screen)[(((*(self)).kitten).x as (isize)) as usize][(((*(self))
                                                                                       .kitten)
                                                                                      .y as
                                                                                      (isize)) as
                                                                                     usize]) !=
                     -1i32)
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
        (((*(self)).screen)[(((*(self)).kitten).x as (isize)) as usize][(((*(self)).kitten).y as
                                                                             (isize)) as
                                                                            usize]) = 1i32;
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
    pub extern "C" fn initialize_bogus(&mut self) {
        for counter in ((0i32)..(((*(self)).num_bogus))) {
            let mut index: i32 = ((0i32));
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
                         x: ((::wrap::rand)() % ((::wrap::cols()) - 1i32) + 1i32),
                         y: ((::wrap::rand)() % ((::wrap::lines()) - 1i32 - 3i32 + 1i32) + 3i32),
                         ..(((*(self)).bogus)[counter as (usize)])
                     });
                if !((((*(self)).screen)[(((*(self)).bogus)[counter as (usize)].x as (isize)) as
                                             usize]
                          [(((*(self)).bogus)[counter as (usize)].y as (isize)) as usize]) !=
                         -1i32)
                {
                    break;
                }
            }
            (((*(self)).screen)[(((*(self)).bogus)[counter as (usize)].x as (isize)) as usize]
                 [(((*(self)).bogus)[counter as (usize)].y as (isize)) as usize]) = counter + 2i32;
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
    pub extern "C" fn initialize_screen(&mut self) {
        unsafe {
            mvprintw!(
                0i32,
                0i32,
                "robotfindskitten v{:}\n\n\u{0}",
                CStr::from_ptr(ver as *mut i8).to_str().unwrap()
            );
            let mut counter: i32 = (0i32);
            while (counter <= (::wrap::cols()) - 1i32) {
                printw!("{:}\u{0}", ((95i32) as u8 as char));
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


    let mut old: usize = ((0usize));
    let mut dummy: i16 = ((0i16));

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
    let mut new: usize = (o.color as (usize) << 0i32 + 8i32);
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
        printw!("{:}\u{0}", ((o.character as i32) as u8 as char));
    } else {
        mvprintw!(o.y, o.x, "{:}\u{0}", ((o.character as i32) as u8 as char));
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
pub extern "C" fn finish(mut sig: i32) {
    // NB: this function is called as a signal handler.
    (::wrap::endwin)();
    unsafe {
        printf!(
            "{:}{:}{:}\u{0}",
            ((27i32) as u8 as char),
            ((b'(' as i32) as u8 as char),
            ((b'B' as i32) as u8 as char)
        );
    }
    (::wrap::exit)(0i32);
}

#[no_mangle]
pub extern "C" fn validchar(mut a: u8) -> i32 {
    if a as (i32) == 127i32 || a as (i32) == b' ' as (i32) || a as (i32) == b'#' as (i32) {
        0i32
    } else {
        1i32
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
    let mut S = State {
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
        screen: Box::new([]),
    };

    if argc == 1i32 {
        (S.num_bogus) = 20i32;
    } else {
        (S.num_bogus) = (::wrap::atoi)(
            (CStr::from_ptr((*argv.offset(1isize) as (*const u8)) as *const c_char)),
        );
        if (S.num_bogus) < 0i32 || (S.num_bogus) > 406i32 {
            printf!(
                "Run-time parameter must be between 0 and {:}.\n\u{0}",
                406i32 as i32
            );
            (::wrap::exit)(0i32);
        }
    }
    (::wrap::srand)((::wrap::time)() as (u32));
    printf!(
        "{:}{:}{:}\u{0}",
        ((27i32) as u8 as char),
        ((b'(' as i32) as u8 as char),
        ((b'U' as i32) as u8 as char)
    );
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
