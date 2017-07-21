
extern "C" {
    fn calloc(__nmemb: u64, __size: u64) -> *mut ::std::os::raw::c_void;
    fn free(__ptr: *mut ::std::os::raw::c_void);
    fn json_c_get_random_seed() -> i32;
    fn lh_abort(msg: *const u8, ...);
    fn strcmp(__s1: *const u8, __s2: *const u8) -> i32;
    fn strlen(__s: *const u8) -> u64;
}

#[no_mangle]
pub unsafe extern "C" fn lh_ptr_hash(mut k: *const ::std::os::raw::c_void) -> u64 {
    (k as (i64) as (u64)).wrapping_mul(0x9e370001u64) >> 4i32 &
        0x7fffffffffffffffu64.wrapping_mul(2u64).wrapping_add(1u64)
}

#[no_mangle]
pub unsafe extern "C" fn lh_ptr_equal(
    mut k1: *const ::std::os::raw::c_void,
    mut k2: *const ::std::os::raw::c_void,
) -> i32 {
    (k1 == k2) as (i32)
}

#[derive(Copy)]
#[repr(C)]
pub union Union1 {
    pub ptr : *const ::std::os::raw::c_void,
    pub i : u64,
}

impl Clone for Union1 {
    fn clone(&self) -> Self {
        *self
    }
}

unsafe extern "C" fn hashlittle(
    mut key: *const ::std::os::raw::c_void,
    mut length: u64,
    mut initval: u32,
) -> u32 {
    let mut _currentBlock;
    let mut a: u32;
    let mut b: u32;
    let mut c: u32;
    let mut u: Union1 = ::std::mem::uninitialized();
    a = {
        b = {
            c = 0xdeadbeefu32.wrapping_add(length as (u32)).wrapping_add(
                initval,
            );
            c
        };
        b
    };
    u.ptr = key;
    if true && (u.i & 0x3u64 == 0u64) {
        let mut k: *const u32 = key as (*const u32);
        'loop62: loop {
            if !(length > 12u64) {
                break;
            }
            a = a.wrapping_add(*k.offset(0isize));
            b = b.wrapping_add(*k.offset(1isize));
            c = c.wrapping_add(*k.offset(2isize));
            a = a.wrapping_sub(c);
            a = a ^ (c << 4i32 | c >> 32i32 - 4i32);
            c = c.wrapping_add(b);
            b = b.wrapping_sub(a);
            b = b ^ (a << 6i32 | a >> 32i32 - 6i32);
            a = a.wrapping_add(c);
            c = c.wrapping_sub(b);
            c = c ^ (b << 8i32 | b >> 32i32 - 8i32);
            b = b.wrapping_add(a);
            a = a.wrapping_sub(c);
            a = a ^ (c << 16i32 | c >> 32i32 - 16i32);
            c = c.wrapping_add(b);
            b = b.wrapping_sub(a);
            b = b ^ (a << 19i32 | a >> 32i32 - 19i32);
            a = a.wrapping_add(c);
            c = c.wrapping_sub(b);
            c = c ^ (b << 4i32 | b >> 32i32 - 4i32);
            b = b.wrapping_add(a);
            length = length.wrapping_sub(12u64);
            k = k.offset(3isize);
        }
        if length == 0u64 {
            return c;
        } else if length == 1u64 {
            a = a.wrapping_add(*k.offset(0isize) & 0xffu32);
        } else if length == 2u64 {
            a = a.wrapping_add(*k.offset(0isize) & 0xffffu32);
        } else if length == 3u64 {
            a = a.wrapping_add(*k.offset(0isize) & 0xffffffu32);
        } else if length == 4u64 {
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 5u64 {
            b = b.wrapping_add(*k.offset(1isize) & 0xffu32);
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 6u64 {
            b = b.wrapping_add(*k.offset(1isize) & 0xffffu32);
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 7u64 {
            b = b.wrapping_add(*k.offset(1isize) & 0xffffffu32);
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 8u64 {
            b = b.wrapping_add(*k.offset(1isize));
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 9u64 {
            c = c.wrapping_add(*k.offset(2isize) & 0xffu32);
            b = b.wrapping_add(*k.offset(1isize));
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 10u64 {
            c = c.wrapping_add(*k.offset(2isize) & 0xffffu32);
            b = b.wrapping_add(*k.offset(1isize));
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 11u64 {
            c = c.wrapping_add(*k.offset(2isize) & 0xffffffu32);
            b = b.wrapping_add(*k.offset(1isize));
            a = a.wrapping_add(*k.offset(0isize));
        } else if length == 12u64 {
            c = c.wrapping_add(*k.offset(2isize));
            b = b.wrapping_add(*k.offset(1isize));
            a = a.wrapping_add(*k.offset(0isize));
        }
    } else if true && (u.i & 0x1u64 == 0u64) {
        let mut k: *const u16 = key as (*const u16);
        let mut k8: *const u8;
        'loop32: loop {
            if !(length > 12u64) {
                break;
            }
            a = a.wrapping_add((*k.offset(0isize) as (u32)).wrapping_add(
                *k.offset(1isize) as (u32) <<
                    16i32,
            ));
            b = b.wrapping_add((*k.offset(2isize) as (u32)).wrapping_add(
                *k.offset(3isize) as (u32) <<
                    16i32,
            ));
            c = c.wrapping_add((*k.offset(4isize) as (u32)).wrapping_add(
                *k.offset(5isize) as (u32) <<
                    16i32,
            ));
            a = a.wrapping_sub(c);
            a = a ^ (c << 4i32 | c >> 32i32 - 4i32);
            c = c.wrapping_add(b);
            b = b.wrapping_sub(a);
            b = b ^ (a << 6i32 | a >> 32i32 - 6i32);
            a = a.wrapping_add(c);
            c = c.wrapping_sub(b);
            c = c ^ (b << 8i32 | b >> 32i32 - 8i32);
            b = b.wrapping_add(a);
            a = a.wrapping_sub(c);
            a = a ^ (c << 16i32 | c >> 32i32 - 16i32);
            c = c.wrapping_add(b);
            b = b.wrapping_sub(a);
            b = b ^ (a << 19i32 | a >> 32i32 - 19i32);
            a = a.wrapping_add(c);
            c = c.wrapping_sub(b);
            c = c ^ (b << 4i32 | b >> 32i32 - 4i32);
            b = b.wrapping_add(a);
            length = length.wrapping_sub(12u64);
            k = k.offset(6isize);
        }
        k8 = k as (*const u8);
        if length == 0u64 {
            return c;
        } else if length == 1u64 {
            a = a.wrapping_add(*k8.offset(0isize) as (u32));
        } else {
            if length == 2u64 {
                _currentBlock = 57;
            } else if length == 3u64 {
                a = a.wrapping_add(*k8.offset(2isize) as (u32) << 16i32);
                _currentBlock = 57;
            } else {
                if length == 4u64 {
                    _currentBlock = 55;
                } else if length == 5u64 {
                    b = b.wrapping_add(*k8.offset(4isize) as (u32));
                    _currentBlock = 55;
                } else {
                    if length == 6u64 {
                        _currentBlock = 53;
                    } else if length == 7u64 {
                        b = b.wrapping_add(*k8.offset(6isize) as (u32) << 16i32);
                        _currentBlock = 53;
                    } else {
                        if length == 8u64 {
                            _currentBlock = 51;
                        } else if length == 9u64 {
                            c = c.wrapping_add(*k8.offset(8isize) as (u32));
                            _currentBlock = 51;
                        } else {
                            if length == 10u64 {
                                _currentBlock = 49;
                            } else if length == 11u64 {
                                c = c.wrapping_add(*k8.offset(10isize) as (u32) << 16i32);
                                _currentBlock = 49;
                            } else if length == 12u64 {
                                c = c.wrapping_add((*k.offset(4isize) as (u32)).wrapping_add(
                                    *k.offset(5isize) as (u32) <<
                                        16i32,
                                ));
                                b = b.wrapping_add((*k.offset(2isize) as (u32)).wrapping_add(
                                    *k.offset(3isize) as (u32) <<
                                        16i32,
                                ));
                                a = a.wrapping_add((*k.offset(0isize) as (u32)).wrapping_add(
                                    *k.offset(1isize) as (u32) <<
                                        16i32,
                                ));
                                _currentBlock = 88;
                            } else {
                                _currentBlock = 88;
                            }
                            if _currentBlock == 88 {
                            } else {
                                c = c.wrapping_add(*k.offset(4isize) as (u32));
                                b = b.wrapping_add((*k.offset(2isize) as (u32)).wrapping_add(
                                    *k.offset(3isize) as (u32) <<
                                        16i32,
                                ));
                                a = a.wrapping_add((*k.offset(0isize) as (u32)).wrapping_add(
                                    *k.offset(1isize) as (u32) <<
                                        16i32,
                                ));
                                _currentBlock = 88;
                            }
                        }
                        if _currentBlock == 88 {
                        } else {
                            b = b.wrapping_add((*k.offset(2isize) as (u32)).wrapping_add(
                                *k.offset(3isize) as (u32) <<
                                    16i32,
                            ));
                            a = a.wrapping_add((*k.offset(0isize) as (u32)).wrapping_add(
                                *k.offset(1isize) as (u32) <<
                                    16i32,
                            ));
                            _currentBlock = 88;
                        }
                    }
                    if _currentBlock == 88 {
                    } else {
                        b = b.wrapping_add(*k.offset(2isize) as (u32));
                        a = a.wrapping_add((*k.offset(0isize) as (u32)).wrapping_add(
                            *k.offset(1isize) as (u32) <<
                                16i32,
                        ));
                        _currentBlock = 88;
                    }
                }
                if _currentBlock == 88 {
                } else {
                    a = a.wrapping_add((*k.offset(0isize) as (u32)).wrapping_add(
                        *k.offset(1isize) as (u32) <<
                            16i32,
                    ));
                    _currentBlock = 88;
                }
            }
            if _currentBlock == 88 {
            } else {
                a = a.wrapping_add(*k.offset(0isize) as (u32));
            }
        }
    } else {
        let mut k: *const u8 = key as (*const u8);
        'loop3: loop {
            if !(length > 12u64) {
                break;
            }
            a = a.wrapping_add(*k.offset(0isize) as (u32));
            a = a.wrapping_add(*k.offset(1isize) as (u32) << 8i32);
            a = a.wrapping_add(*k.offset(2isize) as (u32) << 16i32);
            a = a.wrapping_add(*k.offset(3isize) as (u32) << 24i32);
            b = b.wrapping_add(*k.offset(4isize) as (u32));
            b = b.wrapping_add(*k.offset(5isize) as (u32) << 8i32);
            b = b.wrapping_add(*k.offset(6isize) as (u32) << 16i32);
            b = b.wrapping_add(*k.offset(7isize) as (u32) << 24i32);
            c = c.wrapping_add(*k.offset(8isize) as (u32));
            c = c.wrapping_add(*k.offset(9isize) as (u32) << 8i32);
            c = c.wrapping_add(*k.offset(10isize) as (u32) << 16i32);
            c = c.wrapping_add(*k.offset(11isize) as (u32) << 24i32);
            a = a.wrapping_sub(c);
            a = a ^ (c << 4i32 | c >> 32i32 - 4i32);
            c = c.wrapping_add(b);
            b = b.wrapping_sub(a);
            b = b ^ (a << 6i32 | a >> 32i32 - 6i32);
            a = a.wrapping_add(c);
            c = c.wrapping_sub(b);
            c = c ^ (b << 8i32 | b >> 32i32 - 8i32);
            b = b.wrapping_add(a);
            a = a.wrapping_sub(c);
            a = a ^ (c << 16i32 | c >> 32i32 - 16i32);
            c = c.wrapping_add(b);
            b = b.wrapping_sub(a);
            b = b ^ (a << 19i32 | a >> 32i32 - 19i32);
            a = a.wrapping_add(c);
            c = c.wrapping_sub(b);
            c = c ^ (b << 4i32 | b >> 32i32 - 4i32);
            b = b.wrapping_add(a);
            length = length.wrapping_sub(12u64);
            k = k.offset(12isize);
        }
        if length == 0u64 {
            return c;
        } else {
            if length == 1u64 {
                _currentBlock = 28;
            } else {
                if length == 2u64 {
                    _currentBlock = 27;
                } else {
                    if length == 3u64 {
                        _currentBlock = 26;
                    } else {
                        if length == 4u64 {
                            _currentBlock = 25;
                        } else {
                            if length == 5u64 {
                                _currentBlock = 24;
                            } else {
                                if length == 6u64 {
                                    _currentBlock = 23;
                                } else {
                                    if length == 7u64 {
                                        _currentBlock = 22;
                                    } else {
                                        if length == 8u64 {
                                            _currentBlock = 21;
                                        } else {
                                            if length == 9u64 {
                                                _currentBlock = 20;
                                            } else {
                                                if length == 10u64 {
                                                    _currentBlock = 19;
                                                } else {
                                                    if length == 11u64 {
                                                        _currentBlock = 18;
                                                    } else if length == 12u64 {
                                                        c = c.wrapping_add(
                                                            *k.offset(11isize) as (u32) << 24i32,
                                                        );
                                                        _currentBlock = 18;
                                                    } else {
                                                        _currentBlock = 88;
                                                    }
                                                    if _currentBlock == 88 {
                                                    } else {
                                                        c = c.wrapping_add(
                                                            *k.offset(10isize) as (u32) << 16i32,
                                                        );
                                                        _currentBlock = 19;
                                                    }
                                                }
                                                if _currentBlock == 88 {
                                                } else {
                                                    c = c.wrapping_add(
                                                        *k.offset(9isize) as (u32) << 8i32,
                                                    );
                                                    _currentBlock = 20;
                                                }
                                            }
                                            if _currentBlock == 88 {
                                            } else {
                                                c = c.wrapping_add(*k.offset(8isize) as (u32));
                                                _currentBlock = 21;
                                            }
                                        }
                                        if _currentBlock == 88 {
                                        } else {
                                            b = b.wrapping_add(*k.offset(7isize) as (u32) << 24i32);
                                            _currentBlock = 22;
                                        }
                                    }
                                    if _currentBlock == 88 {
                                    } else {
                                        b = b.wrapping_add(*k.offset(6isize) as (u32) << 16i32);
                                        _currentBlock = 23;
                                    }
                                }
                                if _currentBlock == 88 {
                                } else {
                                    b = b.wrapping_add(*k.offset(5isize) as (u32) << 8i32);
                                    _currentBlock = 24;
                                }
                            }
                            if _currentBlock == 88 {
                            } else {
                                b = b.wrapping_add(*k.offset(4isize) as (u32));
                                _currentBlock = 25;
                            }
                        }
                        if _currentBlock == 88 {
                        } else {
                            a = a.wrapping_add(*k.offset(3isize) as (u32) << 24i32);
                            _currentBlock = 26;
                        }
                    }
                    if _currentBlock == 88 {
                    } else {
                        a = a.wrapping_add(*k.offset(2isize) as (u32) << 16i32);
                        _currentBlock = 27;
                    }
                }
                if _currentBlock == 88 {
                } else {
                    a = a.wrapping_add(*k.offset(1isize) as (u32) << 8i32);
                    _currentBlock = 28;
                }
            }
            if _currentBlock == 88 {
            } else {
                a = a.wrapping_add(*k.offset(0isize) as (u32));
            }
        }
    }
    c = c ^ b;
    c = c.wrapping_sub(b << 14i32 | b >> 32i32 - 14i32);
    a = a ^ c;
    a = a.wrapping_sub(c << 11i32 | c >> 32i32 - 11i32);
    b = b ^ a;
    b = b.wrapping_sub(a << 25i32 | a >> 32i32 - 25i32);
    c = c ^ b;
    c = c.wrapping_sub(b << 16i32 | b >> 32i32 - 16i32);
    a = a ^ c;
    a = a.wrapping_sub(c << 4i32 | c >> 32i32 - 4i32);
    b = b ^ a;
    b = b.wrapping_sub(a << 14i32 | a >> 32i32 - 14i32);
    c = c ^ b;
    c = c.wrapping_sub(b << 24i32 | b >> 32i32 - 24i32);
    c
}

#[no_mangle]
pub unsafe extern "C" fn lh_char_hash(mut k: *const ::std::os::raw::c_void) -> u64 {
    static mut random_seed: i32 = -1i32;
    if random_seed == -1i32 {
        let mut seed: i32;
        'loop2: loop {
            if !({
                     seed = json_c_get_random_seed();
                     seed
                 } == -1i32)
            {
                break;
            }
        }
        random_seed = seed;
    }
    hashlittle(
        k as (*const u8) as (*const ::std::os::raw::c_void),
        strlen(k as (*const u8)),
        random_seed as (u32),
    ) as (u64)
}

#[no_mangle]
pub unsafe extern "C" fn lh_char_equal(
    mut k1: *const ::std::os::raw::c_void,
    mut k2: *const ::std::os::raw::c_void,
) -> i32 {
    (strcmp(k1 as (*const u8), k2 as (*const u8)) == 0i32) as (i32)
}

#[derive(Copy)]
#[repr(C)]
pub struct lh_entry {
    pub k: *mut ::std::os::raw::c_void,
    pub v: *const ::std::os::raw::c_void,
    pub next: *mut lh_entry,
    pub prev: *mut lh_entry,
}

impl Clone for lh_entry {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct lh_table {
    pub size: i32,
    pub count: i32,
    pub collisions: i32,
    pub resizes: i32,
    pub lookups: i32,
    pub inserts: i32,
    pub deletes: i32,
    pub name: *const u8,
    pub head: *mut lh_entry,
    pub tail: *mut lh_entry,
    pub table: *mut lh_entry,
    pub free_fn: unsafe extern "C" fn(*mut lh_entry),
    pub hash_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void) -> u64,
    pub equal_fn:
        unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void) -> i32,
}

impl Clone for lh_table {
    fn clone(&self) -> Self {
        *self
    }
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_new(
    mut size: i32,
    mut name: *const u8,
    mut free_fn: unsafe extern "C" fn(*mut lh_entry),
    mut hash_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void) -> u64,
    mut equal_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void)
                                       -> i32,
) -> *mut lh_table {
    let mut i: i32;
    let mut t: *mut lh_table;
    t = calloc(1u64, ::std::mem::size_of::<lh_table>() as (u64)) as (*mut lh_table);
    if t.is_null() {
        lh_abort((*b"lh_table_new: calloc failed\n\0").as_ptr());
    }
    (*t).count = 0i32;
    (*t).size = size;
    (*t).name = name;
    (*t).table = calloc(size as (u64), ::std::mem::size_of::<lh_entry>() as (u64)) as
        (*mut lh_entry);
    if (*t).table.is_null() {
        lh_abort((*b"lh_table_new: calloc failed\n\0").as_ptr());
    }
    (*t).free_fn = free_fn;
    (*t).hash_fn = hash_fn;
    (*t).equal_fn = equal_fn;
    i = 0i32;
    'loop5: loop {
        if !(i < size) {
            break;
        }
        (*(*t).table.offset(i as (isize))).k = -1i32 as (*mut ::std::os::raw::c_void);
        i = i + 1;
    }
    t
}

#[no_mangle]
pub unsafe extern "C" fn lh_kchar_table_new(
    mut size: i32,
    mut name: *const u8,
    mut free_fn: unsafe extern "C" fn(*mut lh_entry),
) -> *mut lh_table {
    lh_table_new(size, name, free_fn, lh_char_hash, lh_char_equal)
}

#[no_mangle]
pub unsafe extern "C" fn lh_kptr_table_new(
    mut size: i32,
    mut name: *const u8,
    mut free_fn: unsafe extern "C" fn(*mut lh_entry),
) -> *mut lh_table {
    lh_table_new(size, name, free_fn, lh_ptr_hash, lh_ptr_equal)
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_resize(mut t: *mut lh_table, mut new_size: i32) {
    let mut new_t: *mut lh_table;
    let mut ent: *mut lh_entry;
    new_t = lh_table_new(
        new_size,
        (*t).name,
        ::std::mem::transmute(0_usize),
        (*t).hash_fn,
        (*t).equal_fn,
    );
    ent = (*t).head;
    'loop1: loop {
        if ent.is_null() {
            break;
        }
        lh_table_insert(new_t, (*ent).k, (*ent).v);
        ent = (*ent).next as (*mut lh_entry);
    }
    free((*t).table as (*mut ::std::os::raw::c_void));
    (*t).table = (*new_t).table;
    (*t).size = new_size;
    (*t).head = (*new_t).head;
    (*t).tail = (*new_t).tail;
    (*t).resizes = (*t).resizes + 1;
    free(new_t as (*mut ::std::os::raw::c_void));
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_free(mut t: *mut lh_table) {
    let mut c: *mut lh_entry;
    c = (*t).head;
    'loop1: loop {
        if !(c != 0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry)) {
            break;
        }
        if (*t).free_fn as usize != 0 {
            ((*t).free_fn)(c as (*mut lh_entry));
        }
        c = (*c).next as (*mut lh_entry);
    }
    free((*t).table as (*mut ::std::os::raw::c_void));
    free(t as (*mut ::std::os::raw::c_void));
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_insert(
    mut t: *mut lh_table,
    mut k: *mut ::std::os::raw::c_void,
    mut v: *const ::std::os::raw::c_void,
) -> i32 {
    let mut h: u64;
    let mut n: u64;
    (*t).inserts = (*t).inserts + 1;
    if (*t).count as (f64) >= (*t).size as (f64) * 0.66f64 {
        lh_table_resize(t, (*t).size * 2i32);
    }
    h = ((*t).hash_fn)(k as (*const ::std::os::raw::c_void));
    n = h.wrapping_rem((*t).size as (u64));
    'loop3: loop {
        if (*(*t).table.offset(n as (isize))).k == -1i32 as (*mut ::std::os::raw::c_void) ||
            (*(*t).table.offset(n as (isize))).k == -2i32 as (*mut ::std::os::raw::c_void)
        {
            break;
        }
        (*t).collisions = (*t).collisions + 1;
        if !({
                 n = n.wrapping_add(1u64);
                 n
             } as (i32) == (*t).size)
        {
            continue;
        }
        n = 0u64;
    }
    (*(*t).table.offset(n as (isize))).k = k;
    (*(*t).table.offset(n as (isize))).v = v;
    (*t).count = (*t).count + 1;
    if (*t).head == 0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry) {
        (*t).head = {
            (*t).tail = &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry);
            (*t).tail
        };
        (*(*t).table.offset(n as (isize))).next = {
            let _rhs = 0i32 as (*mut ::std::os::raw::c_void);
            let _lhs = &mut (*(*t).table.offset(n as (isize))).prev;
            *_lhs = _rhs as (*mut lh_entry);
            *_lhs
        };
    } else {
        (*(*t).tail).next = &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry) as
            (*mut lh_entry);
        (*(*t).table.offset(n as (isize))).prev = (*t).tail as (*mut lh_entry);
        (*(*t).table.offset(n as (isize))).next = 0i32 as (*mut ::std::os::raw::c_void) as
            (*mut lh_entry);
        (*t).tail = &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry);
    }
    0i32
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_lookup_entry(
    mut t: *mut lh_table,
    mut k: *const ::std::os::raw::c_void,
) -> *mut lh_entry {
    let mut _currentBlock;
    let mut h: u64 = ((*t).hash_fn)(k);
    let mut n: u64 = h.wrapping_rem((*t).size as (u64));
    let mut count: i32 = 0i32;
    (*t).lookups = (*t).lookups + 1;
    'loop1: loop {
        if !(count < (*t).size) {
            _currentBlock = 2;
            break;
        }
        if (*(*t).table.offset(n as (isize))).k == -1i32 as (*mut ::std::os::raw::c_void) {
            _currentBlock = 9;
            break;
        }
        if (*(*t).table.offset(n as (isize))).k != -2i32 as (*mut ::std::os::raw::c_void) &&
            (((*t).equal_fn)(
                (*(*t).table.offset(n as (isize))).k as (*const ::std::os::raw::c_void),
                k,
            ) != 0)
        {
            _currentBlock = 8;
            break;
        }
        if {
            n = n.wrapping_add(1u64);
            n
        } as (i32) == (*t).size
        {
            n = 0u64;
        }
        count = count + 1;
    }
    if _currentBlock == 2 {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry)
    } else if _currentBlock == 8 {
        &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry)
    } else {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry)
    }
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_lookup(
    mut t: *mut lh_table,
    mut k: *const ::std::os::raw::c_void,
) -> *const ::std::os::raw::c_void {
    let mut result: *mut ::std::os::raw::c_void = ::std::mem::uninitialized();
    lh_table_lookup_ex(t, k, &mut result as (*mut *mut ::std::os::raw::c_void));
    result as (*const ::std::os::raw::c_void)
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_lookup_ex(
    mut t: *mut lh_table,
    mut k: *const ::std::os::raw::c_void,
    mut v: *mut *mut ::std::os::raw::c_void,
) -> i32 {
    let mut e: *mut lh_entry = lh_table_lookup_entry(t, k);
    if e != 0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry) {
        if v != 0i32 as (*mut ::std::os::raw::c_void) as (*mut *mut ::std::os::raw::c_void) {
            *v = (*e).v as (*mut ::std::os::raw::c_void);
        }
        1i32
    } else {
        if v != 0i32 as (*mut ::std::os::raw::c_void) as (*mut *mut ::std::os::raw::c_void) {
            *v = 0i32 as (*mut ::std::os::raw::c_void);
        }
        0i32
    }
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_delete_entry(mut t: *mut lh_table, mut e: *mut lh_entry) -> i32 {
    let mut n: i64 = ((e as (isize)).wrapping_sub((*t).table as (isize)) /
                          ::std::mem::size_of::<lh_entry>() as (isize)) as
        (i64);
    if n < 0i64 {
        -2i32
    } else if (*(*t).table.offset(n as (isize))).k == -1i32 as (*mut ::std::os::raw::c_void) ||
               (*(*t).table.offset(n as (isize))).k == -2i32 as (*mut ::std::os::raw::c_void)
    {
        -1i32
    } else {
        (*t).count = (*t).count - 1;
        if (*t).free_fn as usize != 0 {
            ((*t).free_fn)(e as (*mut lh_entry));
        }
        (*(*t).table.offset(n as (isize))).v = 0i32 as (*mut ::std::os::raw::c_void) as
            (*const ::std::os::raw::c_void);
        (*(*t).table.offset(n as (isize))).k = -2i32 as (*mut ::std::os::raw::c_void);
        if (*t).tail == &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry) &&
            ((*t).head == &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry))
        {
            (*t).head = {
                (*t).tail = 0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry);
                (*t).tail
            };
        } else if (*t).head == &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry) {
            (*(*(*t).head).next).prev = 0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry);
            (*t).head = (*(*t).head).next as (*mut lh_entry);
        } else if (*t).tail == &mut *(*t).table.offset(n as (isize)) as (*mut lh_entry) {
            (*(*(*t).tail).prev).next = 0i32 as (*mut ::std::os::raw::c_void) as (*mut lh_entry);
            (*t).tail = (*(*t).tail).prev as (*mut lh_entry);
        } else {
            (*(*(*t).table.offset(n as (isize))).prev).next = (*(*t).table.offset(n as (isize)))
                .next;
            (*(*(*t).table.offset(n as (isize))).next).prev = (*(*t).table.offset(n as (isize)))
                .prev;
        }
        (*(*t).table.offset(n as (isize))).next = {
            let _rhs = 0i32 as (*mut ::std::os::raw::c_void);
            let _lhs = &mut (*(*t).table.offset(n as (isize))).prev;
            *_lhs = _rhs as (*mut lh_entry);
            *_lhs
        };
        0i32
    }
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_delete(
    mut t: *mut lh_table,
    mut k: *const ::std::os::raw::c_void,
) -> i32 {
    let mut e: *mut lh_entry = lh_table_lookup_entry(t, k);
    if e.is_null() {
        -1i32
    } else {
        lh_table_delete_entry(t, e)
    }
}

#[no_mangle]
pub unsafe extern "C" fn lh_table_length(mut t: *mut lh_table) -> i32 {
    (*t).count
}
