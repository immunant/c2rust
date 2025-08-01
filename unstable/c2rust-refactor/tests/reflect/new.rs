#![feature(type_ascription)]

struct S<T>(T);

impl S<(i32, S<Option<i32>>)> {
    fn f(self) {}
}

fn g() {}

fn main() {
    let s = (crate::S::<(i32, crate::S<::std::option::Option<i32>>)>: _)(
        (
            0: i32,
            (crate::S::<::std::option::Option<i32>>: _)(
                ::std::prelude::v1::None: ::std::option::Option<i32>,
            ): crate::S<::std::option::Option<i32>>,
        ): (i32, crate::S<::std::option::Option<i32>>),
    ): crate::S<(i32, crate::S<::std::option::Option<i32>>)>;
    let x = (s: crate::S<(i32, crate::S<::std::option::Option<i32>>)>).f(): ();
    let f = crate::S::<(i32, crate::S<::std::option::Option<i32>>)>::f: _;
    let s = (crate::S::<(i32, crate::S<::std::option::Option<i32>>)>: _)(
        (
            1: i32,
            (crate::S::<::std::option::Option<i32>>: _)(
                (::std::prelude::v1::Some: _)(2: i32): ::std::option::Option<i32>,
            ): crate::S<::std::option::Option<i32>>,
        ): (i32, crate::S<::std::option::Option<i32>>),
    ): crate::S<(i32, crate::S<::std::option::Option<i32>>)>;

    let mut v = (::std::vec::Vec::<i32>::new: _)(): ::std::vec::Vec<i32>;
    (v: ::std::vec::Vec<i32>).push(123: i32): ();
    let f = ::std::vec::Vec::<i32>::pop: _;
    (::std::vec::Vec::<i32>::pop: _)(&mut (v: ::std::vec::Vec<i32>): &mut ::std::vec::Vec<i32>):
        ::std::option::Option<i32>;
    (::std::vec::Vec::<i32>::drain::<::std::ops::RangeFull>: _)(
        &mut (v: ::std::vec::Vec<i32>): &mut ::std::vec::Vec<i32>,
        ::std::ops::RangeFull: ::std::ops::RangeFull,
    ): ::std::vec::Drain<i32>;

}
