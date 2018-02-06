//struct FiboArg(u64, u64);
//#[derive(Hash)]
//struct FiboArg<T>(#[cross_check(custom_hash = "tch")] T, #[cross_check(none)] T);
//struct FiboArg(#[cross_check(custom_hash = "tch")] u64, u64);
//#[cross_check(custom_hash = "hash_arg")] struct FiboArg(u64, u64, #[cross_check(none)] String);
//struct FiboArg(#[cross_check(fixed = "0x1234")] u64, #[cross_check(fixed = "0x2345")] u64);
//struct FiboArg(#[cross_check(fixed = 0x1234)] u64, u64, #[cross_check(none)] String);

struct FiboArg(u64, u64, #[cross_check(none)] String);

fn hash_arg<XCHA, XCHS>(arg: &FiboArg, _: usize) -> u64 {
    arg.0.wrapping_mul(0x10000).wrapping_add(arg.1)
}

fn foo(v: &u64) -> u64 {
    v.wrapping_add(4096)
}

fn tch<XCHA, XCHS, S, F>(h: &mut XCHA, _: &S, field: F, _: usize)
        where XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
              F: ::std::borrow::Borrow<String> {
    h.write_u64(field.borrow().parse::<u64>().unwrap());
}

//#[cross_check(none)]
//#[cross_check(yes, ahasher="::cross_check_runtime::hash::djb2::Djb2Hasher", shasher="::cross_check_runtime::hash::djb2::Djb2Hasher")]
//#[cross_check(yes, args(n(default)))]
#[cross_check(yes, all_args(fixed=0x234), args())]
pub fn fibo(n: u64) -> u64 {
    //#[cross_check]
    let fa = FiboArg(n.wrapping_sub(1), n.wrapping_sub(2), n.to_string());
    cross_check_value!(fa);
    match n {
        0 |
        1 => 1,
        _ => fibo(n - 1) + fibo(n - 2),
    }
}
