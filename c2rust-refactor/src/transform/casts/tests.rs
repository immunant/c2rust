use super::{check_double_cast, DoubleCastAction, SimpleTy};
use quickcheck::{quickcheck, Arbitrary, Gen};
use rand::Rng;
use z3::ast::{Ast, BV};
use z3::{Config, Context, SatResult, Solver};

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
struct PointerWidth(usize);

impl Arbitrary for PointerWidth {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let x = g.gen_range(0, 3);
        PointerWidth([16, 32, 64][x])
    }
}

impl Arbitrary for SimpleTy {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let x = g.gen_range(0, 13);
        match x {
            0 | 1 | 2 | 3 => SimpleTy::Int([8, 16, 32, 64][x], false),
            4 | 5 | 6 | 7 => SimpleTy::Int([8, 16, 32, 64][x - 4], true),
            8 => SimpleTy::Size(false),
            9 => SimpleTy::Size(true),
            10 => SimpleTy::Float32,
            11 => SimpleTy::Float64,
            12 => SimpleTy::Pointer,
            // TODO: generate some Other's
            _ => unreachable!(),
        }
    }
}

fn ty_bit_width(ty: SimpleTy, pw: PointerWidth) -> u32 {
    match ty {
        SimpleTy::Int(w, _) => w.try_into().expect("failed to cast"),
        SimpleTy::Size(_) | SimpleTy::Pointer => pw.0.try_into().expect("failed to cast"),
        SimpleTy::Float32 => 32,
        SimpleTy::Float64 => 64,
        SimpleTy::Ref | SimpleTy::Array | SimpleTy::Other => unreachable!(), // FIXME
    }
}

fn cast_bv<'bv>(bv: BV<'bv>, from_ty: SimpleTy, to_ty: SimpleTy, pw: PointerWidth) -> BV<'bv> {
    let from_width = ty_bit_width(from_ty, pw);
    let to_width = ty_bit_width(to_ty, pw);
    if to_width == from_width {
        bv
    } else if to_width < from_width {
        bv.extract(to_width - 1, 0)
    } else if from_ty.is_signed() {
        bv.sign_ext(to_width - from_width)
    } else {
        bv.zero_ext(to_width - from_width)
    }
}

fn cast_tys<'bv>(bv: BV<'bv>, tys: &[SimpleTy], pw: PointerWidth) -> BV<'bv> {
    tys.windows(2).fold(bv, |y, w| cast_bv(y, w[0], w[1], pw))
}

thread_local!(static Z3_CONFIG: Config = Config::new());
thread_local!(static Z3_CONTEXT: Context = Z3_CONFIG.with(|cfg| Context::new(cfg)));

quickcheck! {
    // Verify `check_double_cast` using QuickCheck and Z3
    fn verify_double_cast(pw: PointerWidth, tys: Vec<SimpleTy>) -> bool {
        if tys.len() <= 1 {
            return true;
        }

        Z3_CONTEXT.with(|ctx| {
            // Build a minimized list of types with double casts removed
            let mut min_tys = vec![tys[0].clone()];
            for ty in &tys[1..] {
                assert!(!min_tys.is_empty());
                if *ty == min_tys[min_tys.len() - 1] {
                    // Cast to the same type, ignore it
                    continue;
                }
                if min_tys.len() < 2 {
                    min_tys.push(ty.clone());
                    continue;
                }
                let last2 = &min_tys[min_tys.len() - 2..];
                match check_double_cast(last2[0], last2[1], *ty) {
                    DoubleCastAction::RemoveBoth => {
                        min_tys.pop();
                    }
                    DoubleCastAction::RemoveInner => {
                        *min_tys.last_mut().unwrap() = ty.clone();
                    }
                    DoubleCastAction::KeepBoth => {
                        min_tys.push(ty.clone());
                    }
                }
            }

            let x = BV::new_const(&ctx, "x", ty_bit_width(tys[0], pw));
            let y = cast_tys(x.clone(), &tys[..], pw);
            let z = cast_tys(x, &min_tys[..], pw);

            // Check the full type list against the minimized one
            let solver = Solver::new(&ctx);
            solver.assert(&z._eq(&y).not());
            solver.check() == SatResult::Unsat
        })
    }
}
