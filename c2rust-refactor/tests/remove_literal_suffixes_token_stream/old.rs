// Regression input for the token-stream mismatch caused by editing vec! literals.

fn main() {
    let ints = vec![10u32, 20u32, 30u32];
    let floats = vec![1.5f64, 2.5f64, 3.5f64];

    macro_rules! suffix_vec {
        ($($val:expr),* $(,)?) => {
            vec![$($val),*]
        };
    }

    let nested = suffix_vec![vec![1u32, 2u32], vec![3u32, 4u32]];
    let _ = (ints, floats, nested);
}
