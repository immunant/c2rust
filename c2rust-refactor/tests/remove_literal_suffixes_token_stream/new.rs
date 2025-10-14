// Regression input for the token-stream mismatch caused by editing vec! literals.

fn main() {
    let ints = vec![10u32, 20, 30];
    let floats = vec![1.5, 2.5, 3.5];

    macro_rules! suffix_vec {
        ($($val:expr),* $(,)?) => {
            vec![$($val),*]
        };
    }

    let nested = suffix_vec![vec![1u32, 2], vec![3u32, 4]];
    let _ = (ints, floats, nested);
}
