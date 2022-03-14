type c_int = i32;
pub fn insertion_sort(n: c_int, p: &mut [c_int]) {
    let mut i: c_int = 1 as c_int;
    while i < n {
        let tmp: c_int = p[i as usize];
        let mut j: c_int = i;
        while j > 0 as c_int && p[(j - 1 as c_int) as usize] > tmp {
            p[j as usize] = p[(j - 1 as c_int) as usize];
            j -= 1
        }
        p[j as usize] = tmp;
        i += 1
    }
}
