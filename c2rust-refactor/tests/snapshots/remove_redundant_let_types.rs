fn main() {
    // Unsuffixed numeric literals and generic calls need the expected type.
    let mut scalar: u64 = 0;
    scalar = scalar.wrapping_add(1);
    let mut array: [u8; 2] = [0; 2];
    array[0] = array[0].wrapping_add(1);
    let float: f32 = 0.;
    assert_eq!(float.abs(), 0.);
    let args: Vec<Vec<u8>> = ["arg"].iter().map(|s| s.as_bytes().to_vec()).collect();
    assert_eq!(args.len(), 1);

    // The annotation also supplies types through local paths and aggregates.
    let source = 0;
    let from_local: u64 = source;
    assert_eq!(std::mem::size_of_val(&from_local), 8);
    let tuple: (u64, f32) = (0, 0.);
    let empty: [u8; 0] = [];
    assert_eq!(std::mem::size_of_val(&tuple.0), 8);
    assert_eq!(empty.len(), 0);

    // Preserve coercions at both the outer initializer and aggregate elements.
    let mut pointer: *const u8 = 0 as *mut u8;
    pointer = std::ptr::null::<u8>();
    let mut pointers: [*const u8; 1] = [0 as *mut u8];
    pointers[0] = std::ptr::null::<u8>();
    let mut pointer_tuple: (*const u8,) = (0 as *mut u8,);
    pointer_tuple.0 = std::ptr::null::<u8>();
    let mut slice: &[u8] = b"a";
    slice = b"longer";
    let mut slices: (&[u8],) = (b"a",);
    slices.0 = b"longer";
    let mut callback: fn(u8) -> u8 = |x| x;
    callback = |x| x + 1;
    assert_eq!(callback(1), 2);

    // A cast containing `_` is not an independent source of type information.
    let inferred_pointer: *const u8 = 0 as *const _;
    assert!(inferred_pointer.is_null());

    // Remove annotations already supplied by the initializer, including aliases.
    type Word = u64;
    let fixed: Word = 1u64;
    let cast: Word = 1 as Word;
    let negative: i64 = -1i64;
    let boolean: bool = !false;
    let character: char = 'a';
    let string: &str = "hello";
    let bytes: &[u8; 2] = b"hi";
    let fixed_array: [u8; 2] = [1u8, 2u8];
    let fixed_repeat: [u8; 2] = [1u8; 2];
    let fixed_tuple: (u64, f32) = (1u64, 0f32);
    let unit: () = ();
}
