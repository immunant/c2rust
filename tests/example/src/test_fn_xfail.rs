// This failure is specific to this test, other passing tests may be present

#[test]
#[should_panic]
pub fn test_xfails() {
    panic!("Not meant to pass");
}
