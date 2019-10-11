#![feature(ptr_wrapping_offset_from, custom_attribute, param_attrs)]

#[path = "../../old.rs"]
mod old;
#[path = "../../new.rs"]
mod new;

#[test]
fn test_ten_mul() {
    let mut f = 4.2f64;
    let mut g = 3.3f64;

    unsafe {
        assert_eq!(old::ten_mul(&mut f, 22, &mut g), 0);
    }

    assert_eq!(f, 67.3);
    assert_eq!(g, 3.3);

    f = 4.2;

    unsafe {
        assert_eq!(new::ten_mul(&mut f, 22, Some(&g)), 0);
    }

    assert_eq!(f, 67.3);
    assert_eq!(g, 3.3);
}

#[test]
fn test_struct_ptr() {
    let read_data = [1u8, 2, 3, 4, 5];
    let mut ctx1 = old::Ctx {
        data: [5, 4, 3, 2, 1]
    };
    let mut ctx2 = old::Ctx {
        data: [9, 8, 7, 6, 5]
    };

    unsafe {
        old::struct_ptr(&mut ctx1, &mut ctx2, read_data.as_ptr());
    }

    assert_eq!(ctx1.data, [4, 4, 3, 2, 1]);
    assert_eq!(ctx2.data, [5, 8, 7, 6, 5]);

    let mut ctx1 = new::Ctx {
        data: [5, 4, 3, 2, 1]
    };
    let mut ctx2 = new::Ctx {
        data: [9, 8, 7, 6, 5]
    };

    unsafe {
        new::struct_ptr(Some(&mut ctx1), Some(&mut ctx2), Some(&read_data));
    }

    assert_eq!(ctx1.data, [4, 4, 3, 2, 1]);
    assert_eq!(ctx2.data, [5, 8, 7, 6, 5]);
}

#[test]
fn test_wmemcmp() {
    let wide1 = [1, 2, 3, 4];
    let wide2 = [1, 2, 3, 4];
    let wide3 = [1, 2, 3, 5];
    let wide4 = [1, 2, 3, 3];

    let ret = unsafe {
        old::wmemcmp(wide1.as_ptr(), wide2.as_ptr(), wide1.len() as _)
    };

    assert_eq!(ret, 0);

    let ret = unsafe {
        old::wmemcmp(wide1.as_ptr(), wide3.as_ptr(), wide1.len() as _)
    };

    assert_eq!(ret, -1);

    let ret = unsafe {
        old::wmemcmp(wide1.as_ptr(), wide4.as_ptr(), wide1.len() as _)
    };

    assert_eq!(ret, 1);

    let ret = unsafe {
        new::wmemcmp(Some(&wide1), Some(&wide2), wide1.len() as _)
    };

    assert_eq!(ret, 0);

    let ret = unsafe {
        new::wmemcmp(Some(&wide1), Some(&wide3), wide1.len() as _)
    };

    assert_eq!(ret, -1);

    let ret = unsafe {
        new::wmemcmp(Some(&wide1), Some(&wide4), wide1.len() as _)
    };

    assert_eq!(ret, 1);

    let ret = unsafe {
        new::wmemcmp2(&wide1, &wide2, wide1.len() as _)
    };

    assert_eq!(ret, 0);

    let ret = unsafe {
        new::wmemcmp2(&wide1, &wide3, wide1.len() as _)
    };

    assert_eq!(ret, -1);

    let ret = unsafe {
        new::wmemcmp2(&wide1, &wide4, wide1.len() as _)
    };

    assert_eq!(ret, 1);
}

#[test]
fn test_eisnan() {
    let mut read_data = [0; 10];

    let ret = unsafe {
        old::eisnan(read_data.as_ptr())
    };

    assert_eq!(ret, 0);

    read_data[9] = 0x7fff;

    let ret = unsafe {
        old::eisnan(read_data.as_ptr())
    };

    assert_eq!(ret, 0);

    read_data[8] = 1;

    let ret = unsafe {
        old::eisnan(read_data.as_ptr())
    };

    assert_eq!(ret, 1);

    read_data = [0; 10];

    let ret = unsafe {
        new::eisnan(Some(&read_data))
    };

    assert_eq!(ret, 0);

    read_data[9] = 0x7fff;

    let ret = unsafe {
        new::eisnan(Some(&read_data))
    };

    assert_eq!(ret, 0);

    read_data[8] = 1;

    let ret = unsafe {
        new::eisnan(Some(&read_data))
    };

    assert_eq!(ret, 1);

    read_data = [0; 10];

    let ret = unsafe {
        new::eisnan2(&read_data)
    };

    assert_eq!(ret, 0);

    read_data[9] = 0x7fff;

    let ret = unsafe {
        new::eisnan2(&read_data)
    };

    assert_eq!(ret, 0);

    read_data[8] = 1;

    let ret = unsafe {
        new::eisnan2(&read_data)
    };

    assert_eq!(ret, 1);
}

#[test]
fn test_eneg() {
    let mut write_data = [0; 10];

    unsafe {
        old::eneg(write_data.as_mut_ptr());
    }

    assert_eq!(write_data[9], 0x8000);

    write_data = [0; 10];

    unsafe {
        new::eneg(Some(&mut write_data));
    }

    assert_eq!(write_data[9], 0x8000);

    write_data = [0; 10];

    unsafe {
        new::eneg2(&mut write_data);
    }

    assert_eq!(write_data[9], 0x8000);

    write_data = [0; 10];

    unsafe {
        new::eneg3(&mut write_data);
    }

    assert_eq!(write_data[9], 0x8000);
}

#[test]
fn test_eshdn1() {
    let mut write_data = [0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

    unsafe {
        old::eshdn1(write_data.as_mut_ptr());
    }

    assert_eq!(write_data, [0, 0, 0, 32769, 1, 32770, 2, 32771, 3, 32772, 4, 32773, 5]);

    write_data = [0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];

    unsafe {
        new::eshdn1(Some(&mut write_data));
    }

    assert_eq!(write_data, [0, 0, 0, 32769, 1, 32770, 2, 32771, 3, 32772, 4, 32773, 5]);
}
