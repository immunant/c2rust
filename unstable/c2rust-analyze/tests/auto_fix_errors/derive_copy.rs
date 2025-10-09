#[derive(Clone, Copy)]
struct S1 {
    x: Box<i32>,
    y: &'static i32,
}

#[derive(Clone, Copy,)]
struct S2 {
    x: Box<i32>,
    y: &'static i32,
}


#[derive(Copy, Clone)]
struct S3 {
    x: Box<i32>,
    y: &'static i32,
}


#[derive(Copy, Clone,)]
struct S4 {
    x: Box<i32>,
    y: &'static i32,
}


#[derive(Copy)]
struct S5 {
    x: Box<i32>,
    y: &'static i32,
}

#[derive(Copy,)]
struct S6 {
    x: Box<i32>,
    y: &'static i32,
}


#[derive(
    Copy
    ,
    Clone
)]
struct S7 {
    x: Box<i32>,
    y: &'static i32,
}
