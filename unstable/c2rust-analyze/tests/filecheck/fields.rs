// CHECK-LABEL: === ADT Metadata ===
// CHECK-DAG: struct Data<'d,'h0,'h1,'h2> {
pub struct Data<'d> {
    // 1: hypothetical pointer field lifetime 'h0 -> Data<'d, 'h0>
    // CHECK-DAG: pi: &'h0 i32
    pub pi: *mut i32,

    // 2: hypothetical pointer field lifetime 'h1 -> Data<'d, 'h0, 'h1>
    // 3: A has no new parameters to bubble up
    // 8: A has new lifetime param 'h2 -> Data<'d, 'h0, 'h1, 'h2>
    // CHECK-DAG: pa: &'h1 A<'d,'h0,'h1,'h2>
    pub pa: *mut A<'d>,

    // 4: A has no new parameters to bubble up
    // CHECK-DAG: a: A<'d,'h0,'h1,'h2>
    pub a: A<'d>,
}

// CHECK-DAG: struct A<'a,'h0,'h1,'h2> {
pub struct A<'a> {
    // 5: Data has new lifetime params 'h0 and 'h1 -> A<'a, 'h0, 'h1>
    // 9: Data has new lifetime param 'h2, but 'h2 is already in A
    // CHECK-DAG: rd: &'a Data<'a,'h0,'h1,'h2>
    pub rd: &'a Data<'a>,

    // 6: hypothetical pointer field lifetime 'h2 -> A<'a, 'h0, 'h1, 'h2>
    // 7: A has new lifetime params to bubble up, but they're already present
    // CHECK-DAG: pra: &'h2 &'a A<'a,'h0,'h1,'h2>
    pub pra: *mut &'a mut A<'a>,
}

// CHECK-DAG: struct VecTup<'a,'h0,'h1,'h2,'h3,'h4> {
struct VecTup<'a> {
    // CHECK-DAG: bar: &'h4 std::vec::Vec<(VecTup<'a,'h0,'h1,'h2,'h3,'h4>,&'h3 A<'a,'h0,'h1,'h2>),std::alloc::Global>
    bar: *mut Vec<(VecTup<'a>, *mut A<'a>)>,
}

struct Hypo {
    h: *mut i32,
}

struct HypoWrapper {
    hw: *const Hypo,
}

// let rd = (*(**ppd).a.pra).rd
// CHECK-DAG: Label { origin: Some(Origin([[REF_D_ORIGIN:[0-9]+]])), origin_params: []{{.*}}}#&'a Data
// CHECK-DAG: Label { origin: None, origin_params: [('d, Origin([[REF_D_ORIGIN]]){{.*}}}#Data
// CHECK-DAG: assign Label { origin: Some(Origin({{[0-9]+}})){{.*}} = Label { origin: Some(Origin([[REF_D_ORIGIN]]))

// *(*(**ppd).pa).pra = ra
// CHECK-DAG: Label { origin: Some(Origin([[REF_A_ORIGIN:[0-9]+]])), origin_params: []{{.*}}}#&'a mut A
// CHECK-DAG: Label { origin: None, origin_params: [('a, Origin([[REF_A_ORIGIN]]){{.*}}#A
// CHECK-DAG: assign Label { origin: Some(Origin([[REF_A_ORIGIN]]))

// (*(**ppd).pa).pra = (*(**ppd).pa).pra
// CHECK-DAG: Label { origin: Some(Origin([[P_REF_A_ORIGIN:[0-9]+]])), origin_params: []{{.*}}}#*mut &'a mut A
// CHECK-DAG: Label { origin: Some(Origin([[REF_A_ORIGIN:[0-9]+]])), origin_params: []{{.*}}}#&'a mut A
// CHECK-DAG: Label { origin: None, origin_params: [('a, Origin([[REF_A_ORIGIN]])){{.*}}('h2, Origin([[P_REF_A_ORIGIN]]))]{{.*}}}#A
// CHECK-DAG: assign Label { origin: Some(Origin([[P_REF_A_ORIGIN]]))

// CHECK-LABEL: final labeling for "_field_access"
// CHECK-DAG: ([[@LINE+3]]: ppd): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK
// CHECK-DAG: ([[@LINE+2]]: ra): &'d mut A<'d>
// CHECK-DAG: ([[@LINE+1]]: ppd): &mut &mut Data
unsafe fn _field_access<'d, 'a: 'd, T: Clone + Copy>(ra: &'d mut A<'d>, ppd: *mut *mut Data<'d>) {
    // CHECK-DAG: ([[@LINE+2]]: rd): addr_of = UNIQUE | NON_NULL | STACK, type = READ | UNIQUE | NON_NULL | HEAP | STACK
    // CHECK-DAG: ([[@LINE+1]]: rd): &Data
    let rd = (*(**ppd).a.pra).rd;

    // CHECK-DAG: ([[@LINE+2]]: pi): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK
    // CHECK-DAG: ([[@LINE+1]]: pi): &mut i32
    let pi = rd.pi;
    *pi = 3;
    let _i = *pi;

    *(*(**ppd).pa).pra = ra;
    (*(**ppd).pa).pra = (*(**ppd).pa).pra;
}

// CHECK-DAG: pub struct Data<'d,'h0,'h1,'h2> {
// CHECK-DAG: pub pi: &'h0 mut (i32),
// CHECK-DAG: pub pa: &'h1 mut (A<'d,'h0,'h1,'h2>),
// CHECK-DAG: pub a: A<'d,'h0,'h1,'h2>,

// CHECK-DAG: pub struct A<'a,'h0,'h1,'h2> {
// CHECK-DAG: pub rd: &'a Data<'a,'h0,'h1,'h2>,
// CHECK-DAG: pub pra: &'h2 core::cell::Cell<(&'a mut A<'a,'h0,'h1,'h2>)>,
// CHECK-DAG: bar: &'h4 (Vec<(VecTup<'a,'h0,'h1,'h2,'h3,'h4>, &'h3 (A<'a,'h0,'h1,'h2>))>),

// CHECK-DAG: struct HypoWrapper<'h5,'h6>
// CHECK-DAG: hw: &'h6 (Hypo<'h5>)

// CHECK-DAG: unsafe fn _field_access<'d, 'a: 'd,'h0,'h1,'h2,'h3,'h4,'h5,'h6,'h7, T: Clone + Copy>(ra: &'d mut A<'d,'h0,'h1,'h2>, ppd: &'h3 mut (&'h4 mut (Data<'d,'h5,'h6,'h7>))) {

use std::ptr;

// CHECK: #[derive(Copy, Clone)]
// CHECK-NEXT: struct Simple
#[derive(Copy, Clone)]
struct Simple {
    x: i32,
    y: i32,
}

// CHECK: #[derive(Copy, Clone)]
// CHECK-NEXT: struct SimpleGeneric<T: Clone>
#[derive(Copy, Clone)]
struct SimpleGeneric<T: Clone> {
    x: i32,
    y: i32,
    t: T,
}

// CHECK: #[derive(Copy, Clone)]
// CHECK-NEXT: struct WithPtr<'h7,'h8> {
#[derive(Copy, Clone)]
struct WithPtr {
    p: *mut i32,
    q: *mut i32,
}

unsafe fn f() {
    let mut s = Simple { x: 1, y: 2 };
    let wp = WithPtr {
        p: ptr::addr_of_mut!(s.x),
        q: ptr::addr_of_mut!(s.y),
    };
    *(wp.p) = *(wp.q) + 1;
}

// CHECK-LABEL: fn empty_generics<'h0>(test: &'h0 (i32))
fn empty_generics<>(test: *const i32) {}

// CHECK-LABEL: fn no_generics<'h0>(test: &'h0 (i32))
fn no_generics(test: *const i32) {}

// CHECK-LABEL: fn const_generics<'h0,const N: usize>(test: &'h0 (i32))
fn const_generics<const N: usize>(test: *const i32) {}

// CHECK-LABEL: fn lifetime_and_const_generics<'a, 't: 'a,'h0, const N: usize>(test: &'h0 (i32))
fn lifetime_and_const_generics<'a, 't: 'a, const N: usize>(test: *const i32) {}

// CHECK-LABEL: fn where_clause<'a, 'b,'h0>(x: &'a i32, y: &'b i32, test: &'h0 (i32))
fn where_clause<'a, 'b>(x: &'a i32, y: &'b i32, test: *const i32)
where 'a: 'b {
}
