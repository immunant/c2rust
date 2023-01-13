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

// CHECK-DAG: struct VecTup<'a,'h3,'h4,'h0,'h1,'h2> {
struct VecTup<'a> {
    // CHECK-DAG: bar: &'h3 std::vec::Vec<(VecTup<'a,'h3,'h4,'h0,'h1,'h2>,&'h4 A<'a,'h0,'h1,'h2>),std::alloc::Global>
    bar: *mut Vec<(VecTup<'a>, *mut A<'a>)>,
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
// CHECK-DAG: ([[@LINE+5]]: ppd): addr_of = UNIQUE, type = READ | WRITE | UNIQUE
// FIXME: `ra: &A` is known to be incorrect - it should be `ra: &mut A`.  However, we don't
// properly prevent rewriting of already-safe reference types at the moment.
// CHECK-DAG: ([[@LINE+2]]: ra): &A
// CHECK-DAG: ([[@LINE+1]]: ppd): &mut &mut Data
unsafe fn _field_access<'d>(ra: &'d mut A<'d>, ppd: *mut *mut Data<'d>) {
    // CHECK-DAG: ([[@LINE+2]]: rd): addr_of = UNIQUE, type = READ | UNIQUE
    // CHECK-DAG: ([[@LINE+1]]: rd): &Data
    let rd = (*(**ppd).a.pra).rd;

    // CHECK-DAG: ([[@LINE+2]]: pi): addr_of = UNIQUE, type = READ | WRITE | UNIQUE
    // CHECK-DAG: ([[@LINE+1]]: pi): &mut i32
    let pi = rd.pi;
    *pi = 3;
    let _i = *pi;

    *(*(**ppd).pa).pra = ra;
    (*(**ppd).pa).pra = (*(**ppd).pa).pra;
}
