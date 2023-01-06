pub struct Data<'d> {
    // final lifetime parameters: Data<'d, 'h0, 'h1, 'h2>

    // 1: hypothetical pointer field lifetime 'h0 -> Data<'d, 'h0>
    pub pi: *mut i32, // &'h0 i32

    // 2: hypothetical pointer field lifetime 'h1 -> Data<'d, 'h0, 'h1>
    // 3: A has no new parameters to bubble up
    // 8: A has new lifetime param 'h2 -> Data<'d, 'h0, 'h1, 'h2>
    pub pa: *mut A<'d>, // &'h1 A<'a, 'h0, 'h1, 'h2>

    // 4: A has no new parameters to bubble up
    pub a: A<'d>, // A<'a, 'h0, 'h1, 'h2>
}

pub struct A<'a> {
    // final lifetime parameters: A<'a, 'h0, 'h1, 'h2>

    // 5: Data has new lifetime params 'h0 and 'h1 -> A<'a, 'h0, 'h1>
    // 9: Data has new lifetime param 'h2, but 'h2 is already in A
    pub rd: &'a Data<'a>, // &'a Data<'a, 'h0, 'h1, 'h2>

    // 6: hypothetical pointer field lifetime 'h2 -> A<'a, 'h0, 'h1, 'h2>
    // 7: A has new lifetime params to bubble up, but they're already present
    pub pra: *mut &'a mut A<'a>, // &'h2 &'a A<'a, 'h0, 'h1, 'h2>
}

// CHECK-LABEL: final labeling for "_field_access"
// CHECK-DAG: ([[#@LINE+3]]: ppd): addr_of = UNIQUE, type = READ | WRITE | UNIQUE
// CHECK-DAG: ([[#@LINE+2]]: ra): &mut A
// CHECK-DAG: ([[#@LINE+1]]: ppd): &mut &mut Data
unsafe fn _field_access<'d>(ra: &'d mut A<'d>, ppd: *mut *mut Data<'d>) {
    // CHECK-DAG: ([[#@LINE+2]]: rd): addr_of = UNIQUE, type = READ | UNIQUE
    // CHECK-DAG: ([[#@LINE+1]]: rd): &Data
    let rd = (*(**ppd).a.pra).rd;

    // CHECK-DAG: ([[#@LINE+2]]: pi): addr_of = UNIQUE, type = READ | WRITE | UNIQUE
    // CHECK-DAG: ([[#@LINE+1]]: pi): &mut i32
    let pi = rd.pi;
    *pi = 3;
    let _i = *pi;

    *(*(**ppd).pa).pra = ra;
    (*(**ppd).pa).pra = (*(**ppd).pa).pra;
}
