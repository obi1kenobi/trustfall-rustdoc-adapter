pub trait AssocConstraintOrder {
    type A;
    type B;
}

pub fn a_then_b(value: Box<dyn AssocConstraintOrder<A = impl Clone, B = impl Copy>>) {
    let _ = value;
}

pub fn b_then_a(value: Box<dyn AssocConstraintOrder<B = impl Copy, A = impl Clone>>) {
    let _ = value;
}
