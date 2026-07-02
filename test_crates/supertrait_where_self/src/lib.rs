pub trait Base {
    type Assoc;
}

pub trait GenericBase<T> {}

pub trait GenericMarker<T> {}

pub trait LocalMarker {}

pub trait HeaderSupertrait: Base<Assoc = u8> {}

pub trait WhereSelfSupertrait
where
    Self: Base<Assoc = u8>,
{
}

pub trait WhereSelfGeneric<T>
where
    Self: GenericBase<T>,
{
}

pub trait NonSelfWhere<T>
where
    T: Base<Assoc = u8>,
{
}

pub trait RefSelfWhere<T>
where
    for<'a> &'a Self: GenericBase<T>,
{
}

pub trait RefSelfWithTraitLifetime<'a, T>
where
    Self: 'a,
    &'a Self: GenericBase<T>,
{
}

pub trait TwoColonTwoWhere<T>: Base<Assoc = u8> + GenericBase<T>
where
    Self: GenericMarker<T>,
    Self: LocalMarker,
{
}

pub trait MixedSuperAndNonSuper<T>: Base<Assoc = u8>
where
    Self: GenericBase<T>,
    for<'a> &'a Self: GenericMarker<T>,
{
}
