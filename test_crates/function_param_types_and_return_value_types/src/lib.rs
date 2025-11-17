pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

pub fn fn_with_ref(param: &i32) -> i32 {
    *param + 1
}

pub fn fn_with_mut_ref(param: &mut i32) {
    *param += 1;
}

pub fn fn_with_generic<T: std::ops::Add<Output = T>>(a: T, b: T) -> T {
    a + b
}

pub fn fn_with_generic_ref<T: std::ops::Add<Output = T>>(a: &T, b: &T) -> T
where
    T: Copy,
{
    *a + *b
}

pub fn fn_with_array_param(arr: [i32; 3]) -> i32 {
    arr.iter().sum()
}
