pub struct Counter(i64);

impl Counter {
    pub const START: i64 = 0;
}

pub trait Batched {
    const LOG_AS: &'static str = "[batch]";
    const DEFAULT_BATCH_SIZE: usize = 16;
    const MAX_BATCH_SIZE: usize;
    const MIN_BATCH_SIZE: usize = min_batch_size();
    const INVALID_BATCH_SIZE: usize = 1 - 1;
}

const fn min_batch_size() -> usize {
    2 - 1
}
