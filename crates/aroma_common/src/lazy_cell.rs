//! A copy-able lazy cell.

use std::ops::Deref;

#[derive(Debug, Copy, Clone)]
enum State<T, F> {
    Uninit(F),
    Init(T),
    Poisoned
}

/// A copy-able lazy cell
#[derive(Debug, Copy, Clone)]
pub struct LazyCopyCell<T, F = fn() -> T> {
    state:  State<T, F>,
}

impl<T : Copy, F: Fn() -> T + Copy> LazyCopyCell<T, F> {
    pub const fn new(f: F) -> Self {
        Self {
            state: State::Uninit(f),
        }
    }

    /// Gets the inner value, evaluating if necessary.
    #[inline]
    pub fn get(&self) -> Option<&T> {
        let state =self.state;
        todo!()
    }
}

impl<T: Copy, F: Fn() -> T + Copy> Deref for LazyCopyCell<T, F> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::mem::copy;
    use super::*;

    #[test]
    fn test_lazy_cell() {
        let cell = LazyCopyCell::new(|| 101);
        let copy = copy(&cell);

        assert_eq!(*cell, *copy);
    }
}
