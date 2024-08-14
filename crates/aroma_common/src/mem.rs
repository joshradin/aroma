//! Some useful memory operations

/// Creates a copy of some type explicitly
pub const fn copy<T : Copy>(v: &T) -> T {
    *v
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copy() {
        let i = 3;
        let copy = copy(&i);
        assert_eq!(i, copy);
    }
}