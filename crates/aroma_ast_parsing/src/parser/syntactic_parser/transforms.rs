//! transforms for converting the hir to mir

mod to_mir;
mod transform_combinators;

use crate::parser::syntactic_parser::hir::translation_unit::TranslationUnit as ParsedTranslationUnit;
use crate::parser::transforms::to_mir::to_mir;
use crate::parser::SyntaxError;
use aroma_ast::mir::translation_unit::TranslationUnit;
use nom::Parser;
pub use transform_combinators::*;

/// The full transform sequence
pub fn transform(translation_unit: ParsedTranslationUnit) -> Result<TranslationUnit, SyntaxError> {
    to_mir(translation_unit)
}

/// A basic transform trait
pub trait Transformer<I, O, E> {
    fn transform(&mut self, input: I) -> Result<O, E>;

    fn map<F: FnMut(O) -> O2, O2>(self, mapper: F) -> Map<Self, F, I, O, O2, E>
    where
        Self: Sized,
    {
        Map::new(self, mapper)
    }

    fn and_then<T: Transformer<O, O2, E>, O2>(self, next: T) -> AndThen<Self, T, I, O, O2, E>
    where
        Self: Sized,
    {
        AndThen::new(self, next)
    }
}

impl<F: FnMut(I) -> Result<O, E>, I, O, E> Transformer<I, O, E> for F {
    fn transform(&mut self, input: I) -> Result<O, E> {
        self(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::syntactic_parser::transforms::Transformer;
    use std::convert::Infallible;

    #[test]
    fn test_transform_map() {
        let mut transform = (|a: i32| Ok::<_, Infallible>(a * a)).map(|b| b * b);
        let result = transform.transform(4).unwrap();
        assert_eq!(result, 256);
    }

    #[test]
    fn test_transform_and_then() {
        let mut transform = (|a: i32| Ok::<_, Infallible>(a * a)).and_then(|b| Ok(b * b));
        let result = transform.transform(4).unwrap();
        assert_eq!(result, 256);
    }
}
