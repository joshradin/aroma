pub mod transform_type_resolution;
pub mod type_checking;

/// transform result
pub type TResult<I, O = I, E = TransformError> = Result<(I, O), E>;

/// A transformation trait
pub trait Transformer<I, O, E> {
    /// Transform an input into an output. Since transformations can
    /// be partial, the input is returned
    fn transform(&mut self, i: I) -> TResult<I, O, E>;
}

impl<F, I, O, E> Transformer<I, O, E> for F
where
    F: FnMut(I) ->TResult<I, O, E>,
{
    fn transform(&mut self, i: I) -> TResult<I, O, E> {
        self(i)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("transform failed")]
pub struct TransformError;