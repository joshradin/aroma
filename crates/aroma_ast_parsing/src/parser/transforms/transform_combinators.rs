use crate::parser::transforms::Transformer;
use std::marker::PhantomData;

type PhantomFunc<I, O> = PhantomData<fn(I) -> O>;

#[derive(Debug)]
pub struct Map<T1, F, I, M, O, E> {
    transform: T1,
    func: F,
    _transform_ty: PhantomFunc<I, Result<M, E>>,
    _func_ty: PhantomFunc<M, O>,
}

impl<T1, F, I, M, O, E> Transformer<I, O, E> for Map<T1, F, I, M, O, E>
where
    T1: Transformer<I, M, E>,
    F: FnMut(M) -> O,
{
    fn transform(&mut self, input: I) -> Result<O, E> {
        let med = self.transform.transform(input)?;
        Ok((self.func)(med))
    }
}

impl<T1, F, I, M, O, E> Map<T1, F, I, M, O, E> {
    pub fn new(transform: T1, func: F) -> Self {
        Self {
            transform,
            func,
            _transform_ty: PhantomFunc::default(),
            _func_ty: PhantomFunc::default(),
        }
    }
}

#[derive(Debug)]
pub struct AndThen<T1, T2, I, M, O, E> {
    t1: T1,
    t2: T2,
    _t1_ty: PhantomFunc<I, Result<M, E>>,
    _t2_ty: PhantomFunc<M, Result<O, E>>,
}

impl<T1, T2, I, M, O, E> Transformer<I, O, E> for AndThen<T1, T2, I, M, O, E>
where
    T1: Transformer<I, M, E>,
    T2: Transformer<M, O, E>,
{
    fn transform(&mut self, input: I) -> Result<O, E> {
        let a = self.t1.transform(input)?;
        self.t2.transform(a)
    }
}

impl<T1, T2, I, M, O, E> AndThen<T1, T2, I, M, O, E> {
    pub fn new(t1: T1, t2: T2) -> Self {
        Self {
            t1,
            t2,
            _t1_ty: PhantomData,
            _t2_ty: PhantomData,
        }
    }
}
