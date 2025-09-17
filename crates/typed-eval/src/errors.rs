use crate::Error;
use std::ops::Deref;

pub type Result<T> = std::result::Result<T, Errors>;

#[derive(Debug, PartialEq)]
pub struct Errors(Vec<Error>);

impl Deref for Errors {
    type Target = [Error];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Error> for Errors {
    fn from(value: Error) -> Self {
        Self(vec![value])
    }
}

impl From<Vec<Error>> for Errors {
    fn from(value: Vec<Error>) -> Self {
        Self(value)
    }
}

impl Errors {
    pub fn empty() -> Errors {
        Self(vec![])
    }

    pub fn combine(
        a: impl Into<Option<Errors>>,
        b: impl Into<Option<Errors>>,
    ) -> Option<Errors> {
        match (a.into(), b.into()) {
            (None, None) => None,
            (Some(e), None) | (None, Some(e)) => Some(e),
            (Some(mut e1), Some(e2)) => {
                e1.0.extend(e2.0);
                Some(e1)
            }
        }
    }

    pub fn append(&mut self, errors: impl Into<Errors>) {
        self.0.append(&mut errors.into().0);
    }
}

pub fn all_ok_vec<T>(results: Vec<Result<T>>) -> Result<Vec<T>> {
    let mut values = Vec::with_capacity(results.len());
    let mut errors: Option<Errors> = None;

    for res in results {
        match res {
            Ok(v) => values.push(v),
            Err(e) => {
                errors = Errors::combine(errors, Some(e));
            }
        }
    }

    if let Some(e) = errors {
        Err(e)
    } else {
        Ok(values)
    }
}

/// This trait is implemented for tuples, so it is possible to check multiple results
/// and collect all errors before bailing out: `let (a, b) = (compute_a(), compute_b()).all_ok()?`
pub trait CombineResults {
    type Output;
    fn all_ok(self) -> Result<Self::Output>;
}

macro_rules! impl_combine_results {
    ( $( $name:ident ),+ ) => {
        impl< $( $name ),+ > CombineResults for ( $( Result<$name> ),+ ) {
            type Output = ( $( $name ),+ );

            #[allow(non_snake_case)]
            fn all_ok(self) -> Result<Self::Output> {
                let ( $( $name ),+ ) = self;

                let mut errors: Option<Errors> = None;

                $(
                    let $name = match $name {
                        Ok(v) => Some(v),
                        Err(e) => {
                            errors = Errors::combine(errors, Some(e));
                            None
                        }
                    };
                )+

                if let Some(e) = errors {
                    Err(e)
                } else {
                    Ok(( $( $name.unwrap() ),+ ))
                }
            }
        }
    };
}
impl_combine_results!(A, B);
impl_combine_results!(A, B, C);
impl_combine_results!(A, B, C, D);
impl_combine_results!(A, B, C, D, E);
impl_combine_results!(A, B, C, D, E, F);
