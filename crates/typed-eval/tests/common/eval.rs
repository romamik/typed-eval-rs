use typed_eval::{Compiler, EvalType};

pub fn test_eval<'a, Ctx, Ret>(
    input: &str,
    ctx: &'a Ctx,
    expected: Result<Ret::RefType<'a>, ()>,
) where
    Ctx: EvalType,
    Ret: EvalType,
    Ret::RefType<'a>: std::fmt::Debug + PartialEq,
{
    let result = eval::<Ctx, Ret>(input, ctx).map_err(|_| ());
    assert_eq!(result, expected)
}

pub fn eval<'a, Ctx, Ret>(
    input: &str,
    ctx: &'a Ctx,
) -> Result<Ret::RefType<'a>, String>
where
    Ctx: EvalType,
    Ret: EvalType,
{
    let compiler = Compiler::<Ctx>::new()?;
    let compiled_fn = compiler.compile::<Ret>(input)?;
    Ok(compiled_fn(ctx))
}
