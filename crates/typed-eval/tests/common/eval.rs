use typed_eval::{Compiler, EvalType, Result};

pub fn eval<'a, Ctx, Ret>(input: &str, ctx: &'a Ctx) -> Result<Ret::RefType<'a>>
where
    Ctx: EvalType,
    Ret: EvalType,
{
    let compiler = Compiler::<Ctx>::new()?;
    let compiled_fn = compiler.compile::<Ret>(input)?;
    Ok(compiled_fn(ctx))
}
