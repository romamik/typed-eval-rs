use typed_eval::{Compiler, EvalType, parse_expr};

pub fn eval<'a, Ctx, Ret>(
    input: &str,
    ctx: &'a Ctx,
) -> Result<Ret::RefType<'a>, String>
where
    Ctx: EvalType,
    Ret: EvalType,
{
    let expr = parse_expr(input);
    if expr.has_errors() || !expr.has_output() {
        let errors = expr
            .errors()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(",");
        Err(format!("Error parsing expression: {}", errors))?;
    }

    let compiler = Compiler::<Ctx>::new().unwrap();
    let compiled_expr = compiler.compile::<Ret>(expr.output().unwrap())?;
    Ok(compiled_expr(ctx))
}
