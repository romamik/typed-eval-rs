mod expr;
mod expr_parser;

pub use expr::*;
pub use expr_parser::*;

pub type CompiledExpr = Box<dyn Fn() -> f64>;

pub fn compile_expr(expr: &Expr) -> Result<CompiledExpr, String> {
    Ok(match expr {
        &Expr::Int(val) => Box::new(move || val as f64),
        &Expr::Float(val) => Box::new(move || val),
        Expr::String(_string) => Err("Strings not supported")?,
        Expr::Var(_var_name) => Err("Variables not supported")?,
        Expr::UnOp(op, rhs) => {
            let rhs = compile_expr(rhs)?;
            match op {
                UnOp::Neg => Box::new(move || -rhs()),
                UnOp::Plus => rhs,
            }
        }
        Expr::BinOp(op, lhs, rhs) => {
            let lhs = compile_expr(lhs)?;
            let rhs = compile_expr(rhs)?;
            match op {
                BinOp::Add => Box::new(move || lhs() + rhs()),
                BinOp::Sub => Box::new(move || lhs() - rhs()),
                BinOp::Mul => Box::new(move || lhs() * rhs()),
                BinOp::Div => Box::new(move || lhs() / rhs()),
            }
        }
        Expr::FieldAccess(_object, _field_name) => Err("Field access not supported")?,
        Expr::FuncCall(_function, _arguments) => Err("Function calls not supported")?,
    })
}

pub fn eval(input: &str) -> Result<f64, String> {
    let expr = parse_expr(input);
    if expr.has_errors() || !expr.has_output() {
        let errors = expr
            .errors()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(",");
        Err(format!("Error parsing expression: {}", errors))?
    }
    let compiled_expr = compile_expr(expr.output().unwrap())?;
    Ok(compiled_expr())
}

#[cfg(test)]
mod tests {
    use crate::eval;

    #[test]
    fn test_eval() {
        assert_eq!(eval("(1 + 2) * 3"), Ok(9.0));
    }
}
