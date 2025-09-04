mod expr;
mod expr_parser;

pub use expr::*;
pub use expr_parser::*;

pub fn eval_expr(expr: &Expr) -> Result<f64, String> {
    Ok(match expr {
        Expr::Int(val) => *val as f64,
        Expr::Float(val) => *val,
        Expr::String(_string) => Err("Strings not supported")?,
        Expr::Var(_var_name) => Err("Variables not supported")?,
        Expr::UnOp(op, rhs) => {
            let rhs = eval_expr(rhs)?;
            match op {
                UnOp::Neg => -rhs,
                UnOp::Plus => rhs,
            }
        }
        Expr::BinOp(op, lhs, rhs) => {
            let lhs = eval_expr(lhs)?;
            let rhs = eval_expr(rhs)?;
            match op {
                BinOp::Add => lhs + rhs,
                BinOp::Sub => lhs - rhs,
                BinOp::Mul => lhs * rhs,
                BinOp::Div => lhs / rhs,
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
    eval_expr(expr.output().unwrap())
}

#[cfg(test)]
mod tests {
    use crate::eval;

    #[test]
    fn test_eval() {
        assert_eq!(eval("(1 + 2) * 3"), Ok(9.0));
    }
}
