use crate::Compiler;

pub trait SupportedType {
    fn register<Ctx>(compiler: &mut Compiler<Ctx>);
}
