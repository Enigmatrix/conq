#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    // String,
    // Void,
    Function {
        args: Vec<Type>,
        ret: Option<Box<Type>>,
    },
}