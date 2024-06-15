#[derive(Debug)]
pub struct Source<'a> {
    pub span: Span<'a>,
    pub defs: Vec<Form<'a, Def<'a>>>,
}

#[derive(Debug)]
pub enum Def<'a> {
    Main { body: Option<Form<'a, Expr<'a>>> },
}

#[derive(Debug)]
pub enum Expr<'a> {
    IntLit(u64),
    StrLit(String),
    Ident(String),
    Log {
        message: Box<Form<'a, Expr<'a>>>,
    },
    Prefix {
        op: Form<'a, PrefixOp>,
        value: Box<Form<'a, Expr<'a>>>,
    },
    Infix {
        left: Box<Form<'a, Expr<'a>>>,
        op: Form<'a, InfixOp>,
        right: Box<Form<'a, Expr<'a>>>,
    },
    Postfix {
        value: Box<Form<'a, Expr<'a>>>,
        op: Form<'a, PostfixOp>,
    },
    Block {
        exprs: Vec<Form<'a, Expr<'a>>>,
    },
    Assign {
        pat: Form<'a, Pat>,
        expr: Box<Form<'a, Expr<'a>>>,
    }
}

#[derive(Debug)]
pub enum Pat {
    Ident(String),
}

#[derive(Debug)]
pub struct Form<'a, T> {
    pub span: Span<'a>,
    pub value: T,
}

#[derive(Debug)]
pub struct Span<'a> {
    pub str: &'a str,
    pub range: (u32, u32),
    pub pos: (u32, u32),
}

#[derive(Debug)]
pub enum PrefixOp {
    BitNot,
    Pos,
    Neg,
    Pin,
    Unquote,
}

#[derive(Debug)]
pub enum InfixOp {
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
    Assoc,
    ArrowL,
    ArrowR,
    Eq,
    Ne,
    Le,
    Ge,
    Pow,
    Concat,
    Assign,
    Pipe,
    Amp,
    Lt,
    Gt,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug)]
pub enum PostfixOp {
    Res,
    Opt,
}
