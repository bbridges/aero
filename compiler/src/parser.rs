use std::process::exit;

use lazy_static::lazy_static;
use pest::{iterators::Pair, pratt_parser::PrattParser, Parser};

use crate::ast::{BindingOp, Def, Expr, Form, InfixOp, Pat, PrefixOp, Source, Span};

mod aero {
    // The pest_derive macro makes "Rule" public, so placing this into a nested
    // module to prevent it from being exposed.
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "aero.pest"]
    pub struct AeroParser;
}

use aero::{AeroParser, Rule};

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        let prefix = Op::<Rule>::prefix;
        let infix = Op::<Rule>::infix;
        let postfix = Op::<Rule>::postfix;

        PrattParser::new()
            .op(infix(BitOr, Left) | infix(BoolOr, Left) | infix(TyOr, Left))
            .op(infix(BitXor, Left))
            .op(infix(TyAnd, Left) | infix(BoolAnd, Left) | infix(TyAnd, Left))
            .op(infix(Eq, Left) | infix(Ne, Left))
            .op(infix(Lt, Left) | infix(Gt, Left) | infix(Le, Left) | infix(Ge, Left))
            .op(infix(BitShl, Left) | infix(BitShr, Left))
            .op(infix(Add, Left) | infix(Sub, Left))
            .op(infix(Mul, Left) | infix(Div, Left) | infix(Rem, Left))
            .op(infix(Pow, Left))
            .op(prefix(BitNot) | prefix(BoolNot) | prefix(Pos) | prefix(Neg) | prefix(Pin))
            .op(postfix(Opt) | postfix(Res))
    };
}

pub fn parse<'a>(input: &'a str) -> Source {
    let mut pairs = AeroParser::parse(Rule::Source, input).unwrap_or_else(|e| {
        eprintln!("{}", e);
        exit(1);
    });

    println!("{:#?}\n", pairs);

    let source_pair = pairs.next().unwrap();

    let span = get_span(&source_pair);
    let defs = source_pair
        .into_inner()
        .take_while(|p| p.as_rule() != Rule::EOI)
        .map(map_def)
        .collect();

    Source { span, defs }
}

fn map_def(pair: Pair<Rule>) -> Form<Def> {
    let span = get_span(&pair);
    let value = match pair.as_rule() {
        Rule::MainDef => {
            let body = pair.into_inner().next().map(map_expr);

            Def::Main { body }
        }
        _ => todo!("def not handled yet"),
    };

    Form { span, value }
}

fn map_expr(pair: Pair<Rule>) -> Form<Expr> {
    if pair.as_rule() == Rule::Block {
        let span = get_span(&pair);
        let exprs: Vec<_> = pair.into_inner().map(map_expr).collect();
        let value = Expr::Block { exprs };

        return Form { span, value };
    }

    let str = pair.as_str();
    let start = pair.as_span().start();

    if pair.as_rule() == Rule::BindingForm {
        // This will only happen when we're in a block. Bindings are at the same
        // level as infix_forms so it's handled here. The pratt parser always
        // will parse everything as an expression, so we manually parse all the
        // patterns.
        let mut binding_pairs = pair.into_inner().rev();
        let mut expr = map_expr(binding_pairs.next().unwrap());

        loop {
            if let Some(op_pair) = binding_pairs.next() {
                let op_span = get_span(&op_pair);
                let op_value = match op_pair.as_rule() {
                    Rule::Assign => BindingOp::Assign,
                    Rule::ArrowL => BindingOp::ArrowL,
                    _ => unreachable!(),
                };
                let op = Form {
                    span: op_span,
                    value: op_value,
                };

                let pat_pair = binding_pairs.next().unwrap();
                let pat = map_pat(pat_pair);

                // Must manually construct the span here.
                let expr_start = pat.span.range.0;
                let expr_stop = expr.span.range.1;
                let expr_span = Span {
                    str: &str[(expr_start as usize - start)..(expr_stop as usize - start)],
                    range: (expr_start, expr_stop),
                    pos: pat.span.pos,
                };
                let expr_value = Expr::Binding {
                    pat,
                    op,
                    expr: Box::new(expr),
                };
                expr = Form {
                    span: expr_span,
                    value: expr_value,
                };
            } else {
                return expr;
            }
        }
    }

    let mut expr = PRATT_PARSER
        .map_primary(map_expr_primary)
        .map_prefix(map_expr_prefix)
        .map_infix(map_expr_infix)
        .parse(pair.into_inner());

    // When creating prefix and infix expressions, we don't have access to the
    // whole span from the pairs. After parsing them, go back and fix the string
    // references using the ranges.
    fn fix_spans<'a>(expr: &mut Form<'a, Expr<'a>>, str: &'a str, start: usize) {
        if let Expr::Prefix { op: _, expr: inner } = &mut expr.value {
            expr.span.str =
                &str[(expr.span.range.0 as usize - start)..(expr.span.range.1 as usize - start)];

            fix_spans(inner, str, start);
        } else if let Expr::Infix { left, op: _, right } = &mut expr.value {
            expr.span.str =
                &str[(expr.span.range.0 as usize - start)..(expr.span.range.1 as usize - start)];

            fix_spans(left, str, start);
            fix_spans(right, str, start);
        }
    }

    fix_spans(&mut expr, str, start);
    expr
}

fn map_expr_primary(pair: Pair<Rule>) -> Form<Expr> {
    let span = get_span(&pair);
    let value = match pair.as_rule() {
        Rule::IntLit => {
            let int = pair
                .as_str()
                .parse::<u64>()
                .expect("failed to parse int literal");

            Expr::IntLit(int)
        }
        Rule::StrLit => {
            let mut content = String::new();

            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::StrSegment => content.push_str(p.as_str()),
                    _ => unreachable!(),
                }
            }

            Expr::StrLit(content)
        }
        Rule::Ident => Expr::Ident(String::from(pair.as_str())),
        Rule::LogExpr => {
            let mut expr_pairs = pair.into_inner();
            let expr = map_expr(expr_pairs.next().unwrap());

            if expr_pairs.next().is_some() {
                panic!("unexpected form in log")
            }

            Expr::Log {
                message: Box::new(expr),
            }
        }
        _ => todo!("primary expr not handled yet"),
    };

    Form { span, value }
}

fn map_expr_prefix<'a>(pair: Pair<'a, Rule>, expr: Form<'a, Expr<'a>>) -> Form<'a, Expr<'a>> {
    let op_span = get_span(&pair);
    let op_value = match pair.as_rule() {
        Rule::BitNot => PrefixOp::BitNot,
        Rule::BoolNot => PrefixOp::BoolNot,
        Rule::Pos => PrefixOp::Pos,
        Rule::Neg => PrefixOp::Neg,
        _ => panic!("unexpected prefix operator"),
    };

    let span = Span {
        str: "", // Determined after parsing whole pratt expression.
        range: (op_span.range.0, expr.span.range.1),
        pos: op_span.pos,
    };
    let value = Expr::Prefix {
        op: Form {
            span: op_span,
            value: op_value,
        },
        expr: Box::new(expr),
    };

    Form { span, value }
}

fn map_expr_infix<'a>(
    left: Form<'a, Expr<'a>>,
    pair: Pair<'a, Rule>,
    right: Form<'a, Expr<'a>>,
) -> Form<'a, Expr<'a>> {
    let op_span = get_span(&pair);
    let op_value = match pair.as_rule() {
        Rule::BitAnd => InfixOp::BitAnd,
        Rule::BitOr => InfixOp::BitOr,
        Rule::BitXor => InfixOp::BitXor,
        Rule::BitShl => InfixOp::BitShl,
        Rule::BitShr => InfixOp::BitShr,
        Rule::BoolAnd => InfixOp::BoolAnd,
        Rule::BoolOr => InfixOp::BoolOr,
        Rule::Eq => InfixOp::Eq,
        Rule::Ne => InfixOp::Ne,
        Rule::Le => InfixOp::Le,
        Rule::Ge => InfixOp::Ge,
        Rule::Lt => InfixOp::Lt,
        Rule::Gt => InfixOp::Gt,
        Rule::Add => InfixOp::Add,
        Rule::Sub => InfixOp::Sub,
        Rule::Mul => InfixOp::Mul,
        Rule::Div => InfixOp::Div,
        Rule::Rem => InfixOp::Rem,
        Rule::Pow => InfixOp::Pow,
        _ => panic!("unexpected infix operator"),
    };

    let span = Span {
        str: "", // Determined after parsing whole pratt expression.
        range: (left.span.range.0, right.span.range.1),
        pos: left.span.pos,
    };
    let value = Expr::Infix {
        left: Box::new(left),
        op: Form {
            span: op_span,
            value: op_value,
        },
        right: Box::new(right),
    };

    Form::<'a> { span, value }
}

fn map_pat(pair: Pair<Rule>) -> Form<Pat> {
    PRATT_PARSER
        .map_primary(map_pat_primary)
        .parse(pair.into_inner())
}

fn map_pat_primary(pair: Pair<Rule>) -> Form<Pat> {
    let span = get_span(&pair);
    let value = match pair.as_rule() {
        Rule::Ident => Pat::Ident(String::from(pair.as_str())),
        _ => todo!("pimary pat not yet handled"),
    };

    Form { span, value }
}

fn get_span<'a>(pair: &Pair<'a, Rule>) -> Span<'a> {
    let pair_span = pair.as_span();
    let pair_line_col = pair_span.start_pos().line_col();

    Span {
        str: pair.as_str(),
        range: (pair_span.start() as u32, pair_span.end() as u32),
        pos: (pair_line_col.0 as u32, pair_line_col.1 as u32),
    }
}
