use std::process::exit;

use lazy_static::lazy_static;
use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};

use crate::ast::{Def, Expr, Form, InfixOp, Source, Span};

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
        PrattParser::new()
            .op(Op::infix(Rule::assign, Assoc::Right))
            .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
            .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
    };
}

pub fn parse<'a>(input: &'a str) -> Source {
    let mut pairs = AeroParser::parse(Rule::source, input).unwrap_or_else(|e| {
        eprintln!("{}", e);
        exit(1);
    });

    // println!("{:#?}", pairs);

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
        Rule::main_def => {
            let body = pair.into_inner().next().map(map_expr);

            Def::Main { body }
        }
        _ => todo!("def not handled yet"),
    };

    Form { span, value }
}

fn map_expr(pair: Pair<Rule>) -> Form<Expr> {
    let span = get_span(&pair);
    match pair.as_rule() {
        Rule::form | Rule::infix_form => {
            // The infix rules won't include infixes, but we can still use the
            // same pratt parser.
            map_pratt_expr(pair, false)
        }
        Rule::block => {
            let exprs: Vec<_> = pair
                .into_inner()
                .map(|pair| map_pratt_expr(pair, true))
                .collect();
            let value = Expr::Block { exprs };

            Form { span, value }
        }
        _ => unreachable!("invalid expr pair"),
    }
}

fn map_pratt_expr(pair: Pair<Rule>, in_block: bool) -> Form<Expr> {
    let pratt_str = pair.as_str();
    let pratt_start = pair.as_span().start();

    let mut expr = PRATT_PARSER
        .map_primary(map_pratt_expr_primary)
        .map_infix(if in_block {
            map_pratt_block_infix
        } else {
            map_pratt_expr_infix
        })
        .parse(pair.into_inner());

    // When creating infix expressions, we don't have access to the whole span
    // from the pairs. After parsing them, go back and fix the string references
    // using the ranges.
    fn fix_infix_spans<'a>(expr: &mut Form<'a, Expr<'a>>, str: &'a str, start: usize) {
        if let Expr::Infix { left, op: _, right } = &mut expr.value {
            expr.span.str =
                &str[(expr.span.range.0 as usize - start)..(expr.span.range.1 as usize - start)];

            fix_infix_spans(left, str, start);
            fix_infix_spans(right, str, start);
        }
    }

    fix_infix_spans(&mut expr, pratt_str, pratt_start);
    expr
}

fn map_pratt_expr_primary(pair: Pair<Rule>) -> Form<Expr> {
    let span = get_span(&pair);
    let value = match pair.as_rule() {
        Rule::int_lit => {
            let int = pair
                .as_str()
                .parse::<u64>()
                .expect("failed to parse int literal");

            Expr::IntLit(int)
        }
        Rule::str_lit => {
            let mut content = String::new();

            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::str_segment => content.push_str(p.as_str()),
                    _ => unreachable!(),
                }
            }

            Expr::StrLit(content)
        }
        Rule::ident => {
            Expr::Ident(String::from(pair.as_str()))
        }
        Rule::log_expr => {
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

fn map_pratt_expr_infix<'a>(
    left: Form<'a, Expr<'a>>,
    pair: Pair<'a, Rule>,
    right: Form<'a, Expr<'a>>,
) -> Form<'a, Expr<'a>> {
    let op_span = get_span(&pair);
    let op_value = match pair.as_rule() {
        Rule::bit_and => InfixOp::BitAnd,
        Rule::bit_or => InfixOp::BitOr,
        Rule::bit_xor => InfixOp::BitXor,
        Rule::bit_shl => InfixOp::BitShl,
        Rule::bit_shr => InfixOp::BitShr,
        Rule::eq => InfixOp::Eq,
        Rule::ne => InfixOp::Ne,
        Rule::le => InfixOp::Le,
        Rule::ge => InfixOp::Ge,
        Rule::pow => InfixOp::Pow,
        Rule::concat => InfixOp::Concat,
        Rule::lt => InfixOp::Lt,
        Rule::gt => InfixOp::Gt,
        Rule::add => InfixOp::Add,
        Rule::sub => InfixOp::Sub,
        Rule::mul => InfixOp::Mul,
        Rule::div => InfixOp::Div,
        Rule::rem => InfixOp::Rem,
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

fn map_pratt_block_infix<'a>(
    left: Form<'a, Expr<'a>>,
    pair: Pair<'a, Rule>,
    right: Form<'a, Expr<'a>>,
) -> Form<'a, Expr<'a>> {
    if pair.as_rule() == Rule::assign {
        unimplemented!()
    } else {
        map_pratt_expr_infix(left, pair, right)
    }
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
