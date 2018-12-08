// Part of the Aero programming language. License: MIT.

//! Wrapper for Rust's `f64` utilities for use in Aero's `f64`.

#[macro_use]
extern crate aero_core;
#[macro_use]
extern crate rustler;

aero_export! {
  "aero_core_f64",
  [
    (floor,   f64::floor,   (f64),           f64),
    (ceil,    f64::ceil,    (f64),           f64),
    (round,   f64::round,   (f64),           f64),
    (trunc,   f64::trunc,   (f64),           f64),
    (fract,   f64::fract,   (f64),           f64),
    (abs,     f64::abs,     (f64),           f64),
    (mul_add, f64::mul_add, (f64, f64, f64), f64),
    (powi,    f64::powi,    (f64, i32),      f64),
    (powf,    f64::powf,    (f64, f64),      f64),
    (inv,     f64::recip,   (f64),           f64),
    (sqrt,    f64::sqrt,    (f64),           f64),
    (cbrt,    f64::cbrt,    (f64),           f64),
    (exp,     f64::exp,     (f64),           f64),
    (exp2,    f64::exp2,    (f64),           f64),
    (exp_m1,  f64::exp_m1,  (f64),           f64),
    (ln,      f64::ln,      (f64),           f64),
    (log,     f64::log,     (f64, f64),      f64),
    (log2,    f64::log2,    (f64),           f64),
    (log10,   f64::log10,   (f64),           f64),
    (ln_1p,   f64::ln_1p,   (f64),           f64),
    (hypot,   f64::hypot,   (f64, f64),      f64),
    (sin,     f64::sin,     (f64),           f64),
    (cos,     f64::cos,     (f64),           f64),
    (tan,     f64::tan,     (f64),           f64),
    (asin,    f64::asin,    (f64),           f64),
    (acos,    f64::acos,    (f64),           f64),
    (atan,    f64::atan,    (f64),           f64),
    (atan2,   f64::atan2,   (f64, f64),      f64),
    (sinh,    f64::sinh,    (f64),           f64),
    (cosh,    f64::cosh,    (f64),           f64),
    (tanh,    f64::tanh,    (f64),           f64),
    (asinh,   f64::asinh,   (f64),           f64),
    (acosh,   f64::acosh,   (f64),           f64),
    (atanh,   f64::atanh,   (f64),           f64)
  ],
  [
    ("floor",   1, floor),
    ("ceil",    1, ceil),
    ("round",   1, round),
    ("trunc",   1, trunc),
    ("fract",   1, fract),
    ("abs",     1, abs),
    ("mul_add", 3, mul_add),
    ("powi",    2, powi),
    ("powf",    2, powf),
    ("inv",     1, inv),
    ("sqrt",    1, sqrt),
    ("cbrt",    1, cbrt),
    ("exp",     1, exp),
    ("exp2",    1, exp2),
    ("exp_m1",  1, exp_m1),
    ("ln",      1, ln),
    ("log",     2, log),
    ("log2",    1, log2),
    ("log10",   1, log10),
    ("ln_1p",   1, ln_1p),
    ("hypot",   2, hypot),
    ("sin",     1, sin),
    ("cos",     1, cos),
    ("tan",     1, tan),
    ("asin",    1, asin),
    ("acos",    1, acos),
    ("atan",    1, atan),
    ("atan2",   2, atan2),
    ("sinh",    1, sinh),
    ("cosh",    1, cosh),
    ("tanh",    1, tanh),
    ("asinh",   1, asinh),
    ("acosh",   1, acosh),
    ("atanh",   1, atanh)
  ]
}