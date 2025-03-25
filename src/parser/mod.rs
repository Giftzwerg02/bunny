#[allow(unused)]
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct BunnyParser;

#[cfg(test)]
mod tests {
    // NOTE: put imports here to use them in sub-modules via `use super::*`

    use super::*;
    use const_format::formatcp;
    use pest::{error::Error, iterators::Pairs};
    use proptest::prelude::*;

    fn parse_valid<'a, S: Into<&'a str> + std::fmt::Display>(
        rule: Rule,
        input: S,
    ) -> Pairs<'a, Rule> {
        let input = input.into();
        let res = BunnyParser::parse(rule, input);
        assert!(
            res.is_ok(),
            "Failed to parse \"{input}\" on rule: {rule:#?}:\n{}\n{:#?}",
            res.clone().unwrap_err(),
            res.unwrap_err()
        );
        res.expect("we just checked this...")
    }

    fn parse_invalid(rule: Rule, input: &str) -> Error<Rule> {
        let res = BunnyParser::parse(rule, input);
        assert!(
            res.is_err(),
            "Illegally parsed \"{input}\" on rule: {rule:#?}: {}",
            res.unwrap()
        );
        res.expect_err("we just checked this...")
    }

    fn prop_parse_valid<'a, S: Into<&'a str> + std::fmt::Display>(
        rule: Rule,
        input: S,
    ) -> Result<Pairs<'a, Rule>, TestCaseError> {
        let input = input.into();
        let res = BunnyParser::parse(rule, input);
        prop_assert!(
            res.is_ok(),
            "Failed to parse \"{input}\" on rule: {rule:#?}:\n{}\n{:#?}",
            res.clone().unwrap_err(),
            res.unwrap_err()
        );
        Ok(res.expect("we just checked this..."))
    }

    fn prop_parse_invalid(rule: Rule, input: &str) -> Result<Error<Rule>, TestCaseError> {
        let res = BunnyParser::parse(rule, input);
        prop_assert!(
            res.is_err(),
            "Illegally parsed \"{input}\" on rule: {rule:#?}: {:#?}",
            res.unwrap()
        );
        Ok(res.expect_err("we just checked this..."))
    }

    mod constants {
        use super::*;

        mod int {
            use super::*;

            proptest! {
                #[test]
                fn any_uint_is_valid(int: u64){
                    let int_str = int.to_string();
                    let p = prop_parse_valid(Rule::int, &*int_str)?;
                    prop_assert_eq!(p.as_str(), &int_str);
                }

                #[test]
                fn ascii_letter_string_is_not_int(s in "[a-zA-Z]+") {
                    prop_parse_invalid(Rule::int, &*s)?;
                }

                #[test]
                fn num_with_invalid_prefix_is_not_int(s in "[a-zA-Z]+", num: u64) {
                    let p = format!("{s}{num}");
                    prop_parse_invalid(Rule::int, &*p)?;
                }

                #[test]
                fn negation_is_not_int(num: u64) {
                    let negated = format!("-{num}");
                    prop_parse_invalid(Rule::int, &*negated)?;
                }

                #[test]
                fn empty_is_not_int(s in "\\s*") {
                    prop_parse_invalid(Rule::int, &*s)?;
                }
            }
        }

        mod float {
            use super::*;

            proptest! {
                #[test]
                fn valid_floats_without_decimal(int: u64) {
                    let input = format!("{int}f");
                    let p = prop_parse_valid(Rule::float, &*input)?;
                    prop_assert_eq!(p.as_str(), &input);
                }

                #[test]
                fn valid_floats_with_decimal_point(int: u64) {
                    let input = format!("{int}.f");
                    let p = prop_parse_valid(Rule::float, &*input)?;
                    prop_assert_eq!(p.as_str(), &input);
                }

                #[test]
                fn valid_floats_all(pre: u64, post: u64) {
                    let input = format!("{pre}.{post}f");
                    let p = prop_parse_valid(Rule::float, &*input)?;
                    prop_assert_eq!(p.as_str(), &input);
                }

                #[test]
                fn invalid_floats_with_invalid_prefix_without_decimal(s in "[a-zA-Z]+", int: u64) {
                    let input = format!("{s}{int}f");
                    prop_parse_invalid(Rule::float, &input)?;
                }

                #[test]
                fn invalid_floats_with_invalid_prefix_with_decimal_point(s in "[a-zA-Z]+", int: u64) {
                    let input = format!("{s}{int}.f");
                    prop_parse_invalid(Rule::float, &input)?;
                }

                #[test]
                fn invalid_floats_with_invalid_prefix_valid_floats_all(s in "[a-zA-Z]+", pre: u64, post: u64) {
                    let input = format!("{s}{pre}.{post}f");
                    prop_parse_invalid(Rule::float, &input)?;
                }

                #[test]
                fn invalid_floats_missing_integer_part(int: u64) {
                    let input = format!(".{int}f");
                    prop_parse_invalid(Rule::float, &input)?;
                }

                #[test]
                fn invalid_floats_empty(s in "\\s*") {
                    prop_parse_invalid(Rule::float, &s)?;
                }
            }
        }

        mod string {
            use super::*;

            proptest! {
                #[test]
                fn valid_anything_between_double_quotes(s in "[^\"]*") {
                    let input = format!("\"{s}\"");
                    let p = parse_valid(Rule::string, &*input);
                    prop_assert_eq!(p.as_str(), &input);
                }

                #[test]
                fn invalid_anything_not_between_double_quotes(s in "[^\"]") {
                    prop_parse_invalid(Rule::string, &s)?;
                }

                #[test]
                fn invalid_missing_double_quote(s in "[^\"]") {
                    prop_parse_invalid(Rule::string, &format!("\"{s}"))?;
                    prop_parse_invalid(Rule::string, &format!("{s}\""))?;
                }
            }
        }
    }

    mod identifier {
        use super::*;

        const LETTER: &str = "a-zA-Z";
        const DIGIT: &str = "0-9";
        const SPECIAL: &str = "\\+\\-\\*/\\$\\^§%&=`<>\\|_@~";

        const IDENTIFIER_REGEX: &str = formatcp!("[{LETTER}{SPECIAL}][{LETTER}{SPECIAL}{DIGIT}]*");

        proptest! {
            #[test]
            fn valid(s in IDENTIFIER_REGEX) {
                let p = prop_parse_valid(Rule::identifier, &*s)?;
                prop_assert_eq!(p.as_str(), &s);
            }


            #[test]
            fn invalid_start_with_digit(s in formatcp!("{DIGIT}{IDENTIFIER_REGEX}")) {
                prop_parse_invalid(Rule::identifier, &*s)?;
            }

            #[test]
            fn invalid_empty(s in "\\s*") {
                prop_parse_invalid(Rule::identifier, &*s)?;
            }

            #[test]
            fn invalid_illegal(s in formatcp!("[\\(\\)\\\\:{DIGIT}\\[\\]]+")) {
                prop_parse_invalid(Rule::identifier, &*s)?;
            }
        }
    }

    mod color {
        use super::*;

        const COLOR: &str = "#[0-9a-fA-F]{6}";

        proptest! {
            #[test]
            fn valid(s in COLOR) {
                let p = prop_parse_valid(Rule::color, &*s)?;
                prop_assert_eq!(p.as_str(), &s);
            }

            #[test]
            fn invalid(color in formatcp!("({COLOR})?"), other in ".+") {
                prop_parse_invalid(Rule::color, &format!("{other}{color}"))?;
            }
        }
    }

    mod expr {
        use std::fmt::{Debug, Display};

        use pest::iterators::Pair;

        use crate::ast::{
            AST, Expr,
            expressions::{Argument, Color, FuncCall, Id, NamedArgument, NonEmptyFuncCall},
        };

        use super::*;

        #[derive(Clone, Debug, PartialEq)]
        struct SimpleExpr {
            inner: Box<AST<SimpleExpr>>,
        }

        impl Expr for SimpleExpr {}

        impl Display for SimpleExpr {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.inner)
            }
        }

        impl SimpleExpr {
            fn new(inner: AST<SimpleExpr>) -> Self {
                Self {
                    inner: Box::new(inner),
                }
            }
        }

        fn arb_expr(depth: u32) -> impl Strategy<Value = SimpleExpr> {
            if depth == 0 {
                return prop_oneof![
                    any::<u64>().prop_map(AST::Int).prop_map(SimpleExpr::new),
                    any::<f64>().prop_map(|f| f.abs()).prop_map(AST::Float).prop_map(SimpleExpr::new),
                    "[^\"]{0,100}"
                        .prop_map(AST::String)
                        .prop_map(SimpleExpr::new),
                    arb_color().prop_map(AST::Color).prop_map(SimpleExpr::new),
                    arb_id().prop_map(AST::Id).prop_map(SimpleExpr::new),
                ]
                .boxed();
            }

            let leaf = prop_oneof![
                any::<u64>().prop_map(AST::Int).prop_map(SimpleExpr::new),
                any::<f64>().prop_map(|f| f.abs()).prop_map(AST::Float).prop_map(SimpleExpr::new),
                "[^\"]{0,100}"
                    .prop_map(AST::String)
                    .prop_map(SimpleExpr::new),
                arb_color().prop_map(AST::Color).prop_map(SimpleExpr::new),
                arb_id().prop_map(AST::Id).prop_map(SimpleExpr::new),
                arb_funccall(depth - 1)
                    .prop_map(AST::FuncCall)
                    .prop_map(SimpleExpr::new),
            ];
            leaf.prop_recursive(depth - 1, 25, 5, |inner| {
                prop_oneof![
                    prop::collection::vec(inner.clone(), 0..4)
                        .prop_map(AST::Array)
                        .prop_map(SimpleExpr::new),
                    prop::collection::vec((inner.clone(), inner.clone()), 0..4)
                        .prop_map(AST::Dict)
                        .prop_map(SimpleExpr::new),
                ]
            })
            .boxed()
        }

        fn arb_funccall(depth: u32) -> impl Strategy<Value = FuncCall<SimpleExpr>> {
            if depth == 0 {
                return Just(FuncCall::Empty).boxed();
            }

            prop_oneof![
                Just(FuncCall::Empty),
                (
                    arb_id(),
                    prop::collection::vec(arb_argument(depth - 1), 0..4)
                )
                    .prop_map(|(id, args)| FuncCall::Call(NonEmptyFuncCall::new(id, args)))
            ].boxed()
        }

        fn arb_argument(depth: u32) -> impl Strategy<Value = Argument<SimpleExpr>> {
            if depth == 0 {
                return arb_expr(0).prop_map(Argument::Unnamed).boxed();
            }

            prop_oneof![
                (arb_id(), arb_expr(depth - 1))
                    .prop_map(|(id, expr)| Argument::Named(NamedArgument::new(id, expr))),
                arb_expr(depth - 1).prop_map(Argument::Unnamed)
            ]
            .boxed()
        }

        fn arb_color() -> impl Strategy<Value = Color> {
            any::<(u8, u8, u8)>().prop_map(|(r, g, b)| Color::new(r, g, b))
        }

        fn arb_id() -> impl Strategy<Value = Id> {
            const LETTER: &str = "a-zA-Z";
            const DIGIT: &str = "0-9";
            const SPECIAL: &str = "\\+\\-\\*/\\$\\^§%&=`<>\\|_@~";

            const IDENTIFIER_REGEX: &str =
                formatcp!("[{LETTER}{SPECIAL}][{LETTER}{SPECIAL}{DIGIT}]*");

            IDENTIFIER_REGEX.prop_map(Id::new)
        }

        proptest! {
            #[test]
            fn valid_expr(expr in arb_expr(16)) {
                // these 3 lines test more than you can ever imagine
                let input = format!("{expr}");
                let mut pairs = prop_parse_valid(Rule::expr, &*input)?;
                check_expr_parse(expr, pairs.next().unwrap())?;
            }
        }

        fn check_argument_parse(arg: Argument<SimpleExpr>, pair: Pair<'_, Rule>) -> Result<(), TestCaseError> {
            let mut pairs = pair.into_inner();
            let pair = pairs.next().unwrap();
            if pair.as_rule() == Rule::expr {
                let Argument::Unnamed(expr) = arg else {
                    prop_assert!(false);
                    return Ok(());
                };

                check_expr_parse(expr, pair)?;
                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            if pair.as_rule() == Rule::identifier {
                let Argument::Named(named) = arg else {
                    prop_assert!(false);
                    return Ok(());
                };

                check_expr_parse(named.expr, pairs.next().unwrap())?;
                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            prop_assert!(false);
            Ok(())
        }

        fn check_funccall_parse(func: FuncCall<SimpleExpr>, pair: Pair<'_, Rule>) -> Result<(), TestCaseError> {
            let pairs_count = pair.clone().into_inner().count();
            if pairs_count == 0 {
                match func {
                    FuncCall::Empty => {}
                    _ => {
                        prop_assert!(false);
                    }
                }

                return Ok(());
            }

            let mut pairs = pair.into_inner();
            let first_pair = pairs.next().unwrap();

            if first_pair.as_rule() == Rule::identifier {
                let FuncCall::Call(func) = func else {
                    prop_assert!(false);
                    return Ok(());
                };

                for arg in func.args {
                    let p = pairs.next().unwrap();
                    check_argument_parse(arg, p)?;
                }

                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            prop_assert!(false);
            Ok(())
        }

        fn check_expr_parse(expr: SimpleExpr, pair: Pair<'_, Rule>) -> Result<(), TestCaseError> {
            match pair.as_rule() {
                Rule::EOI
                | Rule::WHITESPACE
                | Rule::COMMENT
                | Rule::line_comment
                | Rule::special_char => Ok(()),
                Rule::program => {
                    prop_assert!(false, "should never happen");
                    Ok(())
                }
                Rule::expr => {
                    let mut pairs = pair.into_inner();
                    let next = pairs.next().unwrap();
                    check_expr_parse(expr, next)?;
                    prop_assert!(pairs.next().is_none());
                    Ok(())
                }
                Rule::dict => {
                    let AST::Dict(dict) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    let mut pairs = pair.into_inner();

                    for (k, v) in dict {
                        let mut map_assing = pairs.next().unwrap().into_inner();
                        let pair_key = map_assing.next().unwrap();
                        let pair_value = map_assing.next().unwrap();

                        prop_assert_eq!(pair_key.as_rule(), Rule::expr);
                        prop_assert_eq!(pair_value.as_rule(), Rule::expr);

                        check_expr_parse(k, pair_key)?;
                        check_expr_parse(v, pair_value)?;
                    }

                    prop_assert!(pairs.next().is_none());
                    Ok(())
                }
                Rule::dict_assign => {
                    prop_assert!(false, "should not be checked here");
                    Ok(())
                }
                Rule::array => {
                    let AST::Array(array) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    let mut pairs = pair.into_inner();

                    for val in array {
                        let pair_value = pairs.next().unwrap();
                        prop_assert_eq!(pair_value.as_rule(), Rule::expr);
                        check_expr_parse(val, pair_value)?;
                    }

                    prop_assert!(pairs.next().is_none());
                    Ok(())
                }
                Rule::func_call => {
                    let AST::FuncCall(func) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    check_funccall_parse(func, pair)
                }
                Rule::argument => {
                    prop_assert!(false, "should not be checked here");
                    Ok(())
                }
                Rule::identifier => {
                    let AST::Id(_) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::int => {
                    let AST::Int(_) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::float => {
                    let AST::Float(_) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::string => {
                    let AST::String(_) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::color => {
                    let AST::Color(_) = *expr.inner else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    Ok(())
                }
            }
        }
    }

    mod full {
        use super::*;

        #[test]
        fn simple_example_test() {
            let input = include_str!("./examples/simple.bny");
            parse_valid(Rule::program, input);
        }

        #[test]
        fn simple_example_chaos_test() {
            // removes every paren once in the input string (cardinal sin of a lisp programmer)
            // since only one paren is removed at a time, a mismatch must be given, resulting in a
            // parsing-failure

            let input = include_str!("./examples/simple.bny");
            let parens = vec!['(', ')'];

            let to_remove_opts = input
                .char_indices()
                .filter(|(_, c)| parens.contains(c))
                .collect::<Vec<_>>();

            for (idx, _) in to_remove_opts {
                let res = input
                    .char_indices()
                    .filter_map(|(i, c)| if i == idx { None } else { Some(c) })
                    .collect::<String>();

                assert_ne!(input, res); // sanity check
                parse_invalid(Rule::program, &res);
            }
        }
    }
}
