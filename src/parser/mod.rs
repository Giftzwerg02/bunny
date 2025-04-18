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
                    prop_parse_invalid(Rule::int, &s)?;
                }

                #[test]
                fn num_with_invalid_prefix_is_not_int(s in "[a-zA-Z]+", num: u64) {
                    let p = format!("{s}{num}");
                    prop_parse_invalid(Rule::int, &p)?;
                }

                #[test]
                fn negation_is_not_int(num: u64) {
                    let negated = format!("-{num}");
                    prop_parse_invalid(Rule::int, &negated)?;
                }

                #[test]
                fn empty_is_not_int(s in "\\s*") {
                    prop_parse_invalid(Rule::int, &s)?;
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
                prop_parse_invalid(Rule::identifier, &s)?;
            }

            #[test]
            fn invalid_empty(s in "\\s*") {
                prop_parse_invalid(Rule::identifier, &s)?;
            }

            #[test]
            fn invalid_illegal(s in formatcp!(r"[\(\):{DIGIT}\[\]]+")) {
                prop_parse_invalid(Rule::identifier, &s)?;
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
        use std::fmt::Display;

        use pest::iterators::Pair;
        use text_trees::StringTreeNode;

        use crate::ast::{
            Argument, Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallList,
            FuncCallSingle, Int, NamedArgument, PrettyPrintable, StageInfo,
            Str, Symbol,
        };

        use super::*;

        #[derive(Clone, Debug, PartialEq)]
        struct EmptyStageInfo {}

        impl Display for EmptyStageInfo {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "")
            }
        }

        impl PrettyPrintable for EmptyStageInfo {
            fn pretty_print(&self) -> StringTreeNode {
                StringTreeNode::new("".to_string())
            }
        }

        impl StageInfo for EmptyStageInfo {}

        fn arb_expr(depth: u32) -> impl Strategy<Value = Expr<EmptyStageInfo>> {
            if depth == 0 {
                return prop_oneof![
                    any::<u64>()
                        .prop_map(|i| Int::new(i, EmptyStageInfo {}))
                        .prop_map(Expr::Int),
                    any::<f64>()
                        .prop_map(|f| f.abs())
                        .prop_map(|f| Float::new(f, EmptyStageInfo {}))
                        .prop_map(Expr::Float),
                    "[^\"]{0,100}"
                        .prop_map(|s| Str::new(s, EmptyStageInfo {}))
                        .prop_map(Expr::String),
                    arb_color().prop_map(Expr::Color),
                    arb_id().prop_map(Expr::Symbol)
                ]
                .boxed();
            }

            let leaf = prop_oneof![
                any::<u64>()
                    .prop_map(|i| Int::new(i, EmptyStageInfo {}))
                    .prop_map(Expr::Int),
                any::<f64>()
                    .prop_map(|f| f.abs())
                    .prop_map(|f| Float::new(f, EmptyStageInfo {}))
                    .prop_map(Expr::Float),
                "[^\"]{0,100}"
                    .prop_map(|s| Str::new(s, EmptyStageInfo {}))
                    .prop_map(Expr::String),
                arb_color().prop_map(Expr::Color),
                arb_id().prop_map(Expr::Symbol),
                arb_funccall(depth - 1).prop_map(Expr::FuncCall)
            ];
            leaf.prop_recursive(depth - 1, 25, 5, |inner| {
                prop_oneof![
                    prop::collection::vec(inner.clone(), 0..4)
                        .prop_map(|a| Array::new(a, EmptyStageInfo {}))
                        .prop_map(Expr::Array),
                    prop::collection::vec((inner.clone(), inner.clone()), 0..4)
                        .prop_map(|entries| {
                            let entries = entries
                                .into_iter()
                                .map(|(k, v)| DictEntry::new(k, v, EmptyStageInfo {}))
                                .collect::<Vec<_>>();
                            Dict::new(entries, EmptyStageInfo {})
                        })
                        .prop_map(Expr::Dict)
                ]
            })
            .boxed()
        }

        fn empty_func() -> FuncCall<EmptyStageInfo> {
            FuncCall::List(FuncCallList::new(vec![], EmptyStageInfo {}))
        }

        fn arb_funccall(depth: u32) -> impl Strategy<Value = FuncCall<EmptyStageInfo>> {
            if depth == 0 {
                return Just(empty_func()).boxed();
            }

            let leaf =
                prop_oneof![
                    Just(empty_func()),
                    (
                        arb_id(),
                        prop::collection::vec(arb_argument(depth - 1), 0..4)
                    )
                        .prop_map(|(id, args)| FuncCall::Single(
                            FuncCallSingle::new(id, args, EmptyStageInfo {})
                        ))
                ];

            leaf.prop_recursive(depth - 1, 25, 5, |inner| {
                prop::collection::vec(inner.clone(), 0..5)
                    .prop_map(|calls| FuncCall::List(FuncCallList::new(calls, EmptyStageInfo {})))
            })
            .boxed()
        }

        fn arb_argument(depth: u32) -> impl Strategy<Value = Argument<EmptyStageInfo>> {
            if depth == 0 {
                return arb_expr(0)
                    .prop_map(Argument::Positional)
                    .boxed();
            }

            prop_oneof![
                (arb_id(), arb_expr(depth - 1)).prop_map(|(id, expr)| Argument::Named(
                    NamedArgument::new(id, expr, EmptyStageInfo {})
                )),
                arb_expr(depth - 1).prop_map(Argument::Positional)
            ]
            .boxed()
        }

        fn arb_color() -> impl Strategy<Value = Color<EmptyStageInfo>> {
            any::<(u8, u8, u8)>().prop_map(|(r, g, b)| Color::new(r, g, b, EmptyStageInfo {}))
        }

        fn arb_id() -> impl Strategy<Value = Symbol<EmptyStageInfo>> {
            const LETTER: &str = "a-zA-Z";
            const DIGIT: &str = "0-9";
            const SPECIAL: &str = "\\+\\-\\*/\\$\\^§%&=`<>\\|_@~";

            const IDENTIFIER_REGEX: &str =
                formatcp!("[{LETTER}{SPECIAL}][{LETTER}{SPECIAL}{DIGIT}]*");

            IDENTIFIER_REGEX.prop_map(|s| Symbol::new(s, EmptyStageInfo {}))
        }

        proptest! {
            #[test]
            fn valid_expr(expr in arb_expr(16)) {
                // these 3 lines test more than you can ever imagine
                let input = expr.as_code();
                let mut pairs = prop_parse_valid(Rule::expr, &*input)?;
                check_expr_parse(expr, pairs.next().unwrap())?;
            }
        }

        fn check_argument_parse(
            arg: Argument<EmptyStageInfo>,
            pair: Pair<'_, Rule>,
        ) -> Result<(), TestCaseError> {
            let mut pairs = pair.into_inner();
            let pair = pairs.next().unwrap();
            if pair.as_rule() == Rule::expr {
                let Argument::Positional(expr) = arg else {
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

                check_expr_parse(*named.value, pairs.next().unwrap())?;
                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            prop_assert!(false);
            Ok(())
        }

        fn check_funccall_parse(
            func: FuncCall<EmptyStageInfo>,
            pair: Pair<'_, Rule>,
        ) -> Result<(), TestCaseError> {
            let pairs_count = pair.clone().into_inner().count();
            if pairs_count == 0 {
                match func {
                    FuncCall::List(list) if list.calls.is_empty() => {}
                    _ => {
                        prop_assert!(false);
                    }
                }

                return Ok(());
            }

            let mut pairs = pair.into_inner();
            let first_pair = pairs.next().unwrap();

            if first_pair.as_rule() == Rule::func_call {
                let FuncCall::List(list) = func else {
                    prop_assert!(false);
                    return Ok(());
                };

                check_funccall_parse(list.calls[0].clone(), first_pair)?;

                for call in &list.calls[1..] {
                    let p = pairs.next().unwrap();
                    check_funccall_parse(call.clone(), p)?;
                }

                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            if first_pair.as_rule() == Rule::identifier {
                let FuncCall::Single(single) = func else {
                    prop_assert!(false);
                    return Ok(());
                };
                
                let mut arg_pairs = pairs.next().expect("funcargs").into_inner();
                for arg in single.args {
                    let p = arg_pairs.next().unwrap();
                    check_argument_parse(arg, p)?;
                }

                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            prop_assert!(false, "actual: {first_pair}");
            Ok(())
        }

        fn check_expr_parse(
            expr: Expr<EmptyStageInfo>,
            pair: Pair<'_, Rule>,
        ) -> Result<(), TestCaseError> {
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
                    let Expr::Dict(dict) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    let mut pairs = pair.into_inner();

                    for entry in dict.value {
                        let mut map_assing = pairs.next().unwrap().into_inner();
                        let pair_key = map_assing.next().unwrap();
                        let pair_value = map_assing.next().unwrap();

                        prop_assert_eq!(pair_key.as_rule(), Rule::expr);
                        prop_assert_eq!(pair_value.as_rule(), Rule::expr);

                        check_expr_parse(entry.key, pair_key)?;
                        check_expr_parse(entry.value, pair_value)?;
                    }

                    prop_assert!(pairs.next().is_none());
                    Ok(())
                }
                Rule::dict_assign => {
                    prop_assert!(false, "should not be checked here");
                    Ok(())
                }
                Rule::array => {
                    let Expr::Array(array) = expr else {
                        prop_assert!(false, "expected Array, but got {} for {}", expr.name(), pair.as_str());
                        return Ok(());
                    };

                    let mut pairs = pair.into_inner();

                    for val in array.value {
                        let pair_value = pairs.next().unwrap();
                        prop_assert_eq!(pair_value.as_rule(), Rule::expr);
                        check_expr_parse(val, pair_value)?;
                    }

                    prop_assert!(pairs.next().is_none());
                    Ok(())
                }
                Rule::func_call => {
                    let Expr::FuncCall(func) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    check_funccall_parse(func, pair)
                }
                Rule::func_arguments | Rule::named_argument | Rule::positional_argument => {
                    prop_assert!(false, "should not be checked here");
                    Ok(())
                }
                Rule::identifier => {
                    let Expr::Symbol(_) = expr else {
                        prop_assert!(false, "expected Symbol, but got {} for {}", expr.name(), pair.as_str());
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::int => {
                    let Expr::Int(_) = expr else {
                        prop_assert!(false, "expected Int, but got {} for {}", expr.name(), pair.as_str());
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::float => {
                    let Expr::Float(_) = expr else {
                        prop_assert!(false, "expected Float, but got {} for {}", expr.name(), pair.as_str());
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::string => {
                    let Expr::String(_) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    Ok(())
                }
                Rule::color => {
                    let Expr::Color(_) = expr else {
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
            let parens = ['(', ')'];

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
