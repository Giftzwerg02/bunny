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
        use std::fmt::Display;

        use pest::iterators::Pair;

        use super::*;

        #[derive(Clone, Debug, PartialEq, Eq)]
        enum Expr {
            Int(u64),
            Float((u64, Option<u64>)),
            String(String),
            Color(Color),
            Id(Id),
            FuncCall(FuncCall),
            Array(Vec<Expr>),
            Map(Vec<(Expr, Expr)>),
        }

        fn arb_expr(depth: u32) -> impl Strategy<Value = Expr> {
            if depth == 0 {
                return prop_oneof![
                    any::<u64>().prop_map(Expr::Int),
                    any::<(u64, Option<u64>)>().prop_map(Expr::Float),
                    "[^\"]{0,100}".prop_map(Expr::String),
                    arb_color().prop_map(Expr::Color),
                    arb_id().prop_map(Expr::Id),
                ].boxed();
            }

            let leaf = prop_oneof![
                any::<u64>().prop_map(Expr::Int),
                any::<(u64, Option<u64>)>().prop_map(Expr::Float),
                "[^\"]{0,100}".prop_map(Expr::String),
                arb_color().prop_map(Expr::Color),
                arb_id().prop_map(Expr::Id),
                arb_funccall(depth - 1).prop_map(Expr::FuncCall),
            ];
            leaf.prop_recursive(
                depth - 1,
                25,
                5,
                |inner| {
                prop_oneof![
                    prop::collection::vec(inner.clone(), 0..4).prop_map(Expr::Array),
                    prop::collection::vec((inner.clone(), inner.clone()), 0..4)
                        .prop_map(Expr::Map),
                ]
            })
            .boxed()
        }

        impl Display for Expr {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Expr::Int(int) => write!(f, "{int}"),
                    Expr::Float((a, b)) => match b {
                        Some(b) => write!(f, "{a}.{b}f"),
                        None => write!(f, "{a}f"),
                    },
                    Expr::String(s) => write!(f, "\"{s}\""),
                    Expr::Array(exprs) => {
                        let str = exprs.iter().map(|e| format!("{e}")).collect::<Vec<_>>();
                        let str = str.join(" ");
                        write!(f, "[ {str} ]")
                    }
                    Expr::Map(items) => {
                        let str = items
                            .iter()
                            .map(|(k, v)| format!("{k} : {v}"))
                            .collect::<Vec<_>>();
                        let str = str.join(" ");
                        write!(f, "{{ {str} }}")
                    }
                    Expr::FuncCall(func_call) => {
                        write!(f, "{func_call}")
                    }
                    Expr::Color(color) => {
                        write!(f, "{color}")
                    }
                    Expr::Id(id) => write!(f, "{id}"),
                }
            }
        }

        #[derive(Clone, Debug, PartialEq, Eq)]
        enum FuncCall {
            Empty,
            Nested(Vec<FuncCall>),
            Single((Id, Vec<Argument>)),
        }

        fn arb_funccall(depth: u32) -> impl Strategy<Value = FuncCall> {
            if depth == 0 {
                return Just(FuncCall::Empty).boxed();
            }

            let leaf = prop_oneof![
                Just(FuncCall::Empty),
                (
                    arb_id(),
                    prop::collection::vec(arb_argument(depth - 1), 0..4)
                )
                    .prop_map(FuncCall::Single)
            ];

            leaf.prop_recursive(depth - 1, 25, 5, |inner| {
                prop_oneof![prop::collection::vec(inner.clone(), 0..4).prop_map(FuncCall::Nested)]
            }).boxed()
        }

        impl Display for FuncCall {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    FuncCall::Empty => write!(f, "()"),
                    FuncCall::Nested(func_calls) => {
                        let str = func_calls
                            .iter()
                            .map(|f| format!("{f}"))
                            .collect::<Vec<_>>();
                        let str = str.join(" ");
                        write!(f, "( {str} )")
                    }
                    FuncCall::Single((id, args)) => {
                        let str = args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>();
                        let str = str.join(" ");
                        write!(f, "( {} {str} )", id.value)
                    }
                }
            }
        }

        #[derive(Clone, Debug, PartialEq, Eq)]
        enum Argument {
            Named((Id, Expr)),
            Unnamed(Expr),
        }

        fn arb_argument(depth: u32) -> impl Strategy<Value = Argument> {
            if depth == 0 {
                return arb_expr(0).prop_map(Argument::Unnamed).boxed();
            }

            prop_oneof![
                (arb_id(), arb_expr(depth - 1)).prop_map(Argument::Named),
                arb_expr(depth - 1).prop_map(Argument::Unnamed)
            ].boxed()
        }

        impl Display for Argument {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Argument::Named((id, expr)) => write!(f, "{id}: {expr}"),
                    Argument::Unnamed(expr) => write!(f, "{expr}"),
                }
            }
        }

        #[derive(Clone, Debug, PartialEq, Eq)]
        struct Color {
            r: u8,
            g: u8,
            b: u8,
        }

        impl Display for Color {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
            }
        }

        fn arb_color() -> impl Strategy<Value = Color> {
            any::<(u8, u8, u8)>().prop_map(|(r, g, b)| Color { r, g, b })
        }

        #[derive(Clone, Debug, PartialEq, Eq)]
        struct Id {
            value: String,
        }

        impl Display for Id {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.value)
            }
        }

        fn arb_id() -> impl Strategy<Value = Id> {
            const LETTER: &str = "a-zA-Z";
            const DIGIT: &str = "0-9";
            const SPECIAL: &str = "\\+\\-\\*/\\$\\^§%&=`<>\\|_@~";

            const IDENTIFIER_REGEX: &str =
                formatcp!("[{LETTER}{SPECIAL}][{LETTER}{SPECIAL}{DIGIT}]*");

            IDENTIFIER_REGEX.prop_map(|s| Id { value: s })
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

        fn check_argument_parse(arg: Argument, pair: Pair<'_, Rule>) -> Result<(), TestCaseError> {
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
                let Argument::Named((_id, expr)) = arg else {
                    prop_assert!(false);
                    return Ok(());
                };

                check_expr_parse(expr, pairs.next().unwrap())?;
                prop_assert!(pairs.next().is_none());
                return Ok(());

            }

            prop_assert!(false);
            Ok(())
        }

        fn check_funccall_parse(func: FuncCall, pair: Pair<'_, Rule>) -> Result<(), TestCaseError> {
            let pairs_count = pair.clone().into_inner().count();
            if pairs_count == 0 {
                match func {
                    FuncCall::Empty => {},
                    FuncCall::Nested(func_calls) if func_calls.len() == 0 => {},
                    _ => {
                        prop_assert!(false);
                    },
                }

                return Ok(());
            }

            let mut pairs = pair.into_inner();
            let first_pair = pairs.next().unwrap();

            if first_pair.as_rule() == Rule::identifier {
                let FuncCall::Single((_id, args)) = func else { 
                    prop_assert!(false);
                    return Ok(());
                };

                for arg in args {
                    let p = pairs.next().unwrap();
                    check_argument_parse(arg, p)?;
                }

                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            if first_pair.as_rule() == Rule::func_call {
                let FuncCall::Nested(funcs) = func else {
                    prop_assert!(false);
                    return Ok(());
                };

                check_funccall_parse(funcs.first().unwrap().clone(), first_pair)?;

                for f in &funcs[1..] {
                    check_funccall_parse(f.clone(), pairs.next().unwrap())?;
                }

                prop_assert!(pairs.next().is_none());
                return Ok(());
            }

            prop_assert!(false);
            Ok(())
        }

        fn check_expr_parse(expr: Expr, pair: Pair<'_, Rule>) -> Result<(), TestCaseError> {
            match pair.as_rule() {
                Rule::EOI | Rule::WHITESPACE | Rule::COMMENT | Rule::line_comment | Rule::special_char => {
                    Ok(())
                },
                Rule::program => {
                    prop_assert!(false, "should never happen");
                    Ok(())
                },
                Rule::expr => {
                    let mut pairs = pair.into_inner();
                    let next = pairs.next().unwrap();
                    check_expr_parse(expr, next)?;
                    prop_assert!(pairs.next().is_none());
                    Ok(())
                },
                Rule::func_def => {
                    prop_assert!(false, "not checked");
                    Ok(())
                },
                Rule::map => {
                    let Expr::Map(map) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };

                    let mut pairs = pair.into_inner();

                    for (k,v) in map {
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
                },
                Rule::map_assign => {
                    prop_assert!(false, "should not be checked here");
                    Ok(())
                },
                Rule::array => {
                    let Expr::Array(array) = expr else {
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
                },
                Rule::parameters => {
                    prop_assert!(false, "not checked");
                    Ok(())
                },
                Rule::func_call => {
                    let Expr::FuncCall(func) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    check_funccall_parse(func, pair)
                },
                Rule::argument => {
                    prop_assert!(false, "should not be checked here");
                    Ok(())
                },
                Rule::identifier => {
                    let Expr::Id(_) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    
                    Ok(())
                },
                Rule::int => {
                    let Expr::Int(_) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    
                    Ok(())
                },
                Rule::float => {
                    let Expr::Float(_) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    
                    Ok(())
                },
                Rule::string => {
                    let Expr::String(_) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    
                    Ok(())
                },
                Rule::color => {
                    let Expr::Color(_) = expr else {
                        prop_assert!(false);
                        return Ok(());
                    };
                    
                    Ok(())
                },
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
