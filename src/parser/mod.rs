#[allow(unused)]
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct BunnyParser;

#[cfg(test)]
mod tests {
    use pest::{error::Error, iterators::Pairs};

    use super::*;

    fn parse_valid<'a, S: Into<&'a str> + std::fmt::Display>(rule: Rule, input: S) -> Pairs<'a, Rule> {
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
            "Illegally parsed \"{input}\" on rule: {rule:#?}: {:#?}",
            res.unwrap()
        );
        res.expect_err("we just checked this...")
    }

    mod constants {
        use super::*;

        mod int {
            use super::*;

            #[test]
            fn valid() {
                let p = parse_valid(Rule::int, "0");
                assert_eq!(p.as_str(), "0");

                let p = parse_valid(Rule::int, "1245");
                assert_eq!(p.as_str(), "1245");

                let p = parse_valid(Rule::int, "123 456 789"); // only parses the first number
                assert_eq!(p.as_str(), "123");
            }

            #[test]
            fn invalid() {
                parse_invalid(Rule::int, "a123");
                parse_invalid(Rule::int, "-123");
                parse_invalid(Rule::int, "");
                parse_invalid(Rule::int, "   ");
            }
        }

        mod float {
            use super::*;

            #[test]
            fn valid() {
                let p = parse_valid(Rule::float, "0f");
                assert_eq!(p.as_str(), "0f");

                let p = parse_valid(Rule::float, "0.f");
                assert_eq!(p.as_str(), "0.f");

                let p = parse_valid(Rule::float, "0.456f");
                assert_eq!(p.as_str(), "0.456f");

                let p = parse_valid(Rule::float, "53690.456f 1.123f 705.45f");
                assert_eq!(p.as_str(), "53690.456f");
            }

            #[test]
            fn invalid() {
                parse_invalid(Rule::float, "a123f");
                parse_invalid(Rule::float, "a123.456f");
                parse_invalid(Rule::float, "-123f");
                parse_invalid(Rule::float, "-123.456f");
                parse_invalid(Rule::float, "");
                parse_invalid(Rule::float, "   ");
                parse_invalid(Rule::float, "0");
                parse_invalid(Rule::float, "1");
                parse_invalid(Rule::float, ".1f");
                parse_invalid(Rule::float, "f");
                parse_invalid(Rule::float, ".f");
            }
        }

        mod string {
            use super::*;

            #[test]
            fn valid() {
                fn string(s: &str) -> String {
                    format!("\"{s}\"")
                }

                let binding = string("");
                let p = parse_valid(Rule::string, &*binding);
                assert_eq!(p.as_str(), &string(""));

                let binding = string("foobar");
                let p = parse_valid(Rule::string, &*binding);
                assert_eq!(p.as_str(), binding);

                let binding = string("   ");
                let p = parse_valid(Rule::string, &*binding);
                assert_eq!(p.as_str(), binding);

                let binding = string("\t\n\t\n\n\t");
                let p = parse_valid(Rule::string, &*binding);
                assert_eq!(p.as_str(), binding);

                let binding = string("\x00");
                let p = parse_valid(Rule::string, &*binding);
                assert_eq!(p.as_str(), binding);

                let binding = string("\u{03B2}");
                let p = parse_valid(Rule::string, &*binding);
                assert_eq!(p.as_str(), binding);
            }

            #[test]
            fn invalid() {
                parse_invalid(Rule::string, "abc");
                parse_invalid(Rule::string, "");
                parse_invalid(Rule::string, "   ");
                parse_invalid(Rule::string, "\"");
                parse_invalid(Rule::string, "''");
            }
        }
    }

    mod identifier {
        use super::*;

        #[test]
        fn valid() {}

        #[test]
        fn invalid() {}
    }
}
