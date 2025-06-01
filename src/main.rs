pub mod ast;
pub mod cli;
pub mod debug;
mod interpreter;
mod library;
pub mod parser;
mod runner;
mod svg;
mod types;

use nu_ansi_term::{Color, Style};
use lazy_static::lazy_static;
use parser::{BunnyParser, Rule};
use pest::{iterators::Pairs, Parser};
use std::borrow::Cow;

#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;

use crate::library::standard_library;
use interpreter::Interpreter;
use miette::Result;
use reedline::{
    default_vi_insert_keybindings, default_vi_normal_keybindings, Highlighter, Hinter, Prompt, Reedline, Signal, StyledText, ValidationResult, Validator, Vi
};
use svg::output_svg;

struct BunnyReplValidator;

impl Validator for BunnyReplValidator {
    fn validate(&self, line: &str) -> reedline::ValidationResult {
        if line.trim().is_empty() || Interpreter::is_valid_expression(line.to_owned()) {
            ValidationResult::Complete
        } else {
            ValidationResult::Incomplete
        }
    }
}

struct BunnyReplPrompt;

impl Prompt for BunnyReplPrompt {
    fn render_prompt_left(&self) -> std::borrow::Cow<str> {
        Cow::Owned("🐰".to_owned())
    }

    fn render_prompt_right(&self) -> std::borrow::Cow<str> {
        Cow::Owned("".to_owned())
    }

    fn render_prompt_indicator(
        &self,
        prompt_mode: reedline::PromptEditMode,
    ) -> std::borrow::Cow<str> {
        Cow::Owned("> ".to_owned())
    }

    fn render_prompt_multiline_indicator(&self) -> std::borrow::Cow<str> {
        Cow::Owned("| ".to_owned())
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: reedline::PromptHistorySearch,
    ) -> std::borrow::Cow<str> {
        todo!()
    }
}

struct BunnyReplHinter;

impl Hinter for BunnyReplHinter {
    fn handle(
        &mut self,
        line: &str,
        pos: usize,
        history: &dyn reedline::History,
        use_ansi_coloring: bool,
        cwd: &str,
    ) -> String {
        todo!()
    }

    fn complete_hint(&self) -> String {
        todo!()
    }

    fn next_hint_token(&self) -> String {
        todo!()
    }
}

lazy_static! {
    static ref DEFAULT_STYLE: Style = Style::new().fg(Color::White);
    static ref UNKNOWN_STYLE: Style = Style::new().fg(Color::Red);
    static ref REPL_COMMAND_STYLE: Style = DEFAULT_STYLE.bold();
    static ref ASSIGMENT_STYLE: Style = Style::new().fg(Color::Cyan).bold();
    static ref GET_STYLE: Style = Style::new().fg(Color::LightBlue).bold();
    static ref STRING_STYLE: Style = Style::new().fg(Color::Green);
    static ref NUMBER_STYLE: Style = Style::new().fg(Color::Magenta);
    static ref PAREN_STYLE: Style = Style::new().fg(Color::Fixed(7)).dimmed();
    static ref COMMENT_STYLE: Style = Style::new().fg(Color::Fixed(7)).dimmed();
    static ref IDENTIFIER_STYLE: Style = Style::new().fg(Color::Blue).bold();
    static ref DEF_STYLE: Style = Style::new().fg(Color::Red).bold();
}

struct BunnyReplHighlighter;

impl BunnyReplHighlighter {
    fn apply_styles_from_pairs(&self, styled: &mut StyledText, pairs: Pairs<'_, Rule>) {
        for pair in pairs {
            let range = pair.as_span();
            let style = match pair.as_rule() {
                Rule::def_id | Rule::lambda_id => *DEF_STYLE,
                Rule::identifier => *IDENTIFIER_STYLE,
                Rule::int | Rule::float => *NUMBER_STYLE,
                Rule::string => *STRING_STYLE,
                Rule::color => *STRING_STYLE,
                Rule::COMMENT | Rule::line_comment => *COMMENT_STYLE,
                _ => *DEFAULT_STYLE,
            };
            styled.style_range(range.start(), range.end(), style);
            let children = pair.into_inner();
            self.apply_styles_from_pairs(styled, children);
        }
    }
}

impl Highlighter for BunnyReplHighlighter {
    fn highlight(&self, line: &str, cursor: usize) -> reedline::StyledText {
        let mut text = StyledText::new();
        text.push((*DEFAULT_STYLE, line.to_string()));

        let Ok(pairs) = BunnyParser::parse(parser::Rule::program, line) else {
            return text;
        };

        self.apply_styles_from_pairs(&mut text, pairs);
        
        text
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut interpreter = Interpreter::new(standard_library());

    for (name, value) in cli.defined_variables()? {
        interpreter.add_predefined_variable(name, value)?;
    }

    match cli.file {
        Some(file) => {
            let (result, _) = interpreter.run_file(file)?;

            if result.is_renderable() {
                output_svg(&result, &cli.render_config)?;
            } else {
                println!("{}", result);
            }

            Ok(())
        }
        None => {
            let validator = Box::new(BunnyReplValidator);
            let highlighter = Box::new(BunnyReplHighlighter);
            let mut line_editor = Reedline::create()
                .with_edit_mode(Box::new(Vi::new(
                    default_vi_insert_keybindings(),
                    default_vi_normal_keybindings(),
                )))
                .with_validator(validator)
                .with_highlighter(highlighter);
            let prompt = BunnyReplPrompt;

            loop {
                let sig = line_editor.read_line(&prompt);
                match sig {
                    Ok(Signal::Success(buffer)) => {
                        if buffer.trim().is_empty() {
                            continue;
                        }

                        let wrapped = format!("({buffer})");
                        let res = interpreter.run(wrapped, "repl".to_owned());
                        match res {
                            Ok((result, typ)) => {
                                println!(":: {result} : {typ}");
                            }
                            Err(err) => {
                                println!("{err:?}");
                            }
                        };
                    }
                    Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                        println!("\nAborted!");
                        break;
                    }
                    x => {
                        println!("Event: {:?}", x)
                    }
                }
            }

            Ok(())
        }
    }
}
