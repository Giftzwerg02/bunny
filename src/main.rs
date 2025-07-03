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
use parser::{try_highlight, BunnyParser, Rule};
use pest::{iterators::Pairs, Parser};
use runner::value::Value;
use types::hm::Type;
use std::{borrow::Cow, process::Stdio, sync::{Arc, Mutex}};

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
    fn render_prompt_left(&self) -> Cow<'_, str> {
        Cow::Owned("🐰".to_owned())
    }

    fn render_prompt_right(&self) -> Cow<'_, str> {
        Cow::Owned("".to_owned())
    }

    fn render_prompt_indicator(
        &self,
        _prompt_mode: reedline::PromptEditMode,
    ) -> std::borrow::Cow<'_, str> {
        Cow::Owned("> ".to_owned())
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        Cow::Owned("| ".to_owned())
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reedline::PromptHistorySearch,
    ) -> Cow<'_, str> {
        // todo: render history search indicator
        Cow::Owned("".to_string())
    }
}

struct BunnyReplHinter {
    symbols: Arc<Mutex<Vec<String>>>,
    hint_candidate: String,
}

impl Hinter for BunnyReplHinter {
    fn handle(
        &mut self,
        line: &str,
        pos: usize,
        _history: &dyn reedline::History,
        use_ansi_coloring: bool,
        _cwd: &str,
    ) -> String {
        self.hint_candidate = "".to_string();

        if line.is_empty() || pos == 0 {
            return self.hint_candidate.clone();
        }

        let start_of_word = line[..pos]
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
            .map_or(0, |i| i + 1);
        let current_word = &line[start_of_word..pos];

        if current_word.is_empty() {
            return self.hint_candidate.clone();
        }

        let symbols = self.symbols.lock().unwrap();
        for symbol in symbols.iter() {
            if symbol.starts_with(current_word) && symbol.len() > current_word.len() {
                self.hint_candidate = symbol[current_word.len()..].to_string();
                break;
            }
        }

        if use_ansi_coloring {
            HINTER_STYLE
                .paint(self.hint_candidate.clone())
                .to_string()
        }
        else {
            self.hint_candidate.clone()
        }
        
    }

    fn complete_hint(&self) -> String {
        self.hint_candidate.clone()
    }

    fn next_hint_token(&self) -> String {
        String::new() // Not implementing cycling for now
    }
}

lazy_static! {
    static ref DEFAULT_STYLE: Style = Style::new().fg(Color::White);
    static ref UNKNOWN_STYLE: Style = Style::new().fg(Color::Red);
    static ref REPL_COMMAND_STYLE: Style = DEFAULT_STYLE.bold();
    static ref REPL_RESULT_TYPE_STYLE: Style = Style::new().bold();
    static ref ASSIGMENT_STYLE: Style = Style::new().fg(Color::Cyan).bold();
    static ref GET_STYLE: Style = Style::new().fg(Color::LightBlue).bold();
    static ref STRING_STYLE: Style = Style::new().fg(Color::Green);
    static ref NUMBER_STYLE: Style = Style::new().fg(Color::Magenta);
    static ref PAREN_STYLE: Style = Style::new().fg(Color::Fixed(7)).dimmed();
    static ref COMMENT_STYLE: Style = *PAREN_STYLE;
    static ref HINTER_STYLE: Style = *PAREN_STYLE;
    static ref IDENTIFIER_STYLE: Style = Style::new().fg(Color::Blue).bold();
    static ref DEF_STYLE: Style = Style::new().fg(Color::Cyan).bold();
}

struct BunnyReplHighlighter;

impl BunnyReplHighlighter {
    fn apply_styles_from_pairs(styled: &mut StyledText, pairs: Pairs<'_, Rule>) {
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
            Self::apply_styles_from_pairs(styled, children);
        }
    }
}

impl Highlighter for BunnyReplHighlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> reedline::StyledText {
        let mut text = StyledText::new();
        text.push((*DEFAULT_STYLE, line.to_string()));

        let Ok(pairs) = BunnyParser::parse(parser::Rule::program, line) else {
            return text;
        };

        Self::apply_styles_from_pairs(&mut text, pairs);
        
        text
    }
}

fn print_result(result: &Value, typ: &Type) {
    let mut text = StyledText::new();

    text.push((*DEFAULT_STYLE, result.to_string()));

    let parsed = try_highlight(result.to_string());

    if let Ok(pairs) = parsed {
       BunnyReplHighlighter::apply_styles_from_pairs(&mut text, pairs.into_inner());
    }

    text.push((*REPL_RESULT_TYPE_STYLE, format!(" : {typ}")));
    
    println!(":: {}", text.render_simple())
}

use std::process::Command;
use std::env;

fn open_file(path: &str) {
    match env::consts::OS {
        "windows" => {
            let _ = Command::new("start")
                .arg(path)
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .stdin(Stdio::null())
                .spawn();
        },
        "macos" | "ios" => {
            let _ = Command::new("open")
                .arg(path)
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .stdin(Stdio::null())
                .spawn();
        },
        "linux" => {
            let _ = Command::new("xdg-open")
                .arg(path)
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .stdin(Stdio::null())
                .spawn();
        },
        _ => unreachable!(), // No ones gonna run this on BSD on something... no really pls dont
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
                println!("{result}");
            }

            Ok(())
        }
        None => {
            let symbols_for_hinter = Arc::new(Mutex::new(interpreter.get_defined_variables()));

            let validator = Box::new(BunnyReplValidator);
            let highlighter = Box::new(BunnyReplHighlighter);
            let hinter = Box::new(BunnyReplHinter {
                symbols: symbols_for_hinter.clone(),
                hint_candidate: "".to_string()
            });

            let mut line_editor = Reedline::create()
                .with_edit_mode(Box::new(Vi::new(
                    default_vi_insert_keybindings(),
                    default_vi_normal_keybindings(),
                )))
                .with_validator(validator)
                .with_highlighter(highlighter)
                .with_hinter(hinter);
            
            let prompt = BunnyReplPrompt;

            let mut opened = false;

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
                                let mut symbols = symbols_for_hinter.lock().unwrap();
                                *symbols = interpreter.get_defined_variables();

                                if let Some(output_file) = &cli.render_config.output && result.is_renderable() {
                                    output_svg(&result, &cli.render_config).unwrap_or_else(|err| {
                                        println!("Error rendering SVG: {err}");
                                    });

                                    
                                    if !opened {
                                        opened = true;
                                        open_file(output_file.to_str().unwrap_or_default());
                                    }

                                    println!(":: {}", REPL_COMMAND_STYLE.paint(format!("Written to {}", output_file.display())));
                                }
                                else {
                                    print_result(&result, &typ);
                                }
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
                        println!("Event: {x:?}")
                    }
                }
            }

            Ok(())
        }
    }
}
