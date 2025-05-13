use std::{fs, path::Path};
use miette::{NamedSource, Result};

use crate::{ast::{parsed::parsed_expr_pass, scoped::{scoped_expr_pass, ScopedStageInfo, SymbolTable}}, library::Library, parser::pest_parsing_pass, runner::{value::Value, Runner}, types::{typecheck_pass, InferenceState}};

pub struct Interpreter {
    scope_symbol_table: SymbolTable<ScopedStageInfo>,
    typechecker_state: InferenceState,
    runner: Runner
}

impl Interpreter {
    pub fn new(library: Library) -> Self {
        // Library is supposed to be something immutable...
        // Where as the Interpreter state is mutable. That's why we're copying it here
        Self {
            scope_symbol_table: library.scoped,
            typechecker_state: library.typed,
            runner: Runner::new(library.runnable)
        }
    }

    pub fn run(&mut self, bunny_src: String, source_name: String) -> Result<Value> {
        let source = NamedSource::new(source_name, bunny_src.clone())
            .with_language("Lisp");

        fn compute_result(bunny_src: String, interpreter: &mut Interpreter) -> Result<Value> {
            let peg = pest_parsing_pass(bunny_src)?;

            let ast = parsed_expr_pass(peg);

            let scoped_ast = scoped_expr_pass(ast, &interpreter.scope_symbol_table)?;
            interpreter.scope_symbol_table = scoped_ast.info().syms.clone();

            let typed_ast = typecheck_pass(&scoped_ast, &mut interpreter.typechecker_state)?
                .map_stage(&mut |info| info.into());

            let unevalutated_result = interpreter.runner.run(typed_ast);

            Ok(unevalutated_result.eval())
        }

        let result: Result<Value> = compute_result(bunny_src, self);
        
        result.map_err(|report| {
            report.with_source_code(source)
        })
    }
    
    pub fn run_file<P>(&mut self, path: P) -> Result<Value> where P: AsRef<Path>  {
        let bunny_source = fs::read_to_string(&path)
            .map_err(|_| miette::miette!("Could not read file: {}", path.as_ref().display()))?;
        
        let source_name = path.as_ref().display().to_string();

        self.run(bunny_source, source_name)
    }

    pub fn add_predefined_variable(&mut self, key: String, value: String) -> Result<Value> {
        self.run(format!("(def {} {})", key, value), "script-arguments".to_owned())
    }
}
