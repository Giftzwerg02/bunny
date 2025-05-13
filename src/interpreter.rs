use std::{fs, path::Path};
use miette::{NamedSource, Result};

use crate::{ast::{parsed::parsed_expr_pass, scoped::{scoped_expr_pass, ScopedStageInfo, SymbolTable}}, library::Library, parser::pest_parsing_pass, runner::{value::Value, Runner}, types::{typecheck_pass, InferenceState}};

pub struct Interpreter<'a> {
    scope_symbol_table: SymbolTable<ScopedStageInfo<'a>>,
    typechecker_state: InferenceState<'a>,
    runner: Runner<'a>,
    source_buffer: String
}

impl<'a> Interpreter<'a> {
    pub fn new(library: Library<'a>) -> Self {
        // Library is supposed to be something immutable...
        // Where as the Interpreter state is mutable. That's why we're copying it here
        Self {
            scope_symbol_table: library.scoped,
            typechecker_state: library.typed,
            runner: Runner::new(library.runnable),
            source_buffer: String::new()
        }
    }

    pub fn run(&'a mut self, bunny_src: String, source_name: String) -> Result<Value<'a>> {
        self.source_buffer = bunny_src.clone();

        let source = NamedSource::new(source_name, bunny_src)
            .with_language("scheme");

        let result: Result<Value> = {
            let peg = pest_parsing_pass(&self.source_buffer)?;

            let ast = parsed_expr_pass(peg);

            let scoped_ast = scoped_expr_pass(ast, &self.scope_symbol_table)?;

            let typed_ast = typecheck_pass(&scoped_ast, &mut self.typechecker_state)?
                .map_stage(&mut |info| info.into());

            let unevalutated_result = self.runner.run(typed_ast);

            Ok(unevalutated_result.eval())
        };

        result.map_err(|report| {
            report.with_source_code(source)
        })
    }
    
    pub fn run_file<P>(&'a mut self, path: P) -> Result<Value<'a>> where P: AsRef<Path>  {
        let maybe_input = fs::read_to_string(&path);

        let Ok(bunny_source) = maybe_input else {
            todo!();
        };
        
        let source_name = path.as_ref().display().to_string();

        self.run(bunny_source, source_name)
    }
}