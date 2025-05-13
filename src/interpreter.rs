use std::{fs, path::Path};
use miette::{NamedSource, Result};

use crate::{ast::{parsed::parsed_expr_pass, scoped::{scoped_expr_pass, ScopedStageInfo, SymbolTable}}, library::Library, parser::pest_parsing_pass, runner::{value::Value, Runner}, types::{typecheck_pass, InferenceState}};

pub struct Interpreter<'a> {
    scope_symbol_table: SymbolTable<ScopedStageInfo<'a>>,
    typechecker_state: InferenceState<'a>,
    runner: Runner,
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

    pub fn run(&mut self, bunny_src: String, source_name: String) -> Result<Value> {
        self.source_buffer = bunny_src.clone();

        let source = NamedSource::new(source_name, bunny_src)
            .with_language("scheme");

        let peg = pest_parsing_pass(&self.source_buffer)
            .map_err(|report|{ report.with_source_code(source.clone()) })?;

        let ast = parsed_expr_pass(peg);

        let scoped_ast = scoped_expr_pass(ast, &self.scope_symbol_table)?;

        let typed_ast = typecheck_pass(&scoped_ast, &mut self.typechecker_state)
            .map_err(|report|{ report.with_source_code(source) })?;

        // TODO Please PLEASE fucking remove this
        // We should not need to generalize here..
        let typ = typed_ast.map_stage(
            &mut |typed_info| typed_info.generalize(&self.typechecker_state.hm)
        );

        // TODO Make run take a reference and remove clone
        let unevalutated_result = self.runner.run(typ);

        Ok(unevalutated_result.eval())
    }
    
    pub fn run_file<P>(&mut self, path: String) -> Result<Value> where P: AsRef<Path>  {
        let maybe_input = fs::read_to_string(&path);

        let Ok(bunny_source) = maybe_input else {
            todo!();
        };

        self.run(bunny_source, path)
    }
}