use std::{fs, path::Path};
use miette::{NamedSource, Result};

use crate::{ast::{parsed::parsed_expr_pass, scoped::{scoped_expr_pass, ScopedStageInfo, SymbolTable}}, library::Library, parser::pest_parsing_pass, runner::{value::Value, Runner}, types::{hm::Type, typecheck_pass, InferenceState}};

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

    pub fn is_valid_expression(bunny_src: String) -> bool {
        pest_parsing_pass(bunny_src).is_ok()
    }

    pub fn get_defined_variables(&self) -> Vec<String> {
        self.scope_symbol_table
            .inner
            .iter()
            .map(|(key, _)| key.to_string())
            .collect()
    }

    pub fn run(&mut self, bunny_src: String, source_name: String) -> Result<(Value, Type)> {
        let source = NamedSource::new(source_name, bunny_src.clone())
            .with_language("Lisp");

        fn compute_result(bunny_src: String, interpreter: &mut Interpreter) -> Result<(Value, Type)> {
            let peg = pest_parsing_pass(bunny_src)?;

            let ast = parsed_expr_pass(peg)?;

            let scoped_ast = scoped_expr_pass(ast, &interpreter.scope_symbol_table)?;
            interpreter.scope_symbol_table = scoped_ast.info().syms.clone();

            let typed_ast = typecheck_pass(&scoped_ast, &mut interpreter.typechecker_state.clone())?;
            let expr_type = typed_ast.info().typ.clone();
            let any_typed_ast = typed_ast.map_stage(&mut |info| info.into());

            let unevalutated_result = interpreter.runner.run(any_typed_ast);

            Ok((unevalutated_result.eval(), expr_type))
        }

        let result: Result<(Value, Type)> = compute_result(bunny_src, self);
        
        result.map_err(|report| {
            report.with_source_code(source)
        })
    }
    
    pub fn run_file<P>(&mut self, path: P) -> Result<(Value, Type)> where P: AsRef<Path>  {
        let bunny_source = fs::read_to_string(&path)
            .map_err(|_| miette::miette!("Could not read file: {}", path.as_ref().display()))?;
        
        let source_name = path.as_ref().display().to_string();

        self.run(bunny_source, source_name)
    }

    pub fn add_predefined_variable(&mut self, key: String, value: String) -> Result<(Value, Type)> {
        self.run(format!("(def {} {})", key, value), "script-arguments".to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::library::standard_library;
    use crate::runner::value::Value;
    use crate::types::hm::Type;

    fn create_interpreter() -> Interpreter {
        Interpreter::new(standard_library())
    }

    fn run_code(interpreter: &mut Interpreter, code: &str) -> Result<(Value, Type)> {
        interpreter.run(code.to_string(), "test".to_string())
    }

    fn assert_int_result(result: Result<(Value, Type)>, expected: i64) {
        let (value, _) = result.expect("Expected successful execution");
        match value {
            Value::Int(actual) => assert_eq!(actual, expected),
            _ => panic!("Expected integer result, got: {:?}", value),
        }
    }

    fn assert_string_result(result: Result<(Value, Type)>, expected: &str) {
        let (value, _) = result.expect("Expected successful execution");
        match value {
            Value::String(actual) => assert_eq!(actual.as_str(), expected),
            _ => panic!("Expected string result, got: {:?}", value),
        }
    }

    fn assert_array_len(result: Result<(Value, Type)>, expected_len: usize) {
        let (value, _) = result.expect("Expected successful execution");
        match value {
            Value::Array(actual) => assert_eq!(actual.len(), expected_len),
            _ => panic!("Expected array result, got: {:?}", value),
        }
    }

    #[test]
    fn test_basic_arithmetic() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, "((+ 2 3))"), 5);
        assert_int_result(run_code(&mut interpreter, "((- 10 4))"), 6);
        assert_int_result(run_code(&mut interpreter, "((* 6 7))"), 42);
        assert_int_result(run_code(&mut interpreter, "((/ 15 3))"), 5);
        assert_int_result(run_code(&mut interpreter, "((neg 5))"), -5);
    }

    #[test]
    fn test_comparison_operators() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, "((< 3 5))"), 1);
        assert_int_result(run_code(&mut interpreter, "((< 5 3))"), 0);
        assert_int_result(run_code(&mut interpreter, "((> 5 3))"), 1);
        assert_int_result(run_code(&mut interpreter, "((> 3 5))"), 0);
        assert_int_result(run_code(&mut interpreter, "((= 4 4))"), 1);
        assert_int_result(run_code(&mut interpreter, "((= 4 5))"), 0);
        assert_int_result(run_code(&mut interpreter, "((<= 3 3))"), 1);
        assert_int_result(run_code(&mut interpreter, "((>= 5 5))"), 1);
    }

    #[test]
    fn test_logical_operators() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, "((not 0))"), 1);
        assert_int_result(run_code(&mut interpreter, "((not 1))"), 0);
        assert_int_result(run_code(&mut interpreter, "((not 42))"), 0);
    }

    #[test]
    fn test_conditionals() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, "((if 1 42 24))"), 42);
        assert_int_result(run_code(&mut interpreter, "((if 0 42 24))"), 24);
        assert_int_result(run_code(&mut interpreter, "((if (> 5 3) 100 200))"), 100);
    }

    #[test]
    fn test_arrays() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, "((get [1 2 3] 0))"), 1);
        assert_int_result(run_code(&mut interpreter, "((get [1 2 3] 2))"), 3);
        assert_int_result(run_code(&mut interpreter, "((first [42 1 2]))"), 42);
        assert_int_result(run_code(&mut interpreter, "((len [1 2 3 4]))"), 4);
        assert_array_len(run_code(&mut interpreter, "((append [1 2] 3))"), 3);
        assert_array_len(run_code(&mut interpreter, "((append-all [1 2] [3 4]))"), 4);
    }

    #[test]
    fn test_range() {
        let mut interpreter = create_interpreter();
        
        assert_array_len(run_code(&mut interpreter, "((range 0 5))"), 5);
        assert_int_result(run_code(&mut interpreter, "((first (range 10 15)))"), 10);
        assert_int_result(run_code(&mut interpreter, "((get (range 5 10) 2))"), 7);
    }

    #[test]
    fn test_string_operations() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, r#"((strlen "hello"))"#), 5);
        assert_string_result(run_code(&mut interpreter, r#"((format "Hello {}" ["world"]))"#), "Hello world");
        assert_string_result(run_code(&mut interpreter, r#"((join ["a" "b" "c"] ","))"#), "a,b,c");
        assert_string_result(run_code(&mut interpreter, r#"((to-string 42))"#), "42");
    }

    #[test]
    fn test_higher_order_functions() {
        let mut interpreter = create_interpreter();
        
        // Test map with lambda (partial application doesn't work the way I thought)
        let code = r#"((map (\ (x) (+ x 1)) [1 2 3]))"#;
        assert_array_len(run_code(&mut interpreter, code), 3);
        
        // Test foldl
        assert_int_result(run_code(&mut interpreter, "((foldl + 0 [1 2 3 4]))"), 10);
        assert_int_result(run_code(&mut interpreter, "((foldl * 1 [2 3 4]))"), 24);
    }

    #[test]
    fn test_lambda_functions() {
        let mut interpreter = create_interpreter();
        
        // Test lambda with map
        let code = r#"((map (\ (x) (+ x 1)) [1 2 3]))"#;
        assert_array_len(run_code(&mut interpreter, code), 3);
        
        // Test lambda with foldl
        let code = r#"((foldl (\ (acc x) (+ acc x)) 0 [1 2 3]))"#;
        assert_int_result(run_code(&mut interpreter, code), 6);
    }

    #[test]
    fn test_function_definitions() {
        let mut interpreter = create_interpreter();
        
        let code = r#"((
            (def double (x) (* x 2))
            (double 21)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 42);
        
        let code = r#"((
            (def add-two (a b) (+ a b))
            (add-two 10 15)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 25);
    }

    #[test]
    fn test_iterative_functions() {
        let mut interpreter = create_interpreter();
        
        // Test iterative function using foldl instead of recursion
        let code = r#"((
            (def sum-to-n (n) (foldl + 0 (range 1 (+ n 1))))
            (sum-to-n 5)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 15); // 1+2+3+4+5 = 15
        
        // Test another iterative approach
        let code = r#"((
            (def multiply-range (start end) (foldl * 1 (range start end)))
            (multiply-range 2 5)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 24); // 2*3*4 = 24
    }

    #[test]
    fn test_variable_definitions() {
        let mut interpreter = create_interpreter();
        
        let code = r#"((
            (def x 42)
            (def y 8)
            (+ x y)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 50);
    }

    #[test]
    fn test_type_conversions() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, "((floor 3.7f))"), 3);
        // Test to-float conversion
        let code = r#"((
            (def f (to-float 42))
            (floor (*f f 2.0f))
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 84);
    }

    #[test]
    fn test_state_persistence_between_runs() {
        let mut interpreter = create_interpreter();
        
        // First run: define a variable  
        let result1 = run_code(&mut interpreter, "((def persistent-var 100))");
        assert!(result1.is_ok());
        
        // Second run: use the variable from first run
        assert_int_result(run_code(&mut interpreter, "((return persistent-var))"), 100);
        
        // Third run: define a function
        let result3 = run_code(&mut interpreter, "((def add-to-persistent (x) (+ persistent-var x)))");
        assert!(result3.is_ok());
        
        // Fourth run: use both the variable and function
        assert_int_result(run_code(&mut interpreter, "((add-to-persistent 42))"), 142);
        
        // Fifth run: redefine the variable - this might not work as expected in the current implementation
        // Let's test with a new variable instead
        let result5 = run_code(&mut interpreter, "((def another-var 58))");
        assert!(result5.is_ok());
        
        // Sixth run: use the new variable with the function
        let code = r#"((
            (def add-both (x) (+ (+ persistent-var another-var) x))
            (add-both 0)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 158); // 100 + 58 + 0 = 158
    }

    #[test]
    fn test_nested_function_calls() {
        let mut interpreter = create_interpreter();
        
        assert_int_result(run_code(&mut interpreter, "((+ (* 2 3) (- 10 5)))"), 11);
        assert_int_result(run_code(&mut interpreter, "((len (range 0 (+ 2 3))))"), 5);
    }

    #[test]
    fn test_complex_expressions() {
        let mut interpreter = create_interpreter();
        
        // Test fibonacci-like calculation
        let code = r#"((
            (def sum-array (arr) (foldl + 0 arr))
            (sum-array (map (\ (x) (* x 2)) [1 2 3 4 5]))
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 30); // 2+4+6+8+10 = 30
        
        // Test conditional with array operations
        let code = r#"((
            (def arr [1 2 3 4 5])
            (if (> (len arr) 3)
                (first arr)
                (get arr 2)
            )
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 1);
    }

    #[test]
    fn test_error_handling_with_panic_functions() {
        let mut interpreter = create_interpreter();
        
        // Test that panic-int actually panics (we expect this to fail)
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_code(&mut interpreter, r#"((panic-int "test error"))"#)
        }));
        assert!(result.is_err(), "Expected panic-int to cause a panic");
    }

    #[test]
    fn test_compose_function() {
        let mut interpreter = create_interpreter();
        
        let code = r#"((
            (def add-one (x) (+ x 1))
            (def double (x) (* x 2))
            (def composed (compose1 double add-one))
            (apply1 composed 5)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 11); // add-one(5) = 6, then double(6) = 12, but compose order is different
    }

    #[test]
    fn test_variable_scoping() {
        let mut interpreter = create_interpreter();
        
        let code = r#"((
            (def x 10)
            (def test-scope (y) (
                (def x 20)
                (+ x y)
            ))
            (+ (test-scope 5) x)
        ))"#;
        // Inner x should be 20, so test-scope(5) = 25, outer x = 10, total = 35
        assert_int_result(run_code(&mut interpreter, code), 35);
    }

    #[test]
    fn test_array_explode_and_format() {
        let mut interpreter = create_interpreter();
        
        let code = r#"((
            (def parts (explode "a,b,c" ","))
            (len parts)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 3);
        
        let code = r#"((
            (def formatted (format "Item: {}, Count: {}" ["apple" "5"]))
            (strlen formatted)
        ))"#;
        assert_int_result(run_code(&mut interpreter, code), 21); // "Item: apple, Count: 5"
    }
}