use std::sync::Arc;

use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::{eval, library};
use crate::library::runnable_expression::InterpreterSymbolTable;
use crate::runner::value::Lazy;
use crate::types::InferenceState;
use crate::types::util::*;

pub mod macros;
pub mod runnable_expression;

pub struct Library<'a> {
    pub scoped: SymbolTable<ScopedStageInfo<'a>>,
    pub typed: InferenceState<'a>,
    pub runnable: InterpreterSymbolTable
}

pub fn standard_library<'a>() -> Library<'a> {
    library! {
        #[| a:bint() => b:bint() => ret:bint()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) + eval!(b))
        }

        #[| a:bint() => b:bint() => ret:bint()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) - eval!(b))
        }

        #[forall a | arr:barray(&a) => ret:a ]
        fn "first"(Lazy::Array(v)) {
            let arr = (**v).clone();
            (*arr[0]).clone()
        }
 
        #[forall a, b | fun:bfunc1(&a, &b) => arr:barray(&a) => ret:b ]
        fn "map"(Lazy::Lambda(f), Lazy::Array(v)) {
            let f = (**f).clone();
            let mut f = f.func.lock().unwrap();
            let mut res = vec![];
            for elem in (**v).clone() {
                let elem = (*elem).clone();
                let mapped = f(vec![elem.clone()].into());
                println!("i just mapped {:?} to {:?}", elem.eval(), mapped.clone().eval());
                res.push(Arc::new(mapped));
            }
            Lazy::new_array(res.into())
        }

        #[forall a, b | fun:bfunc(&[b.clone(), a.clone()], &b) => ground:b.clone() => arr:barray(&a) => ret:b]
        fn "foldl" (Lazy::Lambda(f), fst, Lazy::Array(list)) {
            let f = (**f).clone();
            let mut f = f.func.lock().unwrap();
            let mut acc = fst.clone();
            for elem in (**list).clone() {
                let elem = (*elem).clone();
                acc = f(vec![acc, elem.clone()].into());
            }
            acc
        }

        #[forall a | val:a.clone() => ret:a]
        fn "return" (v) {
            v.clone()
        }

        #[forall a | cond:bint() => iftrue:a.clone() => iffalse:a.clone() => ret:a]
        fn "if"(Lazy::Int(cond), iftrue, iffalse){
            if eval!(cond) != 0 {
                iftrue.clone().eval().into()
            }
            else {
                iffalse.clone().eval().into()
            }
        }

        #[forall a | elem:a.clone() => ret:a]
        fn "print"(elem){
            println!("Evaluated: {:?}", elem.clone().eval());
            elem.clone()
        }


    }
}

