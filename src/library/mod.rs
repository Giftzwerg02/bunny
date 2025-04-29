use std::sync::{Arc, Mutex};

use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::library::runnable_expression::InterpreterSymbolTable;
use crate::runner::value::{Lazy, LazyFunc, LazyIter};
use crate::types::InferenceState;
use crate::types::util::*;
use crate::{eval, library};

pub mod macros;
pub mod runnable_expression;

pub struct Library<'a> {
    pub scoped: SymbolTable<ScopedStageInfo<'a>>,
    pub typed: InferenceState<'a>,
    pub runnable: InterpreterSymbolTable,
}

macro_rules! ltrue {
    () => {
       Lazy::new_int(1)
    };
}

macro_rules! lfalse {
    () => {
       Lazy::new_int(0)
    };
}

pub fn standard_library<'a>() -> Library<'a> {
    library! {
        #[| a:int() => b:int() => ret:int()]
        fn "+"(a, b): (runner) {
            let a = eval!(runner(a) as Lazy::Int);
            let b = eval!(runner(b) as Lazy::Int);
            Lazy::new_int(a + b)
        }

        #[| a:int() => b:int() => ret:int()]
        fn "-"(a, b): (runner) {
            let a = eval!(runner(a) as Lazy::Int);
            let b = eval!(runner(b) as Lazy::Int);
            Lazy::new_int(a - b)
        }

        #[forall a | arr:array(&a) => ret:a ]
        fn "first"(v): (runner) {
            let v = eval!(runner(v) as Lazy::Array);
            let mut iter = v.inner.lock().unwrap();
            iter.next().expect("called first on empty array")
        }

        #[forall a | arr:array(&a) => ret:int() ]
        fn "len"(v): (runner) {
            let v = eval!(runner(v) as Lazy::Array);
            let mut iter = v.inner.lock().unwrap();
            let len = iter.into_iter().collect::<Vec<_>>().len();
            Lazy::new_int(len as i64)
        }

        #[forall a | arr:array(&a) => idx:int() => ret:a ]
        fn "get"(v, idx): (runner) {
            let v = eval!(runner(v) as Lazy::Array);
            let idx = eval!(runner(idx) as Lazy::Int);
            let mut iter = v.inner.lock().unwrap();
            iter.nth(idx as usize).expect("get idx out of bounce")
        }

        #[forall a, b | fun:func1(&a, &b) => arr:array(&a) => ret:b ]
        fn "map"(f, v): (runner) {
            let f = eval!(runner(f) as Lazy::Lambda);
            let mut v = eval!(runner(v) as Lazy::Array);
            let mut iter = v.inner.clone().lock().unwrap(); 
            let iter = iter
                .into_iter()
                .map(move |elem| {
                    let f = f.clone();
                    let mut f = f.func.lock().unwrap();
                    f(vec![elem])
                });
            v.inner = Arc::new(Mutex::new(iter));
            Lazy::new_array(v)
        }

        #[forall a, b | fun:func2(&b, &a, &b) => ground:b => arr:array(&a) => ret:b]
        fn "foldl" (f, fst, list): (runner) {
            let f = eval!(runner(f) as Lazy::Lambda);
            let mut f = f.func.lock().unwrap();
            let init = eval!(runner(fst));
            let v = eval!(runner(list) as Lazy::Array);
            let mut iter = v.inner.lock().unwrap(); 
            let res = iter.into_iter()
                .fold(init, |acc, elem| {
                    f(vec![acc, elem]) 
                });
            res
        }

        #[forall a | arr:array(&a) => val:a => ret:array(&a)]
        fn "append" (arr, val): (runner) {
            let val = eval!(runner(val));
            let mut v = eval!(runner(arr) as Lazy::Array);
            let mut iter = v.inner.lock().unwrap();
            *iter = Box::new(iter.into_iter().chain(vec![val]));
            Lazy::new_array(v)
        }

        #[| from:int() => to:int() => res:array(&int())]
        fn "range" (from, to): (runner) {
            let from = eval!(runner(from) as Lazy::Int);
            let to = eval!(runner(to) as Lazy::Int);
            let range = (from..to)
                .map(|i| Lazy::new_int(i));
            let inner = Arc::new(Mutex::new(Box::new(range)));
            let lazy_iter = LazyIter::new(inner);
            Lazy::new_array(lazy_iter)
        }

        #[forall a | val:a.clone() => ret:a]
        fn "return" (v): (runner) {
            eval!(runner(v))
        }

        #[forall a | cond:int() => iftrue:a => iffalse:a => ret:a]
        fn "if"(cond, iftrue, iffalse): (runner) {
            if eval!(runner(cond) as Lazy::Int) != 0 {
                eval!(runner(iftrue))
            }
            else {
                eval!(runner(iffalse))
            }
        }

        #[forall a | elem:a => ret:a]
        fn "print"(elem): (runner) {
            let eval = eval!(runner(elem));
            println!("Evaluated: {:?}", eval.clone().full_eval());
            eval
        }

        #[forall a | message:string() => ret:a]
        fn "panic"(message): (runner) {
            panic!("panicked: {}", eval!(runner(message) as Lazy::String))
        }

        #[| a:int() => b:int() => res:int()]
        fn "<"(a, b): (runner) {
            let a = eval!(runner(a) as Lazy::Int);
            let b = eval!(runner(b) as Lazy::Int);
            if a < b {
                ltrue!()
            } else {
                lfalse!()
            }
        }


        #[| a:int() => b:int() => res:int()]
        fn ">"(a, b): (runner) {
            let a = eval!(runner(a) as Lazy::Int);
            let b = eval!(runner(b) as Lazy::Int);
            if a > b {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => b:int() => res:int()]
        fn "<="(a, b): (runner) {
            let a = eval!(runner(a) as Lazy::Int);
            let b = eval!(runner(b) as Lazy::Int);
            if a <= b {
                ltrue!()
            } else {
                lfalse!()
            }
        }


        #[| a:int() => b:int() => res:int()]
        fn ">="(a, b): (runner) {
            let a = eval!(runner(a) as Lazy::Int);
            let b = eval!(runner(b) as Lazy::Int);
            if a >= b {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => b:int() => res:int()]
        fn "="(a, b): (runner) {
            let a = eval!(runner(a) as Lazy::Int);
            let b = eval!(runner(b) as Lazy::Int);
            if a == b {
                ltrue!()
            } else {
                lfalse!()
            }
        }
    }
}
