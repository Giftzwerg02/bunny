use std::sync::Arc;

use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::library::runnable_expression::InterpreterSymbolTable;
use crate::runner::value::Lazy;
use crate::types::InferenceState;
use crate::types::util::*;
use crate::{eval, lazy, library};

pub mod macros;
pub mod runnable_expression;

pub struct Library<'a> {
    pub scoped: SymbolTable<ScopedStageInfo<'a>>,
    pub typed: InferenceState<'a>,
    pub runnable: InterpreterSymbolTable,
}

macro_rules! ltrue {
    () => {
        lazy!(Lazy::Int, 1)
    };
}

macro_rules! lfalse {
    () => {
        lazy!(Lazy::Int, 0)
    };
}

pub fn standard_library<'a>() -> Library<'a> {
    library! {
        #[| a:int() => b:int() => ret:int()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            let a = a.clone();
            let b = b.clone();
            lazy!(Lazy::Int, {
                let a = **a;
                let b = **b;
                a + b
            })
        }

        #[| a:int() => b:int() => ret:int()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            let a = a.clone();
            let b = b.clone();
            lazy!(Lazy::Int, {
                let a = **a;
                let b = **b;
                a - b
            })
        }

        #[forall a | arr:array(&a) => ret:a ]
        fn "first"(Lazy::Array(v)) {
            v[0].clone()
        }

        #[forall a | arr:array(&a) => ret:int() ]
        fn "len"(Lazy::Array(v)) {
            let v = v.clone();
            lazy!(Lazy::Int, {
                let len = v.len();
                len as i64
            })
        }

        #[forall a | arr:array(&a) => idx:int() => ret:a ]
        fn "get"(Lazy::Array(v), Lazy::Int(idx)) {
            let idx = eval!(idx);
            v[idx as usize].clone()
        }

        #[forall a, b | fun:func1(&a, &b) => arr:array(&a) => ret:b ]
        fn "map"(Lazy::Lambda(f), Lazy::Array(v)) {
            let f = f.clone();
            let v = v.clone();
            lazy!(Lazy::Array, {
                let mut f = f.func.lock().unwrap();
                let mut res = vec![];
                for elem in (**v).clone() {
                    let mapped = f(vec![elem.clone()].into());
                    res.push(mapped);
                }
                res.into()
            })
        }

        #[forall a, b | fun:func2(&b, &a, &b) => ground:b => arr:array(&a) => ret:b]
        fn "foldl" (Lazy::Lambda(f), fst, Lazy::Array(list)) {
            let mut f = f.func.lock().unwrap();
            let mut acc = fst.clone();
            for elem in eval!(list) {
                acc = f(vec![acc, elem].into());
            }
            acc
        }

        #[forall a | arr:array(&a) => val:a => ret:array(&a)]
        fn "append" (Lazy::Array(a), val) {
            let a = a.clone();
            let val = val.clone();
            lazy!(Lazy::Array, {
                let mut res = eval!(a);
                res.push_back(val);
                res
            })
        }

        #[| from:int() => to:int() => res:array(&int())]
        fn "range" (Lazy::Int(from), Lazy::Int(to)) {
            let from = from.clone();
            let to = to.clone();
            lazy!(Lazy::Array, {
                let from = **from;
                let to = **to;
                let range = (from..to)
                    .map(|i| lazy!(Lazy::Int, i))
                    .collect();
                range
            })
        }

        #[forall a | val:a.clone() => ret:a]
        fn "return" (v) {
            v.clone()
        }

        #[forall a | cond:int() => iftrue:a => iffalse:a => ret:a]
        fn "if"(Lazy::Int(cond), iftrue, iffalse){
            if eval!(cond) != 0 {
                iftrue.clone()
            }
            else {
                iffalse.clone()
            }
        }

        #[forall a | elem:a => ret:a]
        fn "print"(elem){
            let elem = elem.clone();
            match elem {
                Lazy::Int(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::Int, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        eval!(res)
                    })
                },
                Lazy::Float(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::Float, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        eval!(res)
                    })
                }
                Lazy::String(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::String, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        let res = eval!(res);
                        res
                    })
                },
                Lazy::Color(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::Color, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        eval!(res)
                    })
                },
                Lazy::Opaque(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::Opaque, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        eval!(res)
                    })
                },
                Lazy::Array(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::Array, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        eval!(res)
                    })
                },
                Lazy::Dict(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::Dict, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        eval!(res)
                    })
                },
                Lazy::Lambda(ref lazy_cell) => {
                    let res = lazy_cell.clone();
                    lazy!(Lazy::Lambda, {
                        println!("Evaluated: {:?}", elem.clone().eval());
                        eval!(res)
                    })
                },
            }
        }

        #[forall a | message:string() => ret:a]
        fn "panic"(Lazy::String(message)) {
            let message = message.clone();
            // in this case we just choose any lazy-type
            // since it will panic if this is evaluated anyway
            lazy!(Lazy::Int, {
                panic!("panicked: {}", eval!(message))
            })
        }

        #[| a:int() => b:int() => res:int()]
        fn "<"(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) < eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }


        #[| a:int() => b:int() => res:int()]
        fn ">"(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) > eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => b:int() => res:int()]
        fn "<="(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) <= eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }


        #[| a:int() => b:int() => res:int()]
        fn ">="(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) >= eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => b:int() => res:int()]
        fn "="(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) == eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }
    }
}
