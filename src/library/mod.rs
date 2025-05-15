
use std::sync::{Arc, Mutex};

use esvg::Element;
use im::Vector;
use polygonical::point::Point;
pub use runnable_expression::InterpreterSymbolTable;

use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::runner::value::{to_color_str, Lazy, LazyLambda};
use crate::types::InferenceState;
use crate::types::util::*;
use crate::{eval, lazy, library};

pub mod macros;
pub mod runnable_expression;

pub struct Library {
    pub scoped: SymbolTable<ScopedStageInfo>,
    pub typed: InferenceState,
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

pub fn standard_library() -> Library {
    library! {
        #[forall a, b, c | f1:func1(&a, &b) => f2:func1(&b, &c) => f3:func1(&a, &c)]
        fn "compose1"(Lazy::Lambda(f1), Lazy::Lambda(f2)) {
            let f1 = f1.clone();    
            let f2 = f2.clone();    
            lazy!(Lazy::Lambda, {
                LazyLambda::new(Arc::new(Mutex::new(move |args: Vector<_>| {
                    let mut f1 = f1.func.lock().unwrap();
                    let mut f2 = f2.func.lock().unwrap();
                    f2(vec![f1(args)].into())
                })))
            })
        }

        #[forall a, b | f:func1(&a, &b) => input:a => ret:b]
        fn "apply1"(Lazy::Lambda(f), input) {
            let f = f.clone();
            let mut f = f.func.lock().unwrap();
            let input = input.clone();
            f(vec![input].into())
        }


        #[| a:int() => ret:int()]
        fn "neg"(Lazy::Int(a)) {
            let a = a.clone();
            lazy!(Lazy::Int, -eval!(a))
        }

        #[| a:int() => b:int() => ret:int()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            let a = a.clone();
            let b = b.clone();
            lazy!(Lazy::Int, {
                let a = eval!(a);
                let b = eval!(b);
                a + b
            })
        }

        #[| a:int() => b:int() => ret:int()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            let a = a.clone();
            let b = b.clone();
            lazy!(Lazy::Int, {
                eval!(a) - eval!(b)
            })
        }

        #[| a:int() => b:int() => ret:int()]
        fn "*"(Lazy::Int(a), Lazy::Int(b)) {
            let a = a.clone();
            let b = b.clone();
            lazy!(Lazy::Int, eval!(a) * eval!(b))
        }

        #[| a:int() => b:int() => ret:int()]
        fn "/"(Lazy::Int(a), Lazy::Int(b)) {
            let a = a.clone();
            let b = b.clone();
            lazy!(Lazy::Int, eval!(a) / eval!(b))
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
                for elem in eval!(v) {
                    let mapped = f(vec![elem].into());
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
                
                (from..to)
                    .map(|i| lazy!(Lazy::Int, i))
                    .collect()
            })
        }

        #[forall a | val:a.clone() => ret:a]
        fn "return" (v) {
            v.clone()
        }

        #[forall a | cond:int() => iftrue:a => iffalse:a => ret:a]
        fn "if"(Lazy::Int(cond), iftrue, iffalse){
            let cond = cond.clone();
            let iftrue = iftrue.clone();
            let iffalse = iffalse.clone();

            lazy!([iftrue -> i1, iffalse -> i2], {
                if eval!(cond) != 0 {
                    eval!(i1)
                } else {
                    eval!(i2)
                }
            })
        }

        #[| str:string() => ret:int()]
        fn "strlen"(Lazy::String(s)) {
            let s = s.clone();
            lazy!(Lazy::Int, {
                let len = eval!(s).len();
                len as i64
            })
        }

        #[forall a | elem:a => ret:a]
        fn "print"(elem){
            let elem = elem.clone();
            let e2 = elem.clone();
            lazy!([elem -> value], {
                println!("{}", e2.eval());
                eval!(value)
            })
        }

        #[forall a | message:string() => ret:a]
        fn "panic"(Lazy::String(message)) {
            let message = message.clone();
            lazy!(fromtype[a], {
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

        #[| a:int() => ret:int()]
        fn "not"(Lazy::Int(a)) {
            let a = a.clone();
            lazy!(Lazy::Int, {
                if eval!(a) == 0 {
                    1
                } else {
                    0
                }
            })
        }

        #[| x:int() => y:int() => radius:int() => fill:color() => children:array(&opaque()) => ret:opaque()]
        fn "circle"(Lazy::Int(x), Lazy::Int(y), Lazy::Int(radius), Lazy::Color(fill), Lazy::Array(children)){
            let x = x.clone();
            let y = y.clone();
            let radius = radius.clone();
            let fill = fill.clone();
            let children = children.clone();
            lazy!(Lazy::Opaque, {
                let mut group = Element::new("g");

                let mut circle = esvg::shapes::circle(
                    Point::new(eval!(x) as f32, eval!(y) as f32), // TODO pretty hacky
                    eval!(radius) as i32
                );

                let color_str = to_color_str(&eval!(fill));
                circle.set("fill", color_str);

                group.add(&circle);

                for child in eval!(children) {
                    let Lazy::Opaque(child) = child else { panic!() };

                    group.add(&child);
                }

                group
            })
        }

        #[| x:int() => y:int() => w:int() => h:int() => fill:color() => children:array(&opaque()) => ret:opaque()]
        fn "rect"(Lazy::Int(x), Lazy::Int(y), Lazy::Int(w), Lazy::Int(h), Lazy::Color(fill), Lazy::Array(children)){
            let x = x.clone();
            let y = y.clone();
            let w = w.clone();
            let h = h.clone();
            let fill = fill.clone();
            let children = children.clone();
            lazy!(Lazy::Opaque, {
                let mut group = Element::new("g");

                let mut rect = esvg::shapes::rectangle(
                    Point::new(eval!(x) as f32, eval!(y) as f32), // TODO pretty hacky
                    eval!(w) as f64,
                    eval!(h) as f64,
                );

                let color_str = to_color_str(&eval!(fill));
                rect.set("fill", color_str);

                group.add(&rect);

                for child in eval!(children) {
                    let Lazy::Opaque(child) = child else { panic!() };

                    group.add(&child);
                }

                group
            })
        }
    }
}
