use std::sync::Arc;

use esvg::Element;
use polygonical::point::Point;

use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::library::runnable_expression::InterpreterSymbolTable;
use crate::runner::value::Lazy;
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
        #[| a:int() => ret:int()]
        fn "neg"(Lazy::Int(a)) {
            Lazy::new_int(-eval!(a))
        }

        #[| a:int() => b:int() => ret:int()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) + eval!(b))
        }

        #[| a:int() => b:int() => ret:int()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) - eval!(b))
        }

        #[| a:int() => b:int() => ret:int()]
        fn "*"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) * eval!(b))
        }

        #[| a:int() => b:int() => ret:int()]
        fn "/"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) / eval!(b))
        }

        #[forall a | arr:array(&a) => ret:a ]
        fn "first"(Lazy::Array(v)) {
            v[0].clone()
        }

        #[forall a | arr:array(&a) => ret:int() ]
        fn "len"(Lazy::Array(v)) {
            let len = v.len();
            Lazy::new_int(len as i64)
        }

        #[forall a | arr:array(&a) => idx:int() => ret:a ]
        fn "get"(Lazy::Array(v), Lazy::Int(idx)) {
            let idx = eval!(idx);
            v[idx as usize].clone()
        }

        #[forall a, b | fun:func1(&a, &b) => arr:array(&a) => ret:b ]
        fn "map"(Lazy::Lambda(f), Lazy::Array(v)) {
            let mut f = f.func.lock().unwrap();
            let mut res = vec![];
            for elem in eval!(v) {
                let mapped = f(vec![elem.clone()].into());
                res.push(mapped);
            }
            Lazy::new_array(res.into())
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
            let mut res = eval!(a);
            res.push_back(val.clone());
            Lazy::new_array(res)
        }

        #[| from:int() => to:int() => res:array(&int())]
        fn "range" (Lazy::Int(from), Lazy::Int(to)) {
            let from = eval!(from);
            let to = eval!(to);
            let range: Vec<_> = (from..to)
                .map(Lazy::new_int)
                .collect();
            Lazy::new_array(range.into())
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
            println!("Evaluated: {:?}", elem.clone().eval());
            elem.clone()
        }

        #[forall a | message:string() => ret:a]
        fn "panic"(Lazy::String(message)) {
            panic!("panicked: {}", eval!(message))
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

        #[| x:int() => y:int() => radius:int() => fill:color() => children:array(&opaque()) => ret:opaque()]
        fn "circle"(Lazy::Int(x), Lazy::Int(y), Lazy::Int(radius), Lazy::Color(fill), Lazy::Array(children)){
            let mut group = Element::new("g");

            let mut circle = esvg::shapes::circle(
                Point::new(eval!(x) as f32, eval!(y) as f32), // TODO pretty hacky
                eval!(radius) as i32
            );

            let (r, g, b, a) = eval!(fill).into_components();

            let color_str = format!("#{:02x}{:02x}{:02x}", r, g, b);  // TODO ignore alpha for now
            circle.set("fill", color_str);

            group.add(&circle);

            for child in eval!(children) {
                let Lazy::Opaque(child) = child else { panic!() };

                group.add(&child);
            }

            Lazy::new_opaque(group)
        }

        #[| x:int() => y:int() => w:int() => h:int() => fill:color() => children:array(&opaque()) => ret:opaque()]
        fn "rect"(Lazy::Int(x), Lazy::Int(y), Lazy::Int(w), Lazy::Int(h), Lazy::Color(fill), Lazy::Array(children)){
            let mut group = Element::new("g");

            let mut rect = esvg::shapes::rectangle(
                Point::new(eval!(x) as f32, eval!(y) as f32), // TODO pretty hacky
                eval!(w) as f64,
                eval!(h) as f64,
            );

            let (r, g, b, a) = eval!(fill).into_components();

            let color_str = format!("#{:02x}{:02x}{:02x}", r, g, b); // TODO ignore alpha for now
            rect.set("fill", color_str);

            group.add(&rect);

            for child in eval!(children) {
                let Lazy::Opaque(child) = child else { panic!() };

                group.add(&child);
            }

            Lazy::new_opaque(group)
        }
    }
}
