use std::sync::{Arc, Mutex};

use regex::Regex;

use esvg::Element;
use im::Vector;
use polygonical::point::Point;
pub use runnable_expression::InterpreterSymbolTable;

use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::runner::value::{to_color_str, LazyLambda, LazyType, Value};
use crate::types::InferenceState;
use crate::types::util::*;
use crate::{eval, lazy, library};

pub mod macros;
pub mod runnable_expression;

#[derive(Clone)]
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
        #[forall a, b, c | f1:func1(&a, &b) => f2:func1(&b, &c) => @func1(&a, &c)]
        fn "compose1"(Lazy::Lambda(f1), Lazy::Lambda(f2)) {
            lazy!(Lazy::Lambda, {
                LazyLambda::new(Arc::new(Mutex::new(move |args: Vector<_>| {
                    let mut f1 = f1.func.lock().unwrap();
                    let mut f2 = f2.func.lock().unwrap();
                    f2(vec![f1(args)].into())
                })))
            })
        }

        #[forall a, b | f:func1(&a, &b) => input:a => @b]
        fn "apply1"(Lazy::Lambda(f), input) {
            let mut f = f.func.lock().unwrap();
            f(vec![input].into())
        }


        #[| a:int() => @int()]
        fn "neg"(Lazy::Int(a)) {
            lazy!(Lazy::Int, -eval!(a))
        }

        #[| a:int() => b:int() => @int()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            lazy!(Lazy::Int, {
                let a = eval!(a);
                let b = eval!(b);
                a + b
            })
        }

        #[| a:int() => b:int() => @int()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            lazy!(Lazy::Int, {
                eval!(a) - eval!(b)
            })
        }

        #[| a:int() => b:int() => @int()]
        fn "*"(Lazy::Int(a), Lazy::Int(b)) {
            lazy!(Lazy::Int, eval!(a) * eval!(b))
        }

        #[| a:float() => b:float() => @float()]
        fn "*f"(Lazy::Float(a), Lazy::Float(b)) {
            lazy!(Lazy::Float, eval!(a) * eval!(b))
        }

        #[| a:int() => b:int() => @int()]
        fn "/"(Lazy::Int(a), Lazy::Int(b)) {
            lazy!(Lazy::Int, eval!(a) / eval!(b))
        }

        #[| a:float() => b:float() => @float()]
        fn "/f"(Lazy::Float(a), Lazy::Float(b)) {
            lazy!(Lazy::Float, eval!(a) / eval!(b))
        }

        #[forall a | arr:array(&a) => @a ]
        fn "first"(Lazy::Array(arr)) {
            arr[0].clone()
        }

        #[forall a | arr:array(&a) => @int() ]
        fn "len"(Lazy::Array(arr)) {
            lazy!(Lazy::Int, {
                let len = arr.len();
                len as i64
            })
        }

        #[forall a | arr:array(&a) => idx:int() => @a ]
        fn "get"(Lazy::Array(arr), Lazy::Int(idx)) {
            let idx = eval!(idx);
            arr[idx as usize].clone()
        }

        #[| text:string() => delimiter:string() => @array(&string())]
        fn "explode"(Lazy::String(text), Lazy::String(delimiter)) {
            lazy!(Lazy::Array, {
                let text = eval!(text);
                let delimiter = eval!(delimiter);

                text.to_string()
                    .leak() // Please ignore this for now
                    .split(delimiter.as_str())
                    .map(|s: &str| lazy!(Lazy::String, s.into()))
                    .collect::<Vector<Lazy>>()
                    .into()
            })
        }

        #[| format_str:string() => args:array(&string()) => @string() ]
        fn "format"(Lazy::String(format_str), Lazy::Array(args)) {
            lazy!(Lazy::String, {
                let mut format_str = eval!(format_str).to_string();
                let args = eval!(args);

                for arg in &args {
                    let Lazy::String(arg_str) = arg else {
                        panic!("Expected string argument for format");
                    };
                    
                    let arg_str = eval!(arg_str);
                    format_str = format_str.replacen("{}", &arg_str, 1);

                }

                format_str.into()
            })
        }

        #[| regex_str:string() => input:string() => with:string() => @string()]
        fn "replace-all"(Lazy::String(regex_str), Lazy::String(input), Lazy::String(with)) {
            lazy!(Lazy::String, {
                let re = Regex::new(&regex_str).unwrap();
                let with = eval!(with).to_string();
                let new_text = re.replace_all(&input, &with);
                new_text.into()
            })
        }

        #[forall a, b | fun:func1(&a, &b) => arr:array(&a) => @array(&b) ]
        fn "map"(Lazy::Lambda(fun), Lazy::Array(arr)) {
            lazy!(Lazy::Array, {
                let mut f = fun.func.lock().unwrap();
                let mut res = vec![];
                for elem in eval!(arr) {
                    let mapped = f(vec![elem].into());
                    res.push(mapped);
                }
                res.into()
            })
        }

        #[forall a, b | fun:func2(&b, &a, &b) => fst:b => arr:array(&a) => @b]
        fn "foldl" (Lazy::Lambda(fun), fst, Lazy::Array(arr)) {
            let mut f = fun.func.lock().unwrap();
            let mut acc = fst;
            for elem in eval!(arr) {
                acc = f(vec![acc, elem].into());
            }
            acc
        }

        #[forall a | arr:array(&a) => val:a => @array(&a)]
        fn "append" (Lazy::Array(arr), val) {
            lazy!(Lazy::Array, {
                let mut res = eval!(arr);
                res.push_back(val);
                res
            })
        }

        #[forall a | this:array(&a) => other:array(&a) => @array(&a)]
        fn "append-all"(Lazy::Array(this), Lazy::Array(other)) {
            lazy!(Lazy::Array, {
                let mut res = eval!(this);
                let to_add = eval!(other);
                res.append(to_add);
                res
            })
        }

        #[| from:int() => to:int() => @array(&int())]
        fn "range" (Lazy::Int(from), Lazy::Int(to)) {
            lazy!(Lazy::Array, {
                let from = **from;
                let to = **to;

                (from..to)
                    .map(|i| lazy!(Lazy::Int, i))
                    .collect()
            })
        }

        #[forall a | val:a => @a]
        fn "return" (val) {
            val
        }

        #[forall a | cond:int() => iftrue:a => iffalse:a => @a]
        fn "if"(Lazy::Int(cond), iftrue, iffalse){
            lazy!([iftrue -> i1, iffalse -> i2], {
                if eval!(cond) != 0 {
                    eval!(i1)
                } else {
                    eval!(i2)
                }
            })
        }

        #[| string:string() => @int()]
        fn "strlen"(Lazy::String(string)) {
            lazy!(Lazy::Int, {
                let len = eval!(string).len();
                len as i64
            })
        }

        #[forall a | elem:a => @a]
        fn "print"(elem){
            let e2 = elem.clone();
            lazy!([elem -> value], {
                println!("{}", e2.eval());
                eval!(value)
            })
        }

        #[forall a | message:string() => @a]
        fn "panic-int"(Lazy::String(message)) {
            lazy!(fromtype[a], {
                panic!("panicked: {}", eval!(message))
            })
        }

        #[| message:string() => @int()]
        fn "panic-int"(Lazy::String(message)) {
            lazy!(Lazy::Int, {
                panic!("panicked: {}", eval!(message))
            })
        }

        #[| message:string() => @string()]
        fn "panic-string"(Lazy::String(message)) {
            lazy!(Lazy::String, {
                panic!("panicked: {}", eval!(message))
            })
        }

        #[| a:int() => b:int() => @int()]
        fn "<"(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) < eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => b:int() => @int()]
        fn ">"(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) > eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => b:int() => @int()]
        fn "<="(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) <= eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }


        #[| a:int() => b:int() => @int()]
        fn ">="(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) >= eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => b:int() => @int()]
        fn "="(Lazy::Int(a), Lazy::Int(b)) {
            if eval!(a) == eval!(b) {
                ltrue!()
            } else {
                lfalse!()
            }
        }

        #[| a:int() => @int()]
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

        #[| x:int() => y:int() => @string()]
        fn "move"(Lazy::Int(x), Lazy::Int(y)){
            lazy!(Lazy::String, {
                format!("M {},{}", eval!(x), eval!(y)).into()
            })
        }

        #[| size:int() => @string()]
        fn "horizontal-line"(Lazy::Int(size)){
            lazy!(Lazy::String, {
                format!("h {}", eval!(size)).into()
            })
        }

        #[| size:int() => @string()]
        fn "vertical-line"(Lazy::Int(size)){
            lazy!(Lazy::String, {
                format!("v {}", eval!(size)).into()
            })
        }

        #[| x1:int() => y1:int() => x2:int() => y2:int() => x:int() => y:int() => @string()]
        fn "curve"(Lazy::Int(x1), Lazy::Int(y1), Lazy::Int(x2), Lazy::Int(y2), Lazy::Int(x), Lazy::Int(y)){
            lazy!(Lazy::String, {
                format!("C {} {}, {} {}, {} {}", eval!(x1), eval!(y1), eval!(x2), eval!(y2), eval!(x), eval!(y)).into()
            })
        }

        #[| commands:array(&string()) => fill:color() => @opaque()]
        fn "path"(Lazy::Array(commands), Lazy::Color(fill)) {
            lazy!(Lazy::Opaque, {
                let mut path_str = String::new();
                for command in eval!(commands) {
                    let Lazy::String(cmd) = command else { panic!() };
                    path_str += &eval!(cmd);
                    path_str += " ";
                }
                
                let mut path = Element::new("path");
                path.set("d", path_str.trim_end().to_string());

                let color_str = to_color_str(&eval!(fill));
                path.set("fill", color_str);

                path
            })
        }

        #[| points:array(&array(&int())) => fill:color() => @opaque() ]
        fn "polygon"(Lazy::Array(points), Lazy::Color(fill)){
            lazy!(Lazy::Opaque, {
                let mut polygon = Element::new("polygon");

                let mut point_str = String::new();
                for point in eval!(points) {
                    let Lazy::Array(coords) = point else { panic!() };
                    let coords = eval!(coords);

                    let Value::Int(x) = coords[0].clone().eval() else { panic!() };
                    let Value::Int(y) = coords[1].clone().eval() else { panic!() };

                    point_str += &format!("{x},{y} ");
                }

                polygon.set("points", point_str);
                let color_str = to_color_str(&eval!(fill));
                polygon.set("fill", color_str);

                polygon
            })
        }

        #[| x:int() => y:int() => radius:int() => fill:color() => children:array(&opaque()) => @opaque()]
        fn "circle"(Lazy::Int(x), Lazy::Int(y), Lazy::Int(radius), Lazy::Color(fill), Lazy::Array(children)){
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

        #[| x:int() => y:int() => w:int() => h:int() => fill:color() => children:array(&opaque()) => @opaque()]
        fn "rect"(Lazy::Int(x), Lazy::Int(y), Lazy::Int(w), Lazy::Int(h), Lazy::Color(fill), Lazy::Array(children)){
            lazy!(Lazy::Opaque, {
                let mut group = Element::new("g");

                // TODO pretty hacky
                let pos = Point::new(eval!(x) as f32, eval!(y) as f32);
                let mut rect = rectangle(pos, eval!(w) as f64, eval!(h) as f64);

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

        #[| x1:int() => y1:int() => x2:int() => y2:int() => stroke:color() => @opaque()]
        fn "line"(Lazy::Int(x1),Lazy::Int(y1),Lazy::Int(x2),Lazy::Int(y2),Lazy::Color(stroke)) {
            lazy!(Lazy::Opaque, {
                let mut group = Element::new("g");
                let mut line = esvg::path::create(&[
                    eval_point(x1, y1),
                    eval_point(x2, y2),
                ]);

                let color_str = to_color_str(&eval!(stroke));
                line.set("stroke", color_str);

                group.add(&line);

                group
            })
        }

        #[| elems:array(&opaque()) => @opaque()]
        fn "group"(Lazy::Array(elems)) {
            lazy!(Lazy::Opaque, {
                let mut group = Element::new("g");
                for elem in eval!(elems) {
                    let Lazy::Opaque(elem) = elem else { panic!(); };
                    group.add(&elem);
                }
                group
            })
        }

        #[| s:string() => x:int() => y:int() => @opaque()]
        fn "text"(Lazy::String(s), Lazy::Int(x), Lazy::Int(y)) {
            lazy!(Lazy::Opaque, {
                let loc = eval_point(x, y);
                let style = esvg::text::create_text_style(
                    "Roboto",
                    14,
                    "normal",
                    0.0,
                    "#000000",
                    "#000000",
                    1.0
                );
                esvg::text::create_text(eval!(s).to_string(), loc, &style)
            })
        }


        #[| s:string() => x:int() => y:int() => style:string() => @opaque()]
        fn "text-with-style"(Lazy::String(s), Lazy::Int(x), Lazy::Int(y), Lazy::String(style)) {
            lazy!(Lazy::Opaque, {
                let loc = eval_point(x, y);
                esvg::text::create_text(eval!(s).to_string(), loc, &eval!(style))
            })
        }

        #[| elem:opaque() => x:int() => y:int() => @opaque()]
        fn "translate"(Lazy::Opaque(elem), Lazy::Int(x), Lazy::Int(y)) {
            lazy!(Lazy::Opaque, {
                let mut elem = eval!(elem);
                let x = eval!(x);
                let y = eval!(y);
                elem.set("transform", format!("translate({x},{y})"));
                elem
            })
        }

        #[| elem:opaque() => degrees:float() => @opaque()]
        fn "rotate"(Lazy::Opaque(elem), Lazy::Float(degrees)) {
            lazy!(Lazy::Opaque, {
                let mut elem = eval!(elem);
                let degrees = eval!(degrees);
                elem.set("transform", format!("rotate({degrees})"));
                elem
            })
        }

        #[| elem:opaque() => degrees:float() => @opaque()]
        fn "rotate-self"(Lazy::Opaque(elem), Lazy::Float(degrees)) {
            lazy!(Lazy::Opaque, {
                let mut elem = eval!(elem);
                let degrees = eval!(degrees);
                let x = elem.get("x").unwrap_or("0".to_owned());
                let y = elem.get("y").unwrap_or("0".to_owned());
                elem.set("transform", format!("rotate({degrees},{x},{y})"));
                elem
            })
        }

        #[| f:float() => @int()]
        fn "floor"(Lazy::Float(f)) {
            lazy!(Lazy::Int, eval!(f) as i64)
        }

        #[| i:int() => @float()]
        fn "to-float"(Lazy::Int(i)) {
            lazy!(Lazy::Float, eval!(i) as f64)
        }

        #[forall a | input:a => @string()]
        fn "to-string"(input) {
            lazy!(Lazy::String, {
                let a = input.eval();
                format!("{a}").into()
            })
        }

        #[| a:array(&string()) => sep:string() => @string()]
        fn "join"(Lazy::Array(a), Lazy::String(sep)) {
            lazy!(Lazy::String, {
                let a = eval!(a);
                let a = a.into_iter()
                    .map(|e| e.eval())
                    .map(|e| {
                        let Value::String(e) = e else { panic!(); };
                        e
                    })
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>();
                let sep = eval!(sep);
                let joined = a.join(&sep);
                joined.into()
            })
        }
    }
}

fn eval_point(x: LazyType<i64>, y: LazyType<i64>) -> Point {
    Point::new(eval!(x) as f32, eval!(y) as f32)
}

fn rectangle(pos: Point, width: f64, height: f64) -> Element {
    let offset = Point::new(width / 2.0, height / 2.0);
    let center = pos.translate(&offset);
    esvg::shapes::rectangle(center, width, height)
}
