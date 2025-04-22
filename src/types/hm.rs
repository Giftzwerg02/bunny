use std::cell::RefCell;
use std::cmp::min;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

/**
 * Base for of a Hindley-Milner Algorithm J implementation
 * Mostly translated from https://github.com/jfecher/algorithm-j/blob/master/j.ml
 */

/// Unique IDs for our types
type TypeVarId = u64;

/// How many Let-Levels deep we are.
/// A let level is a single func call with defs. E.g.:
/// (def a 1)
/// (def b 2)
/// (add a b)
///
/// Is actually:
/// let a = 1, b = 2 in a + b
/// in Hindley-Milner terms
type Level = u64;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    TUnit,

    /// A maybe-bound type variable
    TVar(Rc<RefCell<TypeVar>>),

    /// A basic type with a name
    Basic(String),

    /// The type of type application. E.g. a list<int>
    TApp(String, Vec<Type>),

    /// The type of function. Functions are curried in our type system.
    Fn(Box<Type>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    Bound(Type),
    Unbound(TypeVarId, Level),
}

/// A polytype: a list of type variable ids (for un-generalized typevars) and a type.
/// The list of ids represent those that remain monomorphic.
#[derive(Debug, Clone)]
pub struct PolyType {
    pub typevars: Vec<TypeVarId>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct HMState {
    current_level: Level,
    current_typevar: TypeVarId
}

impl HMState {
    pub fn new() -> HMState {
        HMState {
            current_level: 1,
            current_typevar: 0
        }
    }

    /// Increase the current level (enter a let-binding, for example).
    pub fn enter_level(&mut self) {
        self.current_level += 1;
    }

    /// Decrease the current level.
    pub fn exit_level(&mut self) {
        self.current_level -= 1;
    }

    pub fn newvar(&mut self) -> Type {
        self.current_typevar += 1;

        Type::TVar(Rc::new(RefCell::new(
            TypeVar::Unbound(self.current_typevar, self.current_level)))
        )
    }
}

impl PolyType {
    /// Instantiate a polytype by replacing its bound type variables
    /// with fresh monomorphic type variables.
    /// This “copies” the type, substituting the unbound (monomorphic) variable
    /// consistently.
    pub fn inst(&self, state: &mut HMState) -> Type {
        // A hash map from typevar id to new type.
        let mut replacements: HashMap<TypeVarId, Type> = HashMap::new();

        // For each typevar id in the polytype, associate a fresh variable.
        for tv in &self.typevars {
            replacements.insert(*tv, state.newvar());
        }

        // Replace the typevars in typ recursively.
        fn replace_tvs(typ: &Type, repl: &mut HashMap<TypeVarId, Type>) -> Type {
            match typ {
                Type::TUnit => typ.clone(),

                Type::TVar(tvar_rc) => {
                    match &*tvar_rc.borrow() {
                        TypeVar::Bound(t) => {
                            // recurse into the bound type
                            replace_tvs(t, repl)
                        }
                        TypeVar::Unbound(id, _level) => {
                            if let Some(t) = repl.get(id) {
                                t.clone()
                            } else {
                                typ.clone()
                            }
                        }
                    }
                }

                Type::Basic(_) => typ.clone(),

                Type::TApp(name, vars) => {
                    let new_vars = vars
                        .iter()
                        .map(|typ| replace_tvs(typ, repl))
                        .collect();

                    Type::TApp(name.clone(), new_vars)
                },

                Type::Fn(a, b) => {
                    let a_new = Box::new(replace_tvs(a, repl));
                    let b_new = Box::new(replace_tvs(b, repl));
                    Type::Fn(a_new, b_new)
                }
            }
        }

        replace_tvs(&self.typ, &mut replacements)
    }
}

impl Type {
    /// Walks over a type and collects type variable ids for unbound type variables
    /// that are deeper than the current level. Then wraps the type in a PolyType.
    /// This corresponds to the generalization of a let-binding.
    pub fn generalize(&self, state: &HMState) -> PolyType {
        let mut tvs = vec![];

        // Helper function that recursively collects unbound type variables.
        fn find_all_tvs(t: &Type, current_level: Level, acc: &mut Vec<TypeVarId>) {
            match t {
                Type::TUnit => {},

                Type::TVar(tvar_rc) => {
                    let tvar = tvar_rc.borrow();
                    match *tvar {
                        TypeVar::Bound(ref t_inner) => find_all_tvs(t_inner, current_level, acc),
                        TypeVar::Unbound(id, level) => {
                            // in OCaml: if level > !current_level then [n] else []
                            if level > current_level {
                                acc.push(id);
                            }
                        }
                    }
                }

                Type::Basic(_) => {},

                Type::TApp(_, vars) => {
                    vars.iter().for_each(|typ| find_all_tvs(typ, current_level, acc))
                }

                Type::Fn(a, b) => {
                    find_all_tvs(a, current_level, acc);
                    find_all_tvs(b, current_level, acc);
                }
            }
        }

        find_all_tvs(self, state.current_level, &mut tvs);
        // sort and remove duplicates
        tvs.sort();
        tvs.dedup();
        PolyType {
            typevars: tvs,
            typ: self.clone(),
        }
    }

    /// For lambda parameters that should not be generalized,
    /// mark the polytype as non-generalized by an empty list.
    pub fn dont_generalize(&self) -> PolyType {
        PolyType {
            typevars: vec![],
            typ: self.clone(),
        }
    }

    /// Unify two types, modifying type variables in-place.
    /// This implements the union-find unification algorithm with occurs check.
    pub fn unify(&self, other: &Type) {
        match (self, other) {
            // Dont create recursive bindings
            (_, _) if self == other => return,

            // This two maches replace the 'find' part of the algorithm
            (Type::TVar(tv), other) |
            (other, Type::TVar(tv)) => {
                Self::unify_typevar(tv.clone(), other)
            }

            (Type::Fn(a1, b1), Type::Fn(a2, b2)) => {
                a1.unify(a2);
                b1.unify(b2)
            }

            (Type::TApp(a, a_vars), Type::TApp(b, b_vars)) => {
                if a != b { panic!() }

                for (a_var, b_var) in a_vars.iter().zip(b_vars) {
                    a_var.unify(b_var);
                }
            }

            _ => panic!(),
        }
    }

    fn unify_typevar(tv_ref: Rc<RefCell<TypeVar>>, other: &Type){
        tv_ref.replace_with(|tv|{
            match tv {
                // This two maches replace the 'find' part of the algorithm
                TypeVar::Bound(bound_type) => {
                    other.unify(&bound_type);
                    tv.clone()
                }

                TypeVar::Unbound(id, level) => {
                    if Self::occurs(*id, *level, other) { panic!() }
                   TypeVar::Bound(other.clone())
                }
            }
        });
    }

    /// Checks whether a monomorphic type variable with id a_id occurs in type,
    /// updating the level of unbound typevars to the minimum encountered level.
    /// This is used in the unification step.
    /// Returns true if a_id is found, false otherwise.
    fn occurs(a_id: TypeVarId, a_level: Level, typ: &Type) -> bool {
        match typ {
            Type::TUnit => false,

            Type::TVar(tvar_rc) => {
                let mut tb = tvar_rc.borrow_mut();
                match &mut *tb {
                    TypeVar::Bound(t) => {
                        // descend into bound type
                        Self::occurs(a_id, a_level, t)
                    }
                    TypeVar::Unbound(b_id, b_level) => {
                        // update the level in place
                        *b_level = min(a_level, *b_level);
                        *b_id == a_id
                    }
                }
            }

            Type::Basic(_) => false,

            Type::TApp(_, vars) => {
                vars.iter().any(|typ| Self::occurs(a_id, a_level, typ))
            },

            Type::Fn(a, b) =>
                Self::occurs(a_id, a_level, a) || Self::occurs(a_id, a_level, b),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::TUnit =>
                write!(f, "()"),

            Type::TVar(tv) =>
                write!(f, "{}", tv.borrow()),

            Type::Basic(name) =>
                write!(f, "{}", name),

            Type::TApp(name, vars) => {
              let vars_str = vars
                  .iter()
                  .map(|var| var.to_string())
                  .collect::<Vec<String>>()
                  .join(" ");

                write!(f, "{}[{}]", name, vars_str)
            },

            Type::Fn(arg, ret) =>
                write!(f, "{} -> {}", arg, ret)
        }
    }
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeVar::Bound(t) =>
                write!(f, "<{}>", t),

            TypeVar::Unbound(tv_id, _) =>
                write!(f, "<'{}>", tv_id)
        }
    }
}