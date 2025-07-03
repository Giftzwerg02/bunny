//! Base for of a Hindley-Milner Algorithm J implementation
//! Mostly translated from https://github.com/jfecher/algorithm-j/blob/master/j.ml

use std::cell::RefCell;
use std::cmp::min;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub enum HMError {
    UnificationError(Type, Type),

    OccursCheckError(Type)
}

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

impl Default for HMState {
    fn default() -> Self {
        Self::new()
    }
}

impl HMState {
    pub fn new() -> HMState {
        HMState {
            current_level: 0,
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
    pub fn unify(&self, other: &Type) -> Result<(), HMError> {
        match (self, other) {
            // Dont create recursive bindings
            (_, _) if self == other => Ok(()),

            // This two maches replace the 'find' part of the algorithm
            (Type::TVar(tv), other) |
            (other, Type::TVar(tv)) =>
                Self::unify_typevar(tv.clone(), other),

            (Type::Fn(a1, b1), Type::Fn(a2, b2)) => {
                a1.unify(a2)?;
                b1.unify(b2)
            }

            (Type::TApp(a, a_vars), Type::TApp(b, b_vars)) if a == b => {
                for (a_var, b_var) in a_vars.iter().zip(b_vars) {
                    a_var.unify(b_var)?;
                }

                Ok(())
            }

            (Type::Fn(a, b), o) |
            (o, Type::Fn(a, b)) if **a == Type::TUnit =>
                b.unify(o),

            _ => Err(HMError::UnificationError(self.clone(), other.clone())),
        }
    }

    fn unify_typevar(tv_ref: Rc<RefCell<TypeVar>>, other: &Type) -> Result<(), HMError> {
        let tv_guard = tv_ref.borrow_mut();

        match &*tv_guard {
            TypeVar::Bound(bound_type) => {
                // If tv_ref is already bound to bound_type, we need to unify bound_type with 'other'.
                // Clone 'bound_type' to avoid borrow checker issues if 'other' refers back to 'tv_ref'
                // or if 'unify' needs to modify parts of 'bound_type' (which it does by changing TVars).
                let current_bound_type = bound_type.clone();
                
                // Release the mutable borrow on tv_ref *before* calling unify.
                // This is critical because 'unify' might attempt to borrow 'tv_ref' again,
                // for example, if 'other' is or contains 'TVar(tv_ref)'.
                drop(tv_guard);
                
                current_bound_type.unify(other)
            }
            TypeVar::Unbound(id, level) => {
                // 'tv_ref' is an unbound type variable.
                // Before binding it, we must perform an occurs check to prevent infinite types.
                // Note: The case where 'other' is 'TVar(tv_ref)' (i.e., unifying a TVar with itself)
                // is handled by the `self == other` check in the main `unify` function.
                
                let var_id = *id;
                let var_level = *level;
                
                // Release the mutable borrow before calling occurs to avoid borrow conflicts
                drop(tv_guard);
                
                if Self::occurs(var_id, var_level, other) {
                    Err(HMError::OccursCheckError(other.clone()))
                } else {
                    // Occurs check passed. Bind the type variable tv_ref to 'other'.
                    // Reacquire the mutable borrow to update the type variable.
                    let mut tv_guard = tv_ref.borrow_mut();
                    *tv_guard = TypeVar::Bound(other.clone());
                    Ok(())
                }
            }
        }
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
                        // Clone the bound type to avoid borrow conflicts when recursing
                        let bound_type = t.clone();
                        // Release the borrow before recursing
                        drop(tb);
                        Self::occurs(a_id, a_level, &bound_type)
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
                write!(f, "{name}"),

            Type::TApp(name, vars) => {
              let vars_str = vars
                  .iter()
                  .map(|var| var.to_string())
                  .collect::<Vec<String>>()
                  .join(" ");

                write!(f, "{name}[{vars_str}]")
            },

            Type::Fn(arg, ret) =>
                write!(f, "{arg} -> {ret}")
        }
    }
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeVar::Bound(t) =>
                write!(f, "{t}"),

            TypeVar::Unbound(tv_id, _) =>
                write!(f, "'{tv_id}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to create basic types
    fn basic(name: &str) -> Type {
        Type::Basic(name.to_string())
    }

    // Helper function to create type applications
    fn tapp(name: &str, vars: Vec<Type>) -> Type {
        Type::TApp(name.to_string(), vars)
    }

    // Helper function to create function types
    fn func(arg: Type, ret: Type) -> Type {
        Type::Fn(Box::new(arg), Box::new(ret))
    }

    #[test]
    fn test_unify_identical_types() {
        let int_type = basic("int");
        let other_int = basic("int");
        
        assert!(int_type.unify(&other_int).is_ok());
    }

    #[test]
    fn test_unify_different_basic_types() {
        let int_type = basic("int");
        let string_type = basic("string");
        
        assert!(int_type.unify(&string_type).is_err());
    }

    #[test]
    fn test_unify_tvar_with_basic() {
        let mut state = HMState::new();
        let tvar = state.newvar();
        let int_type = basic("int");
        
        assert!(tvar.unify(&int_type).is_ok());
        
        // Check that the TVar is now bound to int
        if let Type::TVar(tv_ref) = &tvar {
            if let TypeVar::Bound(bound_type) = &*tv_ref.borrow() {
                assert_eq!(*bound_type, int_type);
            } else {
                panic!("TVar should be bound");
            }
        } else {
            panic!("Expected TVar");
        }
    }

    #[test]
    fn test_unify_basic_with_tvar() {
        let mut state = HMState::new();
        let int_type = basic("int");
        let tvar = state.newvar();
        
        assert!(int_type.unify(&tvar).is_ok());
        
        // Check that the TVar is now bound to int
        if let Type::TVar(tv_ref) = &tvar {
            if let TypeVar::Bound(bound_type) = &*tv_ref.borrow() {
                assert_eq!(*bound_type, int_type);
            } else {
                panic!("TVar should be bound");
            }
        } else {
            panic!("Expected TVar");
        }
    }

    #[test]
    fn test_unify_two_tvars() {
        let mut state = HMState::new();
        let tvar1 = state.newvar();
        let tvar2 = state.newvar();
        
        assert!(tvar1.unify(&tvar2).is_ok());
        
        // One of them should be bound to the other
        let (bound, unbound) = if let Type::TVar(tv1) = &tvar1 {
            if let TypeVar::Bound(_) = &*tv1.borrow() {
                (&tvar1, &tvar2)
            } else {
                (&tvar2, &tvar1)
            }
        } else {
            panic!("Expected TVar");
        };

        if let Type::TVar(bound_ref) = bound {
            if let TypeVar::Bound(bound_type) = &*bound_ref.borrow() {
                assert_eq!(*bound_type, *unbound);
            } else {
                panic!("Expected bound TVar");
            }
        }
    }

    #[test]
    fn test_unify_function_types() {
        let int_type = basic("int");
        let string_type = basic("string");
        
        let func1 = func(int_type.clone(), string_type.clone());
        let func2 = func(int_type.clone(), string_type.clone());
        
        assert!(func1.unify(&func2).is_ok());
    }

    #[test]
    fn test_unify_function_types_different_args() {
        let int_type = basic("int");
        let string_type = basic("string");
        let bool_type = basic("bool");
        
        let func1 = func(int_type.clone(), string_type.clone());
        let func2 = func(bool_type.clone(), string_type.clone());
        
        assert!(func1.unify(&func2).is_err());
    }

    #[test]
    fn test_unify_function_with_tvar() {
        let mut state = HMState::new();
        let int_type = basic("int");
        let tvar = state.newvar();
        
        let func_type = func(int_type.clone(), tvar.clone());
        let string_type = basic("string");
        let expected_func = func(int_type, string_type.clone());
        
        assert!(func_type.unify(&expected_func).is_ok());
        
        // Check that tvar is bound to string
        if let Type::TVar(tv_ref) = &tvar {
            if let TypeVar::Bound(bound_type) = &*tv_ref.borrow() {
                assert_eq!(*bound_type, string_type);
            } else {
                panic!("TVar should be bound");
            }
        }
    }

    #[test]
    fn test_unify_tapp_same_constructor() {
        let int_type = basic("int");
        
        let list_int = tapp("List", vec![int_type.clone()]);
        let list_int2 = tapp("List", vec![int_type.clone()]);
        
        assert!(list_int.unify(&list_int2).is_ok());
    }

    #[test]
    fn test_unify_tapp_different_constructors() {
        let int_type = basic("int");
        
        let list_int = tapp("List", vec![int_type.clone()]);
        let option_int = tapp("Option", vec![int_type]);
        
        assert!(list_int.unify(&option_int).is_err());
    }

    #[test]
    fn test_unify_tapp_with_tvars() {
        let mut state = HMState::new();
        let int_type = basic("int");
        let tvar = state.newvar();
        
        let list_int = tapp("List", vec![int_type.clone()]);
        let list_tvar = tapp("List", vec![tvar.clone()]);
        
        assert!(list_int.unify(&list_tvar).is_ok());
        
        // Check that tvar is bound to int
        if let Type::TVar(tv_ref) = &tvar {
            if let TypeVar::Bound(bound_type) = &*tv_ref.borrow() {
                assert_eq!(*bound_type, int_type);
            } else {
                panic!("TVar should be bound");
            }
        }
    }

    #[test]
    fn test_unify_unit_function_optimization() {
        let int_type = basic("int");
        let unit_to_int = func(Type::TUnit, int_type.clone());
        
        assert!(unit_to_int.unify(&int_type).is_ok());
        assert!(int_type.unify(&unit_to_int).is_ok());
    }

    #[test]
    fn test_occurs_check_simple() {
        let mut state = HMState::new();
        let tvar = state.newvar();
        
        // Try to unify tvar with List[tvar] - should fail due to occurs check
        let list_tvar = tapp("List", vec![tvar.clone()]);
        
        assert!(tvar.unify(&list_tvar).is_err());
        
        if let Err(HMError::OccursCheckError(_)) = tvar.unify(&list_tvar) {
            // Expected
        } else {
            panic!("Expected OccursCheckError");
        }
    }

    #[test]
    fn test_occurs_check_in_function() {
        let mut state = HMState::new();
        let tvar = state.newvar();
        let int_type = basic("int");
        
        // Try to unify tvar with int -> tvar - should fail due to occurs check
        let func_tvar = func(int_type, tvar.clone());
        
        if let Err(HMError::OccursCheckError(_)) = tvar.unify(&func_tvar) {
            // Expected
        } else {
            panic!("Expected OccursCheckError");
        }
    }

    #[test]
    fn test_occurs_check_nested() {
        let mut state = HMState::new();
        let tvar = state.newvar();
        let _int_type = basic("int");
        
        // Create List[List[tvar]]
        let inner_list = tapp("List", vec![tvar.clone()]);
        let outer_list = tapp("List", vec![inner_list]);
        
        if let Err(HMError::OccursCheckError(_)) = tvar.unify(&outer_list) {
            // Expected
        } else {
            panic!("Expected OccursCheckError");
        }
    }

    #[test]
    fn test_occurs_check_through_bound_var() {
        let mut state = HMState::new();
        let tvar1 = state.newvar();
        let tvar2 = state.newvar();
        
        // First bind tvar2 to tvar1
        assert!(tvar2.unify(&tvar1).is_ok());
        
        // Now try to unify tvar1 with List[tvar2] 
        // This should fail because tvar2 is bound to tvar1,
        // so we're essentially trying to unify tvar1 with List[tvar1]
        let list_tvar2 = tapp("List", vec![tvar2]);
        
        if let Err(HMError::OccursCheckError(_)) = tvar1.unify(&list_tvar2) {
            // Expected
        } else {
            panic!("Expected OccursCheckError");
        }
    }

    #[test]
    fn test_generalization_basic() {
        let mut state = HMState::new();
        
        // Enter a level to simulate let-binding context
        state.enter_level();
        let tvar = state.newvar();
        
        // Exit level and generalize
        state.exit_level();
        let polytype = tvar.generalize(&state);
        
        // Should contain the type variable id
        assert_eq!(polytype.typevars.len(), 1);
        assert_eq!(polytype.typ, tvar);
    }

    #[test]
    fn test_generalization_with_levels() {
        let mut state = HMState::new();
        
        // Enter level 1
        state.enter_level();
        
        // Create a type variable at level 1
        let tvar1 = state.newvar();
        
        // Enter level 2
        state.enter_level();
        
        // Create a type variable at level 2
        let tvar2 = state.newvar();
        
        // Create a function type: tvar1 -> tvar2
        let func_type = func(tvar1.clone(), tvar2.clone());
        
        // Generalize at level 2 - should only generalize tvar2 (level 2 > 2 is false, so none)
        let polytype = func_type.generalize(&state);
        
        // Should contain no type variables since we're still at level 2
        assert_eq!(polytype.typevars.len(), 0);
        
        // Exit to level 1
        state.exit_level();
        
        // Now generalize at level 1 - should generalize tvar2 (level 2 > 1 is true)
        let polytype = func_type.generalize(&state);
        assert_eq!(polytype.typevars.len(), 1);
        
        // Exit to level 0
        state.exit_level();
        
        // Now generalize at level 0 - should generalize both (levels 1,2 > 0)
        let polytype = func_type.generalize(&state);
        assert_eq!(polytype.typevars.len(), 2);
    }

    #[test]
    fn test_generalization_bound_vars() {
        let mut state = HMState::new();
        let tvar = state.newvar();
        let int_type = basic("int");
        
        // Bind the type variable
        tvar.unify(&int_type).unwrap();
        
        // Generalize - should not include bound variables
        let polytype = tvar.generalize(&state);
        assert_eq!(polytype.typevars.len(), 0);
        
        // But the type itself should be the bound type (int)
        if let Type::TVar(tv_ref) = &polytype.typ {
            if let TypeVar::Bound(bound_type) = &*tv_ref.borrow() {
                assert_eq!(*bound_type, int_type);
            }
        }
    }

    #[test]
    fn test_dont_generalize() {
        let mut state = HMState::new();
        let tvar = state.newvar();
        
        let polytype = tvar.dont_generalize();
        
        // Should have empty typevars list
        assert_eq!(polytype.typevars.len(), 0);
        assert_eq!(polytype.typ, tvar);
    }

    #[test]
    fn test_instantiation() {
        let mut state = HMState::new();
        
        // Enter level and create a type variable that will be generalized
        state.enter_level();
        let tvar = state.newvar();
        
        // Get the type variable id for later comparison
        let original_id = if let Type::TVar(tv_ref) = &tvar {
            if let TypeVar::Unbound(id, _) = &*tv_ref.borrow() {
                *id
            } else {
                panic!("Expected unbound TVar");
            }
        } else {
            panic!("Expected TVar");
        };
        
        // Exit level and generalize
        state.exit_level();
        let polytype = tvar.generalize(&state);
        let instantiated = polytype.inst(&mut state);
        
        // The instantiated type should be a different type variable
        if let Type::TVar(tv_ref) = &instantiated {
            if let TypeVar::Unbound(id, _) = &*tv_ref.borrow() {
                assert_ne!(*id, original_id);
            } else {
                panic!("Expected unbound TVar");
            }
        } else {
            panic!("Expected TVar");
        }
    }

    #[test]
    fn test_instantiation_multiple_vars() {
        let mut state = HMState::new();
        let tvar1 = state.newvar();
        let tvar2 = state.newvar();
        
        let func_type = func(tvar1.clone(), tvar2.clone());
        let polytype = func_type.generalize(&state);
        
        // Instantiate twice
        let inst1 = polytype.inst(&mut state);
        let inst2 = polytype.inst(&mut state);
        
        // The two instantiations should be unifiable (same structure)
        // but contain different fresh type variables
        assert!(inst1.unify(&inst2).is_ok());
    }

    #[test]
    fn test_complex_unification_scenario() {
        let mut state = HMState::new();
        
        // Create: List[a] -> List[b] where a and b are type variables
        let tvar_a = state.newvar();
        let tvar_b = state.newvar();
        let list_a = tapp("List", vec![tvar_a.clone()]);
        let list_b = tapp("List", vec![tvar_b.clone()]);
        let func_type = func(list_a, list_b);
        
        // Create: List[int] -> List[string]
        let int_type = basic("int");
        let string_type = basic("string");
        let list_int = tapp("List", vec![int_type.clone()]);
        let list_string = tapp("List", vec![string_type.clone()]);
        let concrete_func = func(list_int, list_string);
        
        // Unify them
        assert!(func_type.unify(&concrete_func).is_ok());
        
        // Check that a is bound to int and b is bound to string
        if let Type::TVar(tv_a) = &tvar_a {
            if let TypeVar::Bound(bound) = &*tv_a.borrow() {
                assert_eq!(*bound, int_type);
            } else {
                panic!("TVar a should be bound");
            }
        }
        
        if let Type::TVar(tv_b) = &tvar_b {
            if let TypeVar::Bound(bound) = &*tv_b.borrow() {
                assert_eq!(*bound, string_type);
            } else {
                panic!("TVar b should be bound");
            }
        }
    }

    #[test]
    fn test_level_updating_in_occurs_check() {
        let mut state = HMState::new();
        
        // Enter level 1
        state.enter_level();
        let tvar1 = state.newvar();
        
        // Enter level 2
        state.enter_level();
        let tvar2 = state.newvar();
        
        // Get the initial levels
        let (id1, level1) = if let Type::TVar(tv) = &tvar1 {
            if let TypeVar::Unbound(id, level) = &*tv.borrow() {
                (*id, *level)
            } else {
                panic!("Expected unbound TVar");
            }
        } else {
            panic!("Expected TVar");
        };
        
        let (_id2, level2) = if let Type::TVar(tv) = &tvar2 {
            if let TypeVar::Unbound(id, level) = &*tv.borrow() {
                (*id, *level)
            } else {
                panic!("Expected unbound TVar");
            }
        } else {
            panic!("Expected TVar");
        };
        
        assert_eq!(level1, 1);
        assert_eq!(level2, 2);
        
        // Use occurs check - this should update tvar2's level to min(level1, level2) = 1
        let result = Type::occurs(id1, level1, &tvar2);
        assert!(!result); // Different variables, so should return false
        
        // Check that tvar2's level was updated
        if let Type::TVar(tv) = &tvar2 {
            if let TypeVar::Unbound(_, level) = &*tv.borrow() {
                assert_eq!(*level, 1); // Should be updated to min(1, 2) = 1
            } else {
                panic!("Expected unbound TVar");
            }
        }
    }

    #[test]
    fn test_complex_generalization_scenario() {
        let mut state = HMState::new();
        
        // Simulate: let f = fun x -> x in (f 1, f true)
        // This should generalize f to have type: forall a. a -> a
        
        // Enter let level
        state.enter_level();
        
        // Create identity function: a -> a
        let tvar_a = state.newvar();
        let identity_type = func(tvar_a.clone(), tvar_a.clone());
        
        // Exit let level and generalize
        state.exit_level();
        let generalized_identity = identity_type.generalize(&state);
        
        // Should have generalized the type variable
        assert_eq!(generalized_identity.typevars.len(), 1);
        
        // Now instantiate twice for two different uses
        let use1 = generalized_identity.inst(&mut state);
        let use2 = generalized_identity.inst(&mut state);
        
        // Use with int: should unify use1 with int -> int
        let int_type = basic("int");
        let int_to_int = func(int_type.clone(), int_type.clone());
        assert!(use1.unify(&int_to_int).is_ok());
        
        // Use with bool: should unify use2 with bool -> bool
        let bool_type = basic("bool");
        let bool_to_bool = func(bool_type.clone(), bool_type.clone());
        assert!(use2.unify(&bool_to_bool).is_ok());
    }

    #[test]
    fn test_unification_with_nested_functions() {
        let mut state = HMState::new();
        
        // Test: (a -> b) -> (b -> c) -> (a -> c)
        let tvar_a = state.newvar();
        let tvar_b = state.newvar();
        let tvar_c = state.newvar();
        
        let a_to_b = func(tvar_a.clone(), tvar_b.clone());
        let b_to_c = func(tvar_b.clone(), tvar_c.clone());
        let a_to_c = func(tvar_a.clone(), tvar_c.clone());
        
        let compose_type1 = func(a_to_b, func(b_to_c, a_to_c));
        
        // Create another compose type with different variables
        let tvar_x = state.newvar();
        let tvar_y = state.newvar();
        let tvar_z = state.newvar();
        
        let x_to_y = func(tvar_x.clone(), tvar_y.clone());
        let y_to_z = func(tvar_y.clone(), tvar_z.clone());
        let x_to_z = func(tvar_x.clone(), tvar_z.clone());
        
        let compose_type2 = func(x_to_y, func(y_to_z, x_to_z));
        
        // They should unify successfully
        assert!(compose_type1.unify(&compose_type2).is_ok());
    }

    #[test]
    fn test_occurs_check_with_multiple_levels() {
        let mut state = HMState::new();
        
        state.enter_level();
        let tvar1 = state.newvar();
        
        state.enter_level();
        let tvar2 = state.newvar();
        
        // Try to create a cycle: tvar1 -> List[tvar2], tvar2 -> tvar1
        let list_tvar2 = tapp("List", vec![tvar2.clone()]);
        assert!(tvar1.unify(&list_tvar2).is_ok());
        
        // Now this should fail due to occurs check
        if let Err(HMError::OccursCheckError(_)) = tvar2.unify(&tvar1) {
            // Expected
        } else {
            panic!("Expected OccursCheckError");
        }
    }

    #[test]
    fn test_unification_of_partial_application_types() {
        let mut state = HMState::new();
        
        // Test partial application: List[int] vs List[a] where a should unify with int
        let int_type = basic("int");
        let tvar_a = state.newvar();
        
        let list_int = tapp("List", vec![int_type.clone()]);
        let list_a = tapp("List", vec![tvar_a.clone()]);
        
        assert!(list_int.unify(&list_a).is_ok());
        
        // Check that tvar_a is now bound to int
        if let Type::TVar(tv_ref) = &tvar_a {
            if let TypeVar::Bound(bound_type) = &*tv_ref.borrow() {
                assert_eq!(*bound_type, int_type);
            } else {
                panic!("TVar should be bound");
            }
        }
        
        // Test with nested type applications
        let nested_list_int = tapp("List", vec![list_int]);
        let nested_list_a = tapp("List", vec![list_a]);
        
        assert!(nested_list_int.unify(&nested_list_a).is_ok());
    }
}
