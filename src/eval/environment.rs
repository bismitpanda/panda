use hashbrown::HashMap;

use super::{EvaluatedModule, Object};
use crate::ast::ClassDecl;

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Environment {
    pub store: HashMap<String, (Object, bool)>,
    pub outer: Option<Box<Environment>>,
    pub types: HashMap<String, ClassDecl>,
    pub imports: HashMap<String, EvaluatedModule>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
            types: HashMap::new(),
            imports: HashMap::new(),
        }
    }

    pub fn new_enclosed(outer: Self) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
            types: HashMap::new(),
            imports: HashMap::new(),
        }
    }

    pub fn get(&self, name: String) -> Option<(Object, bool)> {
        self.store.get(&name).cloned().map_or_else(
            || {
                self.outer
                    .as_ref()
                    .map_or_else(|| None, |outer| outer.get(name))
            },
            Some,
        )
    }

    pub fn set(&mut self, name: String, val: Object, mutable: bool) {
        self.store.insert(name, (val, mutable));
    }

    pub fn delete(&mut self, name: &str) -> Option<Object> {
        if let Some((obj, _)) = self.store.remove(name) {
            Some(obj)
        } else if let Some(ref mut outer) = self.outer {
            outer.delete(name)
        } else {
            None
        }
    }

    pub fn get_type(&self, name: &str) -> Option<ClassDecl> {
        self.types.get(name).cloned().map_or_else(
            || {
                self.outer
                    .as_ref()
                    .map_or_else(|| None, |outer| outer.get_type(name))
            },
            Some,
        )
    }

    pub fn set_type(&mut self, name: String, class: ClassDecl) {
        self.types.insert(name, class);
    }

    pub fn get_import(&self, name: &str) -> Option<EvaluatedModule> {
        self.imports.get(name).cloned().map_or_else(
            || {
                self.outer
                    .as_ref()
                    .map_or_else(|| None, |outer| outer.get_import(name))
            },
            Some,
        )
    }

    pub fn set_import(&mut self, name: String, class: EvaluatedModule) {
        self.imports.insert(name, class);
    }
}
