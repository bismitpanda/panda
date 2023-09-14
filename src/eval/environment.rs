use hashbrown::HashMap;

use crate::ast::ClassDeclAst;

use super::{EvaluatedModuleObject, Object};

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Environment {
    pub store: HashMap<String, (Object, bool)>,
    pub outer: Option<Box<Environment>>,
    pub types: HashMap<String, ClassDeclAst>,
    pub imports: HashMap<String, EvaluatedModuleObject>,
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
        if let Some(obj) = self.store.get(&name).cloned() {
            Some(obj)
        } else if let Some(ref outer) = self.outer {
            outer.get(name)
        } else {
            None
        }
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

    pub fn get_type(&self, name: &str) -> Option<ClassDeclAst> {
        if let Some(obj) = self.types.get(name).cloned() {
            Some(obj)
        } else if let Some(ref outer) = self.outer {
            outer.get_type(name)
        } else {
            None
        }
    }

    pub fn set_type(&mut self, name: String, class: ClassDeclAst) {
        self.types.insert(name, class);
    }

    pub fn get_import(&self, name: &str) -> Option<EvaluatedModuleObject> {
        if let Some(obj) = self.imports.get(name).cloned() {
            Some(obj)
        } else if let Some(ref outer) = self.outer {
            outer.get_import(name)
        } else {
            None
        }
    }

    pub fn set_import(&mut self, name: String, class: EvaluatedModuleObject) {
        self.imports.insert(name, class);
    }
}
