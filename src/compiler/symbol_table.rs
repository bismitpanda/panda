use hashbrown::HashMap;

use crate::{ast::ClassDeclAst, object::CompiledModuleObject};

#[derive(strum::Display, Clone, Copy, PartialEq, Eq, Debug)]
#[strum(serialize_all = "UPPERCASE")]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
    pub mutable: bool,
}

impl Symbol {
    pub fn new(name: &str, scope: SymbolScope, index: usize, mutable: bool) -> Self {
        Self {
            name: name.to_string(),
            scope,
            index,
            mutable,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Symbol>,
    pub num_definitions: usize,

    pub free_symbols: Vec<Symbol>,

    types: Vec<(String, ClassDeclAst)>,
    imports: Vec<(String, CompiledModuleObject)>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,

            outer: None,
            free_symbols: Vec::new(),

            types: Vec::new(),
            imports: Vec::new(),
        }
    }

    pub fn new_enclosed(outer: Self) -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,

            outer: Some(Box::new(outer)),
            free_symbols: Vec::new(),

            types: Vec::new(),
            imports: Vec::new(),
        }
    }

    pub fn define(&mut self, name: &str, mutable: bool) -> Symbol {
        let mut symbol = Symbol::new(name, SymbolScope::Global, self.num_definitions, mutable);
        if self.outer.is_some() {
            symbol.scope = SymbolScope::Local;
        }

        self.store.insert(name.to_string(), symbol.clone());

        self.num_definitions += 1;

        symbol
    }

    pub fn define_builtin(&mut self, name: &str, index: usize) -> Symbol {
        let symbol = Symbol::new(name, SymbolScope::Builtin, index, false);
        self.store.insert(name.to_string(), symbol.clone());

        symbol
    }

    pub fn define_function_name(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new(name, SymbolScope::Function, 0, false);
        self.store.insert(name.to_string(), symbol.clone());

        symbol
    }

    pub fn define_free(&mut self, original: Symbol) -> Symbol {
        let symbol = Symbol {
            name: original.name.clone(),
            index: self.free_symbols.len(),
            scope: SymbolScope::Free,
            mutable: original.mutable,
        };
        self.store.insert(original.name.clone(), symbol.clone());

        self.free_symbols.push(original);

        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.store.get(name) {
            Some(symbol.clone())
        } else if let Some(outer) = self.outer.as_mut() {
            if let Some(obj) = outer.resolve(name) {
                if obj.scope == SymbolScope::Global || obj.scope == SymbolScope::Builtin {
                    return Some(obj);
                }

                let free = self.define_free(obj);
                Some(free)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn delete(&mut self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.store.remove(name) {
            Some(symbol)
        } else if let Some(outer) = self.outer.as_mut() {
            outer.delete(name)
        } else {
            None
        }
    }

    pub fn define_type(&mut self, name: String, decl: ClassDeclAst) -> usize {
        let pos = self.types.len();
        self.types.push((name, decl));

        pos
    }

    pub fn define_import(&mut self, name: String, module: CompiledModuleObject) -> usize {
        let pos = self.imports.len();
        self.imports.push((name, module));

        pos
    }

    pub fn resolve_type(&self, name: &str) -> Option<(ClassDeclAst, usize)> {
        for (pos, (class_name, node)) in self.types.iter().enumerate() {
            if class_name == name {
                return Some((node.clone(), pos));
            }
        }

        None
    }

    pub fn resolve_import(&self, name: &str) -> Option<(CompiledModuleObject, usize)> {
        for (pos, (import, node)) in self.imports.iter().enumerate() {
            if import == name {
                return Some((node.clone(), pos));
            }
        }

        None
    }
}
