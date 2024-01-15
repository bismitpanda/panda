pub mod builtins;

use std::{
    fmt::Display,
    hash::{Hash as StdHash, Hasher},
};

use ahash::AHasher;
use hashbrown::HashMap;

use super::{Environment, Write};
use crate::{ast::BlockStatement, code::Instructions, compiler::symbol_table::SymbolTable};

pub type BuiltinFunction = fn(&Object, &[Object]) -> Object;

pub const TRUE: Object = Object::Bool(Bool { value: true });
pub const FALSE: Object = Object::Bool(Bool { value: false });
pub const NULL_OBJ: Object = Object::Null {};

pub const DIR_ENV_VAR_NAME: &str = "STARTING_POINT_DIR";

#[derive(Clone, PartialEq, Debug)]
pub struct HashPair {
    pub key: Hashable,
    pub value: Object,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Int {
    pub value: isize,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Float {
    pub value: f64,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Char {
    pub value: char,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Bool {
    pub value: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Str {
    pub value: String,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    pub message: String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvaluatedFunction {
    pub parameters: Vec<String>,
    pub body: BlockStatement,
    pub environment: Environment,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Builtin {
    pub name: String,
    pub func: BuiltinFunction,
    pub caller: Option<Box<Object>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Array {
    pub elements: Vec<Object>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Dict {
    pub pairs: HashMap<u64, HashPair>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassMember {
    pub name: String,
    pub mutable: bool,
    pub obj: Object,
}

impl ClassMember {
    pub fn new(name: String, obj: Object, mutable: bool) -> Self {
        Self { name, mutable, obj }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Class {
    pub name: String,
    pub members: HashMap<u8, ClassMember>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Type {
    pub id: usize,
    pub lit: String,
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Range {
    pub start: isize,
    pub end: isize,
    pub step: isize,
}

impl Range {
    pub fn len(&self) -> usize {
        usize::try_from((self.end - self.start) / self.step).unwrap()
            + usize::from((self.end - self.start) % self.step != 0)
    }

    pub fn nth(&self, idx: usize) -> isize {
        self.start + (self.step * isize::try_from(idx).unwrap())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvaluatedModule {
    pub name: String,
    pub environment: Environment,
}

#[derive(Clone, PartialEq, Debug)]
pub struct CompiledModule {
    pub name: String,
    pub symbol_table: SymbolTable,
    pub constants: Vec<Object>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
    pub func: CompiledFunction,
    pub free: Vec<Object>,
}

#[derive(Clone, PartialEq, Eq, Debug, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum ControlFlow {
    Break,
    Continue,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Iter {
    pub expr: Iterable,
    pub current: usize,
    pub size: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Int(Int),
    Float(Float),
    Bool(Bool),
    Str(Str),
    Char(Char),
    Null,
    ReturnValue(ReturnValue),
    Error(Error),
    EvaluatedFunction(EvaluatedFunction),
    Builtin(Builtin),
    Array(Array),
    Dict(Dict),
    Class(Class),
    Type(Type),
    Range(Range),
    CompiledFunction(CompiledFunction),
    Closure(Closure),
    ControlFlow(ControlFlow),
    Iter(Iter),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array(Array { elements }) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Bool(Bool { value }) => write!(f, "{value}"),

            Self::Builtin(Builtin { name, .. }) => write!(f, "built-in function \"{name}\""),

            Self::Char(Char { value }) => write!(f, "{value}"),

            Self::Class(Class { name, .. }) => write!(f, "<class \"{name}\">"),

            Self::Error(Error { message }) => write!(f, "{message}"),

            Self::Float(Float { value }) => write!(f, "{value}"),

            Self::EvaluatedFunction(EvaluatedFunction { parameters, .. }) => {
                write!(f, "function ({} parameters)", parameters.len())
            }

            Self::Dict(Dict { pairs }) => write!(
                f,
                "{{{}}}",
                pairs
                    .values()
                    .map(|v| format!("{}: {}", v.key, v.value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Int(Int { value }) => write!(f, "{value}"),

            Self::Null => write!(f, "null"),

            Self::Range(Range { start, end, step }) => {
                write!(f, "{start}..{end}..{step}")
            }

            Self::ReturnValue(ReturnValue { value }) => write!(f, "{value}"),

            Self::Str(Str { value }) => write!(f, "{value}"),

            Self::Type(Type { id, lit }) => write!(f, "<type \"{lit}\"(0x{id:x})"),

            Self::CompiledFunction(_) => write!(f, "<compiled-fn>"),

            Self::Closure(_) => write!(f, "<closure>"),

            Self::ControlFlow(t) => write!(f, "{t}"),

            Self::Iter(Iter {
                expr: iter,
                current,
                size,
            }) => {
                write!(f, "<iter ({current}, {size}){}>", iter.to_object())
            }
        }
    }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Int(_)
            | Self::Float(_)
            | Self::Bool(_)
            | Self::Null
            | Self::ReturnValue(_)
            | Self::Builtin(_)
            | Self::Range(_)
            | Self::Class(_)
            | Self::EvaluatedFunction(_)
            | Self::Type(_)
            | Self::CompiledFunction(_)
            | Self::Closure(_)
            | Self::ControlFlow(_)
            | Self::Iter(_) => self.to_string(),

            Self::Char(Char { value }) => format!("'{value}'"),

            Self::Str(Str { value }) => format!("\"{value}\""),

            Self::Error(Error { message }) => format!("ERROR: {message}"),

            Self::Array(Array { elements }) => format!(
                "({} elements)[{}]",
                elements.len(),
                elements
                    .iter()
                    .map(Self::inspect)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Dict(Dict { pairs }) => format!(
                "({} pairs){{{}}}",
                pairs.len(),
                pairs
                    .values()
                    .map(|v| format!("{}: {}", v.key.to_object().inspect(), v.value.inspect()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }

    pub fn kind(&self) -> String {
        let out = match self {
            Self::Int(_) => "INT",
            Self::Float(_) => "FLOAT",
            Self::Bool(_) => "BOOL",
            Self::Null => "NULL",
            Self::ReturnValue(_) => "RETURN_VALUE",
            Self::Builtin(_) => "BUILTIN",
            Self::Range(_) => "RANGE",
            Self::Class(_) => "CLASS",
            Self::EvaluatedFunction(_) => "FUNCTION",
            Self::Type(_) => "TYPE",
            Self::Char(_) => "CHAR",
            Self::Str(_) => "STR",
            Self::Error(_) => "ERROR",
            Self::Array(_) => "ARRAY",
            Self::Dict(_) => "HASH",
            Self::CompiledFunction(_) => "COMPILED_FUNCTION",
            Self::Closure(_) => "CLOSURE",
            Self::ControlFlow(_) => "CONTROL_FLOW",
            Self::Iter(_) => "ITER",
        };

        out.to_string()
    }

    pub fn get_id(&self) -> usize {
        match self {
            Self::Int(_) => 0,
            Self::Float(_) => 1,
            Self::Str(_) => 2,
            Self::Char(_) => 3,
            Self::Array(_) => 4,
            Self::Dict(_) => 5,
            _ => usize::MAX,
        }
    }

    pub fn call_method(&self, method: u8, params: Option<&[Self]>) -> Self {
        match self {
            Self::Class(Class { name, members }) => members.get(&method).map_or_else(
                || {
                    new_error(format!(
                        "no method named \"{method}\" found for class \"{name}\"",
                    ))
                },
                |class_member| class_member.obj.clone(),
            ),

            Self::Int(_)
            | Self::Float(_)
            | Self::Str(_)
            | Self::Char(_)
            | Self::Array(_)
            | Self::Dict(_) => {
                let (_, func) = builtins::BUILTIN_METHODS[self.get_id()]
                    .iter()
                    .find(|(name, _)| hash_method_name(name) == method)
                    .unwrap();

                params.map_or_else(
                    || {
                        Self::Builtin(Builtin {
                            name: format!("{}.{}", self.kind(), method),
                            func: *func,
                            caller: Some(Box::new(self.clone())),
                        })
                    },
                    |params| func(self, params),
                )
            }

            _ => new_error(format!("{} cannot have user-defined method", self.kind())),
        }
    }
}

fn intersperse<T: Copy, U: Iterator<Item = T>>(iter: U, sep: T) -> Vec<T> {
    let mut peekable = iter.peekable();
    let mut out = Vec::new();

    while peekable.peek().is_some() {
        out.push(peekable.next().unwrap());
        out.push(sep);
    }

    out
}

fn new_error(message: String) -> Object {
    Object::Error(Error { message })
}

pub fn allowed_in_array(obj: &Object) -> bool {
    matches!(
        obj,
        Object::Int(_)
            | Object::Float(_)
            | Object::Bool(_)
            | Object::Null
            | Object::Str(_)
            | Object::Char(_)
            | Object::Array(_)
            | Object::Dict(_)
    )
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Hashable {
    Char(Char),
    Int(Int),
    Bool(Bool),
    Str(Str),
}

impl Hashable {
    pub fn hash_key(&self) -> u64 {
        let mut hasher = AHasher::default();
        match self {
            Self::Char(Char { value }) => {
                value.hash(&mut hasher);
                hasher.finish()
            }
            Self::Int(Int { value }) => {
                value.hash(&mut hasher);
                hasher.finish()
            }
            Self::Bool(Bool { value }) => {
                value.hash(&mut hasher);
                hasher.finish()
            }
            Self::Str(Str { value }) => {
                value.hash(&mut hasher);
                hasher.finish()
            }
        }
    }

    pub fn to_object(&self) -> Object {
        match self {
            Self::Char(node) => Object::Char(node.clone()),
            Self::Int(node) => Object::Int(node.clone()),
            Self::Bool(node) => Object::Bool(node.clone()),
            Self::Str(node) => Object::Str(node.clone()),
        }
    }

    pub fn from_object(obj: &Object) -> Option<Self> {
        match obj {
            Object::Char(node) => Some(Self::Char(node.clone())),
            Object::Int(node) => Some(Self::Int(node.clone())),
            Object::Bool(node) => Some(Self::Bool(node.clone())),
            Object::Str(node) => Some(Self::Str(node.clone())),
            _ => None,
        }
    }
}

impl Display for Hashable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(Bool { value }) => write!(f, "{value}"),
            Self::Char(Char { value }) => write!(f, "{value}"),
            Self::Int(Int { value }) => write!(f, "{value}"),
            Self::Str(Str { value }) => write!(f, "{value}"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Iterable {
    Range(Range),
    Array(Array),
    Hash(Dict),
    Str(Str),
}

impl Iterable {
    pub fn from_object(obj: Object) -> Option<Self> {
        match obj {
            Object::Range(ast_node) => Some(Self::Range(ast_node)),
            Object::Array(ast_node) => Some(Self::Array(ast_node)),
            Object::Dict(ast_node) => Some(Self::Hash(ast_node)),
            Object::Str(ast_node) => Some(Self::Str(ast_node)),
            _ => None,
        }
    }

    pub fn to_object(&self) -> Object {
        match self.clone() {
            Self::Range(ast_node) => Object::Range(ast_node),
            Self::Array(ast_node) => Object::Array(ast_node),
            Self::Hash(ast_node) => Object::Dict(ast_node),
            Self::Str(ast_node) => Object::Str(ast_node),
        }
    }

    pub fn count(&self) -> usize {
        match self {
            Self::Range(ast_node) => ast_node.len(),
            Self::Array(ast_node) => ast_node.elements.len(),
            Self::Hash(ast_node) => ast_node.pairs.len(),
            Self::Str(ast_node) => ast_node.value.len(),
        }
    }

    pub fn get(&self, idx: usize) -> Object {
        match self {
            Self::Range(ast_node) => Object::Int(Int {
                value: ast_node.nth(idx),
            }),
            Self::Array(ast_node) => ast_node.elements[idx].clone(),
            Self::Hash(ast_node) => ast_node
                .pairs
                .values()
                .map(|value| value.key.clone())
                .nth(idx)
                .map(|some| some.to_object())
                .unwrap(),
            Self::Str(ast_node) => ast_node
                .value
                .chars()
                .nth(idx)
                .map(|value| Object::Char(Char { value }))
                .unwrap(),
        }
    }
}

pub fn hash_method_name(name: &str) -> u8 {
    let mut hasher = AHasher::default();
    name.hash(&mut hasher);
    let hash = hasher.finish();

    let mut out = 0u8;
    for i in 0..8 {
        out ^= (hash >> (i * 8)) as u8;
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = Hashable::Str(Str {
            value: "Hello World!".to_string(),
        });
        let hello2 = Hashable::Str(Str {
            value: "Hello World!".to_string(),
        });

        let diff1 = Hashable::Str(Str {
            value: "My name is johnny.".to_string(),
        });
        let diff2 = Hashable::Str(Str {
            value: "My name is johnny.".to_string(),
        });

        assert_eq!(
            hello1.hash_key(),
            hello2.hash_key(),
            "strings with same content have different hash keys"
        );
        assert_eq!(
            diff1.hash_key(),
            diff2.hash_key(),
            "strings with same content have different hash keys"
        );
        assert_ne!(
            hello1.hash_key(),
            diff1.hash_key(),
            "strings with different content have same hash keys"
        );
    }
}
