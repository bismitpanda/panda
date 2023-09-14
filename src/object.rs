pub mod builtins;

use std::{
    fmt::Display,
    hash::{Hash as StdHash, Hasher},
};

use ahash::AHasher;
use hashbrown::HashMap;
use num_bigint::BigInt;
use num_traits::{sign::Signed, ToPrimitive};

use super::{Environment, Write};
use crate::{ast::BlockStatement, code::Instructions, compiler::symbol_table::SymbolTable};

pub type BuiltinFunction = fn(&Object, &[Object]) -> Object;

pub const TRUE: Object = Object::Bool(BoolObject { value: true });
pub const FALSE: Object = Object::Bool(BoolObject { value: false });
pub const NULL: Object = Object::Null {};

pub const DIR_ENV_VAR_NAME: &str = "STARTING_POINT_DIR";

#[derive(Clone, PartialEq, Debug)]
pub struct HashPair {
    pub key: Hashable,
    pub value: Object,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IntObject {
    pub value: BigInt,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FloatObject {
    pub value: f64,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CharObject {
    pub value: char,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BoolObject {
    pub value: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StrObject {
    pub value: String,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ErrorObject {
    pub message: String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ReturnValueObject {
    pub value: Box<Object>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvaluatedFunctionObject {
    pub parameters: Vec<String>,
    pub body: BlockStatement,
    pub env: Environment,
}

#[derive(Clone, PartialEq, Debug)]
pub struct BuiltinObject {
    pub name: String,
    pub func: BuiltinFunction,
    pub caller: Option<Box<Object>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ArrayObject {
    pub elements: Vec<Object>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct HashObject {
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
pub struct ClassObject {
    pub name: String,
    pub members: HashMap<u8, ClassMember>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeObject {
    pub id: usize,
    pub lit: String,
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RangeObject {
    pub start: isize,
    pub stop: isize,
    pub step: isize,
}

impl RangeObject {
    pub fn len(&self) -> usize {
        ((self.stop - self.start) / self.step) as usize
            + usize::from((self.stop - self.start) % self.step != 0)
    }

    pub fn nth(&self, idx: usize) -> isize {
        self.start + (self.step * idx as isize)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvaluatedModuleObject {
    pub name: String,
    pub env: Environment,
}

#[derive(Clone, PartialEq, Debug)]
pub struct CompiledModuleObject {
    pub name: String,
    pub symbol_table: SymbolTable,
    pub constants: Vec<Object>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CompiledFunctionObject {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClosureObject {
    pub func: CompiledFunctionObject,
    pub free: Vec<Object>,
}

#[derive(Clone, PartialEq, Eq, Debug, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum ControlFlowObject {
    Break,
    Continue,
}

#[derive(Clone, PartialEq, Debug)]
pub struct IterObject {
    pub iter: Iterable,
    pub current: usize,
    pub size: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Int(IntObject),
    Float(FloatObject),
    Bool(BoolObject),
    Str(StrObject),
    Char(CharObject),
    Null,
    ReturnValue(ReturnValueObject),
    Error(ErrorObject),
    EvaluatedFunction(EvaluatedFunctionObject),
    Builtin(BuiltinObject),
    Array(ArrayObject),
    Hash(HashObject),
    Class(ClassObject),
    Type(TypeObject),
    Range(RangeObject),
    CompiledFunction(CompiledFunctionObject),
    Closure(ClosureObject),
    ControlFlow(ControlFlowObject),
    Iter(IterObject),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array(ArrayObject { elements }) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Bool(BoolObject { value }) => write!(f, "{value}"),

            Self::Builtin(BuiltinObject { name, .. }) => write!(f, "built-in function '{name}'"),

            Self::Char(CharObject { value }) => write!(f, "{value}"),

            Self::Class(ClassObject { name, .. }) => write!(f, "<class '{name}'>"),

            Self::Error(ErrorObject { message }) => write!(f, "{message}"),

            Self::Float(FloatObject { value }) => write!(f, "{value}"),

            Self::EvaluatedFunction(EvaluatedFunctionObject { parameters, .. }) => {
                write!(f, "function ({} parameters)", parameters.len())
            }

            Self::Hash(HashObject { pairs }) => write!(
                f,
                "{{{}}}",
                pairs
                    .values()
                    .map(|v| format!("{}: {}", v.key, v.value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Int(IntObject { value }) => write!(f, "{value}"),

            Self::Null => write!(f, "null"),

            Self::Range(RangeObject { start, stop, step }) => {
                write!(f, "{start}..{stop}..{step}")
            }

            Self::ReturnValue(ReturnValueObject { value }) => write!(f, "{value}"),

            Self::Str(StrObject { value }) => write!(f, "{value}"),

            Self::Type(TypeObject { id, lit }) => write!(f, "<type '{lit}'(0x{id:x})"),

            Self::CompiledFunction(_) => write!(f, "<compiled-fn>"),

            Self::Closure(_) => write!(f, "<closure>"),

            Self::ControlFlow(t) => write!(f, "{t}"),

            Self::Iter(IterObject {
                iter,
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

            Self::Char(CharObject { value }) => format!("'{value}'"),

            Self::Str(StrObject { value }) => format!("\"{value}\""),

            Self::Error(ErrorObject { message }) => format!("ERROR: {message}"),

            Self::Array(ArrayObject { elements }) => format!(
                "({} elements)[{}]",
                elements.len(),
                elements
                    .iter()
                    .map(Self::inspect)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),

            Self::Hash(HashObject { pairs }) => format!(
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
            Self::Hash(_) => "HASH",
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
            Self::Hash(_) => 5,
            _ => usize::MAX,
        }
    }

    pub fn call_method(&self, method: u8, params: Option<Vec<Self>>) -> Self {
        match self {
            Self::Class(ClassObject { name, members }) => {
                if let Some(class_member) = members.get(&method) {
                    class_member.obj.clone()
                } else {
                    new_error(format!(
                        "no method named '{method}' found for class '{name}'",
                    ))
                }
            }

            Self::Int(_)
            | Self::Float(_)
            | Self::Str(_)
            | Self::Char(_)
            | Self::Array(_)
            | Self::Hash(_) => {
                let (_, func) = builtins::BUILTIN_METHODS[self.get_id()]
                    .iter()
                    .find(|(name, _)| hash_method_name(name) == method)
                    .unwrap();

                params.map_or_else(
                    || {
                        Self::Builtin(BuiltinObject {
                            name: format!("{}.{}", self.kind(), method),
                            func: *func,
                            caller: Some(Box::new(self.clone())),
                        })
                    },
                    |params| func(self, &params),
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
    Object::Error(ErrorObject { message })
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
            | Object::Hash(_)
    )
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Hashable {
    Char(CharObject),
    Int(IntObject),
    Bool(BoolObject),
    Str(StrObject),
}

impl Hashable {
    pub fn hash_key(&self) -> u64 {
        let mut hasher = AHasher::default();
        match self {
            Self::Char(CharObject { value }) => {
                value.hash(&mut hasher);
                hasher.finish()
            }
            Self::Int(IntObject { value }) => {
                value.hash(&mut hasher);
                hasher.finish()
            }
            Self::Bool(BoolObject { value }) => {
                value.hash(&mut hasher);
                hasher.finish()
            }
            Self::Str(StrObject { value }) => {
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
            Self::Bool(BoolObject { value }) => write!(f, "{value}"),
            Self::Char(CharObject { value }) => write!(f, "{value}"),
            Self::Int(IntObject { value }) => write!(f, "{value}"),
            Self::Str(StrObject { value }) => write!(f, "{value}"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Iterable {
    Range(RangeObject),
    Array(ArrayObject),
    Hash(HashObject),
    Str(StrObject),
}

impl Iterable {
    pub fn from_object(obj: Object) -> Option<Self> {
        match obj {
            Object::Range(ast_node) => Some(Self::Range(ast_node)),
            Object::Array(ast_node) => Some(Self::Array(ast_node)),
            Object::Hash(ast_node) => Some(Self::Hash(ast_node)),
            Object::Str(ast_node) => Some(Self::Str(ast_node)),
            _ => None,
        }
    }

    pub fn to_object(&self) -> Object {
        match self.clone() {
            Self::Range(ast_node) => Object::Range(ast_node),
            Self::Array(ast_node) => Object::Array(ast_node),
            Self::Hash(ast_node) => Object::Hash(ast_node),
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
            Self::Range(ast_node) => Object::Int(IntObject {
                value: ast_node.nth(idx).into(),
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
                .map(|value| Object::Char(CharObject { value }))
                .unwrap(),
        }
    }
}

fn hash_method_name(name: &str) -> u8 {
    let mut hasher = AHasher::default();
    name.hash(&mut hasher);
    let hash = hasher.finish();

    let mut out = 0u8;
    for i in 0..8 {
        out ^= (hash >> (i * 8)) as u8
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = Hashable::Str(StrObject {
            value: "Hello World!".to_string(),
        });
        let hello2 = Hashable::Str(StrObject {
            value: "Hello World!".to_string(),
        });

        let diff1 = Hashable::Str(StrObject {
            value: "My name is johnny.".to_string(),
        });
        let diff2 = Hashable::Str(StrObject {
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
        )
    }
}
