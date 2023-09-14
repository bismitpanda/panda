use hashbrown::HashMap;

use pretty_assertions::assert_eq;

use super::symbol_table::*;

#[test]
fn test_define_global() {
    let expected: HashMap<String, Symbol> = HashMap::from([
        (
            "a".to_string(),
            Symbol::new("a", SymbolScope::Global, 0, true),
        ),
        (
            "b".to_string(),
            Symbol::new("b", SymbolScope::Global, 1, false),
        ),
    ]);

    let mut global = SymbolTable::new();

    let a = global.define("a", true);
    assert_eq!(a, expected["a"]);

    let b = global.define("b", false);
    assert_eq!(b, expected["b"]);
}

#[test]
fn test_resolve_global() {
    let expected: HashMap<String, Symbol> = HashMap::from([
        (
            "a".to_string(),
            Symbol::new("a", SymbolScope::Global, 0, true),
        ),
        (
            "b".to_string(),
            Symbol::new("b", SymbolScope::Global, 1, false),
        ),
    ]);

    let mut global = SymbolTable::new();

    global.define("a", true);
    global.define("b", false);

    for sym in expected.values() {
        assert_eq!(Some(sym.clone()), global.resolve(&sym.name))
    }
}

#[test]
fn test_resolve_local() {
    let expected = Vec::from([
        Symbol::new("a", SymbolScope::Global, 0, false),
        Symbol::new("b", SymbolScope::Global, 1, true),
        Symbol::new("c", SymbolScope::Local, 0, true),
        Symbol::new("d", SymbolScope::Local, 1, false),
    ]);

    let mut global = SymbolTable::new();
    global.define("a", false);
    global.define("b", true);

    let mut local = SymbolTable::new_enclosed(global);
    local.define("c", true);
    local.define("d", false);

    for symbol in expected {
        let result = local.resolve(&symbol.name);

        assert_eq!(result, Some(symbol))
    }
}

struct NestedLocalTestCase {
    table: SymbolTable,
    expected_symbols: Vec<Symbol>,
}

#[test]
fn test_resolve_nested_local() {
    let mut global = SymbolTable::new();
    global.define("a", true);
    global.define("b", true);

    let mut first_local = SymbolTable::new_enclosed(global);
    first_local.define("c", true);
    first_local.define("d", true);

    let mut second_local = SymbolTable::new_enclosed(first_local.clone());
    second_local.define("e", true);
    second_local.define("f", true);

    let test_cases = [
        NestedLocalTestCase {
            table: first_local,
            expected_symbols: Vec::from([
                Symbol::new("a", SymbolScope::Global, 0, true),
                Symbol::new("b", SymbolScope::Global, 1, true),
                Symbol::new("c", SymbolScope::Local, 0, true),
                Symbol::new("d", SymbolScope::Local, 1, true),
            ]),
        },
        NestedLocalTestCase {
            table: second_local,
            expected_symbols: Vec::from([
                Symbol::new("a", SymbolScope::Global, 0, true),
                Symbol::new("b", SymbolScope::Global, 1, true),
                Symbol::new("e", SymbolScope::Local, 0, true),
                Symbol::new("f", SymbolScope::Local, 1, true),
            ]),
        },
    ];

    for mut test_case in test_cases {
        for symbol in test_case.expected_symbols {
            assert_eq!(test_case.table.resolve(&symbol.name), Some(symbol))
        }
    }
}

#[test]
fn test_define_resolve_builtins() {
    let mut global = SymbolTable::new();

    let expected = [
        Symbol::new("a", SymbolScope::Builtin, 0, false),
        Symbol::new("c", SymbolScope::Builtin, 1, false),
        Symbol::new("e", SymbolScope::Builtin, 2, false),
        Symbol::new("f", SymbolScope::Builtin, 3, false),
    ];

    for (i, v) in expected.iter().enumerate() {
        global.define_builtin(&v.name, i);
    }

    let mut first_local = SymbolTable::new_enclosed(global.clone());
    let mut second_local = SymbolTable::new_enclosed(first_local.clone());

    for symbol in &expected {
        assert_eq!(global.resolve(&symbol.name), Some(symbol.clone()));
    }

    for symbol in &expected {
        assert_eq!(first_local.resolve(&symbol.name), Some(symbol.clone()));
    }

    for symbol in &expected {
        assert_eq!(second_local.resolve(&symbol.name), Some(symbol.clone()));
    }
}
