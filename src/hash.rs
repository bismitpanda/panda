use std::hash::{Hash, Hasher};

use ahash::AHasher;

pub const METHOD_NAMES: [&str; 42] = [
    "abs",
    "bits",
    "chars",
    "contains",
    "first",
    "insert",
    "isAlphabetic",
    "isAlphanumeric",
    "isAscii",
    "isAsciiAlphabetic",
    "isAsciiAlphanumeric",
    "isAsciiControl",
    "isAsciiDigit",
    "isAsciiGraphic",
    "isAsciiLowercase",
    "isAsciiPunctuation",
    "isAsciiUppercase",
    "isAsciiWhitespace",
    "isBinDigit",
    "isControl",
    "isDecDigit",
    "isDigit",
    "isHexDigit",
    "isInf",
    "isLowercase",
    "isNaN",
    "isNegInf",
    "isNumeric",
    "isOctDigit",
    "isUppercase",
    "isWhitespace",
    "join",
    "keys",
    "last",
    "len",
    "push",
    "rest",
    "split",
    "toAsciiLowercase",
    "toAsciiUppercase",
    "trimWhitespace",
    "values",
];

pub fn hash_method_name(method_name: &str) -> usize {
    METHOD_NAMES
        .binary_search(&method_name)
        .unwrap_or_else(|_| {
            let mut hasher = AHasher::default();

            method_name.hash(&mut hasher);
            hasher.finish() as usize
        })
}
