use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(
    Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum, strum::EnumString, strum::Display,
)]
#[strum(ascii_case_insensitive, serialize_all = "lowercase")]
pub enum Engine {
    Eval,
    VM,
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Run the provided file
    Run(RunArgs),

    /// Start the panda REPL
    Repl(ReplArgs),

    /// Formats the provided panda source file
    Format(FormatArgs),

    /// Outputs debug outputs for testing
    Debug(DebugArgs),
}

#[derive(Args)]
pub struct RunArgs {
    /// The input file path
    pub file_name: String,

    /// The engine to use as the interpreter
    #[arg(long, short, default_value_t = Engine::VM)]
    pub engine: Engine,
}

#[derive(Args)]
pub struct ReplArgs {
    /// The engine to use as the interpreter
    #[arg(long, short, default_value_t = Engine::Eval)]
    pub engine: Engine,
}

#[derive(Args)]
pub struct FormatArgs {
    /// The input file path
    pub file_name: String,

    /// The output file path
    #[arg(long, short)]
    pub out: Option<String>,
}

#[derive(
    Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum, strum::EnumString, strum::Display,
)]
#[strum(ascii_case_insensitive, serialize_all = "lowercase")]
pub enum DebugOut {
    Ast,
    ByteCode,
    Stack,
}

#[derive(Args)]
pub struct DebugArgs {
    /// The debug data to output
    #[arg(long, short, default_value_t = DebugOut::Ast)]
    pub format: DebugOut,

    /// Output file path
    #[arg(long, short)]
    pub out_file: Option<PathBuf>,

    /// The input file path
    pub file: Option<String>,
}
