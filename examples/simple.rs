#![allow(missing_docs)]

use std::fs::write;
use std::path::PathBuf;

use anyhow::Error;
use clap::Parser;
use tracing_subscriber::{fmt, EnvFilter};
use xsd_parser::{
    config::{
        ContentMode, Generate, GenerateFlags, InterpreterFlags, OptimizerFlags, ParserFlags,
        Resolver, Schema,
    },
    exec_generator, exec_interpreter, exec_optimizer, exec_parser,
    types::{IdentType, Type},
    Config,
};

fn main() -> Result<(), Error> {
    fmt()
        .without_time()
        .with_file(true)
        .with_level(true)
        .with_line_number(true)
        .with_thread_ids(true)
        .with_thread_names(true)
        .pretty()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    tracing::info!("Run with arguments: {args:#?}");

    let inputs = args
        .inputs
        .into_iter()
        .map(|p| p.canonicalize())
        .collect::<Result<Vec<_>, _>>()?;

    let mut config = Config::default();
    config.parser.resolver = vec![Resolver::File];
    config.parser.flags = ParserFlags::all();
    config.parser.schemas = inputs.into_iter().map(Schema::File).collect();
    config.interpreter.flags = InterpreterFlags::all();
    config.optimizer.flags = OptimizerFlags::all()
        - OptimizerFlags::REMOVE_DUPLICATES
        - OptimizerFlags::CONVERT_DYNAMIC_TO_CHOICE
        - OptimizerFlags::FLATTEN_ELEMENT_CONTENT;
    config.generator.flags =
        GenerateFlags::all() - GenerateFlags::WITH_NAMESPACE_TRAIT - GenerateFlags::QUICK_XML;
    config.generator.content_mode = ContentMode::Enum;

    if let Some(out_dir) = args
        .enable_debug_output
        .then_some(())
        .and_then(|()| args.output.parent())
    {
        config.parser.debug_output = Some(out_dir.join("parser.log"));
        config.interpreter.debug_output = Some(out_dir.join("interpreter.log"));
        config.optimizer.debug_output = Some(out_dir.join("optimizer.log"));
    }

    let schemas = exec_parser(config.parser)?;
    let mut types = exec_interpreter(config.interpreter, &schemas)?;

    for type_ in types.values_mut() {
        match type_ {
            Type::Enumeration(ti) => {
                for var in &mut *ti.variants {
                    match var.ident.name.as_str() {
                        Some("+") => var.display_name = Some("Plus".into()),
                        Some("-") => var.display_name = Some("Minus".into()),
                        Some(s) if s.starts_with(char::is_numeric) => {
                            var.display_name = Some(format!("_{s}"));
                        }
                        _ => (),
                    }
                }
            }
            Type::Union(ti) => {
                for var in &mut *ti.types {
                    match var.type_.name.as_str() {
                        Some("+") => var.display_name = Some("Plus".into()),
                        Some("-") => var.display_name = Some("Minus".into()),
                        Some(s) if s.starts_with(char::is_numeric) => {
                            var.display_name = Some(format!("_{s}"));
                        }
                        _ => (),
                    }
                }
            }
            _ => (),
        }
    }

    let types_to_generate = types
        .keys()
        .filter(|ident| ident.name.is_named() && ident.type_ == IdentType::Element)
        .map(|ident| {
            let ns = ident
                .ns
                .as_ref()
                .and_then(|ns| types.modules.get(ns))
                .and_then(|module| module.name.as_ref());
            let ident = if let Some(ns) = ns {
                format!("{ns}:{}", ident.name)
            } else {
                format!("{}", ident.name)
            };

            (IdentType::Element, ident)
        })
        .collect::<Vec<_>>();
    config.generator.generate = Generate::Types(types_to_generate);

    let types = exec_optimizer(config.optimizer, types)?;
    let code = exec_generator(config.generator, &schemas, &types)?;

    let code = code.to_string();

    write(&args.output, code)?;

    Ok(())
}

/// Simple command line tool to generate code out of any XML schema that is
/// passed as input argument.
#[derive(Debug, Parser)]
struct Args {
    /// Write additional debut output in the output directory.
    #[arg(short, long)]
    enable_debug_output: bool,

    /// Path to write the generated code to.
    #[arg()]
    output: PathBuf,

    /// Paths to read the schema files from.
    #[arg()]
    inputs: Vec<PathBuf>,
}
