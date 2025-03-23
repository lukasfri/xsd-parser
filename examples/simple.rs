#![allow(missing_docs)]

use std::fs::write;
use std::io::Write;
use std::process::{Stdio, Command};
use std::path::PathBuf;

use anyhow::Error;
use clap::Parser;
use tracing_subscriber::{fmt, EnvFilter};
use xsd_parser::{
    config::{GeneratorFlags, InterpreterFlags, OptimizerFlags, ParserFlags, Resolver, Schema},
    exec_generator, exec_interpreter, exec_optimizer, exec_parser,
    types::Type,
    Config,
};

mod ms {
    #![allow(
        dead_code,
        unused_mut,
        unreachable_pub,
        unused_variables,
        non_camel_case_types,
    )]

    // Execute `cargo run --example simple -- --enable-debug-output target/output.rs schema/Microsoft.Build.Core.xsd`
    // to generate the `../target/output.rs` and uncomment the line below to test.
    // include!("../target/output.rs");
}

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

    let config = Config::default();

    // HINT:
    // Default is `Debug + Clone`, but we use dynamic types in the schema, which
    // result in traits, so we can't use `Clone`.
    let mut config = config.with_derive(["Debug"]);

    config.parser.resolver = vec![Resolver::File];
    config.parser.flags = ParserFlags::all();
    config.parser.schemas = inputs.into_iter().map(Schema::File).collect();
    config.interpreter.flags = InterpreterFlags::all();
    config.optimizer.flags = OptimizerFlags::all()
        - OptimizerFlags::REMOVE_DUPLICATES
        // Hint::
        // The abstract `Property` element defined in `Microsoft.Build.Core.xsd`
        // does not define any derived element, so the `CONVERT_DYNAMIC_TO_CHOICE`
        // optimization would result in a empty enum ,which then causes compiler
        // errors in the generated `quick_xml` deserializer code. If you don't need
        // de/serialization support at all, you can re-enable this.
        - OptimizerFlags::CONVERT_DYNAMIC_TO_CHOICE;
    config.generator.flags = GeneratorFlags::all();

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
    let types = exec_interpreter(config.interpreter, &schemas)?;
    let mut types = exec_optimizer(config.optimizer, types)?;

    for ty in types.values_mut() {
        let Type::Enumeration(ei) = ty else {
            continue;
        };

        for var in &mut *ei.variants {
            if matches!(var.ident.name.as_str(), Some("*")) {
                var.display_name = Some("Wildcard".into());
            }
        }
    }

    let code = exec_generator(config.generator, &schemas, &types)?;
    let code = code.to_string();
    let code = fmt_code(&code);

    write(&args.output, code)?;

    Ok(())
}

fn fmt_code(s: &str) -> String {
    let mut child = Command::new("rustfmt")
        .arg("--emit")
        .arg("stdout")
        .arg("--edition")
        .arg("2021")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn rustfmt: {}", e))
        .expect("Unable to spawn rustfmt command");

    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(s.as_bytes())
        .expect("Unable to write data to stdin");
    let output = child
        .wait_with_output()
        .expect("Unable to get formatted output");

    if output.status.success() {
        String::from_utf8(output.stdout).expect("Invalid output")
    } else {
        s.into()
    }
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
