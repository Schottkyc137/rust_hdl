use std::{
    fs::{File, OpenOptions},
    io::{self, Read, stdout},
    path::{Path, PathBuf},
    process::exit,
};

use clap::Parser;
use vhdl_fmt::format;
use vhdl_syntax::{
    parser,
    syntax::{AstNode, DesignFileSyntax, node::SyntaxNode},
};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to the file
    file: PathBuf,

    /// Print to stdout instead of replacing the file.
    #[arg(short, long, default_value = "false")]
    print_to_stdout: bool,
}

struct FromFileError {
    message: String,
    err_code: i32,
}

fn parse_from_file(path: &Path) -> Result<DesignFileSyntax, FromFileError> {
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(e) => {
            return Err(FromFileError {
                message: format!("Cannot read file {}: {}", path.display(), e),
                err_code: e.raw_os_error().unwrap_or(1),
            });
        }
    };
    let mut buf = Vec::new();
    match file.read_to_end(&mut buf) {
        Ok(_) => {}
        Err(e) => {
            return Err(FromFileError {
                message: format!("Cannot read file {}: {}", path.display(), e),
                err_code: e.raw_os_error().unwrap_or(1),
            });
        }
    }
    // TODO: Do not ignore errors
    let (node, _) = parser::parse(buf);
    Ok(node)
}

fn write_to_file(path: &Path, node: &SyntaxNode) -> io::Result<()> {
    let mut file = OpenOptions::new().write(true).truncate(true).open(path)?;
    node.write_to(&mut file)
}

fn main() {
    let args = Args::parse();
    let node = match parse_from_file(&args.file) {
        Ok(design) => design,
        Err(e) => {
            println!("{}", e.message);
            exit(e.err_code);
        }
    };
    let formatted_node = format(node.raw());
    let result = if args.print_to_stdout {
        formatted_node.write_to(&mut stdout())
    } else {
        write_to_file(&args.file, &formatted_node)
    };
    match result {
        Ok(_) => {}
        Err(e) => {
            println!("Cannot write formatted document back to file: {}", e);
            exit(e.raw_os_error().unwrap_or(1));
        }
    }
}
