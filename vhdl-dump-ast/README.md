# vhdl-dump-ast

A utility to dump VHDL ASTs in various output formats (JSON, YAML).

## Status

> [!WARNING]
> **Early Stage**: This crate is in a very early stage of development and is not intended for production use.
> The output format and command-line interface may change at any time without further notice.

## Usage

```bash
vhdl-dump-ast <FILE> [OPTIONS]
```

### Options

- `-f, --format <FORMAT>` — Output format: `json` (default) or `yaml`
- `-n, --no-pretty` — Disable pretty printing
- `-t, --trivia` — Include trivia (whitespace, comments) in the dump
- `-c, --comment-encoding <ENCODING>` — Define how comments are encoded, see [Comments](#comments). Default is `utf-8`.

### Example

```bash
vhdl-dump-ast design.vhd --format json --trivia
```

### Exit codes

- `0` — the AST was dumped successfully
- `1` — the input file could not be read
- `2` — the input contained syntax errors
- `3` — the AST could not be serialized into the requested output format

Syntax errors are reported on stderr as `<start>..<end> <message>` where the
span is a byte range (not a `line:col` position).

## Comments

Comments (line comments starting with `--` or block comments enclosed in `/*...*/`) can have arbitrary encoding in VHDL.
The serialized AST, therefore, includes those comments as byte-array with an additional `encoding` field attached that informs consumers on how to interpret the data.

## Encoding

Serialized data is UTF-8. All textual data originates from [ISO-8859-1](https://de.wikipedia.org/wiki/ISO_8859-1) (Latin-1) and is decoded losslessly. For byte-to byte compatibility, the text must be re-encoded into Latin-1 to be a valid VHDL file.

## Contributing

Found an issue or have a feature request? Please open an issue on the [GitHub repository](https://github.com/VHDL-LS/rust_hdl/issues).

Want to discuss the serialization format or request additional output formats? Start a [discussion](https://github.com/VHDL-LS/rust_hdl/discussions).

## License

See the root repository for license information.
