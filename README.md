## Description

A purely functional regular expressions library, lexer and SLR(1) parser, written in OCaml.

## Running
Edit the file in resources/test.kl according to the grammar. The following command will then create a parse tree (if the input can be parsed).
```
dune exec compiler
```

## Testing
Run the following command to run the supplied tests:
```
dune runtest
```